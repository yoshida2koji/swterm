(defpackage :swterm
  (:use :cl)
  (:import-from
   :uiop
   :if-let)
  (:import-from
   :struct+
   :defstruct+)
  (:import-from
   :swterm.ansi-256-color
   :*colors*)
  (:local-nicknames (:x :xclhb)
                    (:shm :xclhb-shm))
  (:export
   :open-terminal))

(in-package :swterm)

(declaim (inline offset))
(defun offset (x y w)
  (* 4 (+ (* y w) x)))

(declaim (inline blue green red alpha))
(defun blue (c) (ldb (byte 8 0) c))
(defun green (c) (ldb (byte 8 8) c))
(defun red (c) (ldb (byte 8 16) c))
(defun alpha (c) (ldb (byte 8 24) c))


(declaim (inline set-color))
(defun set-color (data i c)
  (setf (cffi:mem-ref data :uint8 i) (blue c)
        (cffi:mem-ref data :uint8 (+ i 1)) (green c)
        (cffi:mem-ref data :uint8 (+ i 2)) (red c)
        (cffi:mem-ref data :uint8 (+ i 3)) (alpha c)))

(defstruct+ glyph-cache ()
  (data nil :type (simple-array (unsigned-byte 8) (* *)))
  (use-count 1 :type (integer 0))
  (last-use-time (get-internal-real-time) :type (integer 0)))

(deftype character-buffer () '(simple-array t (* *)))

(defun make-character-buffer (w h)
  (make-array (list h w) :element-type t :initial-element nil))

(defparameter *default-foreground-index* 15)

(defparameter *default-background-index* 0)

(defstruct+ terminal ()
  (client nil :type x:client)
  (window 0 :type x:card32)
  (pixmap 0 :type x:card32)
  (gc 0 :type x:card32)
  (shmseg 0 :type x:card32)
  (colormap 0 :type x:card32)
  (segment nil :type (or null shm:shm-segment))
  (width 0 :type (integer 0))
  (height 0 :type (integer 0))
  (rows 24 :type (integer 0))
  (cols 80 :type (integer 0))
  (font nil :type string)
  (font-loader nil :type (or null zpb-ttf::font-loader))
  (font-size 0 :type (integer 0))
  (font-size-px 0 :type (integer 0))
  (background #xff000000 :type (unsigned-byte 32))
  (transparency 0.8 :type (real 0.0 1.0))
  (foreground #xffffffff :type (unsigned-byte 32))
  (characters nil :type (or null character-buffer))
  (caret-x 0 :type (integer 0))
  (caret-y 0 :type (integer 0))
  (pixel-x 0 :type (integer 0))
  (pixel-y 0 :type (integer 0))
  (pre-pixel-x 0 :type (integer 0))
  (pre-pixel-y 0 :type (integer 0))
  (glyph-scale 0 :type real)
  (glyph-baseline 0 :type real)
  (glyph-height 0 :type (integer 0))
  (glyph-cache-table (make-hash-table :size 1024) :type hash-table)
  (glyph-cache-limit 1048576 :type (integer 0))
  (glyph-cache-size 0 :type (integer 0))
  (half-char-width 0 :type (integer 0))
  (pty)
  (caret-backup-buffer nil :type (or null (simple-array (unsigned-byte 32) (* *)))))


(defun set-alpha-function (buffer)
  (let ((right (1- (array-dimension buffer 1)))
        (bottom (1- (array-dimension buffer 0))))
    (declare (type fixnum right bottom))
    (lambda (x y a)
      (when (and (<= 0 x right)
                 (<= 0 y bottom))
        (setf (aref buffer y x) (min 255 (abs a)))))))

(defun set-font-size (terminal size)
  (with-terminal (client font-size font-size-px font-loader
                         glyph-scale glyph-baseline glyph-height)
      terminal
    (let ((screen (elt (x:setup-roots (x:client-server-information client)) 0)))
      (x:with-screen (width-in-pixels width-in-millimeters) screen
        (setf font-size size
              font-size-px (ceiling (/ (* size width-in-pixels 254)
                                       (* width-in-millimeters 720)))
              glyph-scale (/ font-size-px (zpb-ttf:units/em font-loader))
              glyph-baseline (* glyph-scale (zpb-ttf:ascender font-loader))
              glyph-height (ceiling (- glyph-baseline
                                         (* glyph-scale (zpb-ttf:descender font-loader))))))))
  (values))

(defun make-glyph-data (terminal ch)
  (let-terminal (font-loader glyph-scale glyph-height glyph-baseline) terminal
    (let* ((glyph (zpb-ttf:find-glyph ch font-loader))
           (width (ceiling (* glyph-scale (zpb-ttf:advance-width glyph))))
           (glyph-data (make-array (list glyph-height width) :element-type '(unsigned-byte 8) :initial-element 0))
           (paths (paths-ttf:paths-from-glyph glyph :offset (paths:make-point 0 glyph-baseline)
                                                    :scale-x glyph-scale
                                                    :scale-y (- glyph-scale)))
           (state (aa:make-state)))
      (vectors:update-state state paths)
      (aa:cells-sweep state (set-alpha-function glyph-data))
      glyph-data)))

(defun clear-glyph-cache (terminal)
  (let ((table (terminal-glyph-cache-table terminal)))
    (maphash (lambda (k v)
               (declare (ignore v))
               (remhash k table))
             table)))


(defun get-glyph (terminal ch)
  (with-terminal (glyph-cache-table
                  (limit glyph-cache-limit)
                  (size glyph-cache-size))
      terminal
    (if-let (cache (gethash ch glyph-cache-table))
      (glyph-cache-data cache)
      (let* ((data (make-glyph-data terminal ch))
             (data-size (array-total-size data))
             (new-size (+ size data-size)))
        (cond ((> new-size limit)
               (clear-glyph-cache terminal)
               (setf size data-size))
              (t
               (setf size
                     new-size
                     (gethash ch glyph-cache-table)
                     (make-glyph-cache :data data))))
        data))))


(defun copy-row (terminal src-row dest-row)
  (let-terminal
      (characters segment width cols glyph-height)
      terminal
    ;; copy characters
    (dotimes (j cols)
      (setf (aref characters dest-row j) (aref characters src-row j)))
    ;; copy pixels
    (let ((data (shm:shm-segment-data segment)))
      (loop :repeat (* glyph-height width 4)
            :for src-i :from (* 4 width src-row glyph-height)
            :for dest-i :from (* 4 width dest-row glyph-height)
            :do (setf (cffi:mem-aref data :uint8 dest-i)
                      (cffi:mem-aref data :uint8 src-i))))))

(defun scroll (terminal ps)
  (let-terminal (width height rows glyph-height background) terminal
    (cond ((plusp ps)
           (loop :for i :from 0 :below (- rows ps)
                 :do (copy-row terminal (+ i ps) i))
           (fill-rect terminal
                      0 (* glyph-height (- rows ps))
                      width height background))
          ((minusp ps)
           (loop :for i :from (- rows 1) :downto (- ps)
                 :do (copy-row terminal (+ i ps) i))
           (fill-rect terminal
                      0 0 
                      width (* glyph-height (- ps)) background)))))

(defun composite-color-1 (b f a)
  (floor (+ (* f a)
            (* b (- #xff a)))
         #xff))

(defun make-color (a r g b)
  (dpb a (byte 8 24)
       (dpb r (byte 8 16)
            (dpb g (byte 8 8) b))))

(defun composite-color (b f alpha)
  (make-color (alpha f)
              (composite-color-1 (red b) (red f)  alpha)
              (composite-color-1 (green b) (green f) alpha)
              (composite-color-1 (blue b) (blue f) alpha)))


(defun draw-glyph-at (terminal ch x y)
  (let-terminal (segment width foreground background) terminal
    (let* ((data (shm:shm-segment-data segment))
           (glyph (get-glyph terminal ch))
           (glyph-width (array-dimension glyph 1))
           (glyph-height (array-dimension glyph 0)))
      (dotimes (gy glyph-height)
        (loop :for gx :from 0 :below glyph-width
              :for offset :from (offset (+ x gx) (+ y gy) width) :by 4
              :for alpha := (aref glyph gy gx)
              :do (set-color data offset
                             (cond ((= alpha 0) background)
                                   ((= alpha #xff) foreground)
                                   (t (composite-color background foreground alpha))))))))
  (values))


(defun draw-glyph (terminal ch)
  (with-terminal (width pixel-x pixel-y caret-x caret-y
                        characters rows glyph-height half-char-width)
      terminal
    (let* ((glyph (get-glyph terminal ch))
           (glyph-width (array-dimension glyph 1)))
      (when (> (+ pixel-x glyph-width) width)
        (setf pixel-x 0
              caret-x 0)
        (cond ((>= (+ caret-y 1) rows)
               (scroll terminal 1))
              (t
               (incf pixel-y glyph-height)
               (incf caret-y))))
      (setf (aref characters caret-y caret-x) ch)
      (draw-glyph-at terminal ch pixel-x pixel-y )
      (incf pixel-x glyph-width)
      (incf caret-x)
      ;; full width char
      (when (> glyph-width half-char-width)
        (setf (aref characters caret-y caret-x) :right-of-full-width-char)
        (incf caret-x))
      ))
  (values))

(defun up-line (terminal)
  (with-terminal (caret-x caret-y pixel-x pixel-y glyph-height) terminal
    (cond ((<= (- caret-y 1) 0)
           (scroll terminal -1)
           )
          (t
           (decf caret-y)
           (decf pixel-y glyph-height))))
  (values))

(defun down-line (terminal)
  (with-terminal (caret-x caret-y pixel-x pixel-y rows glyph-height) terminal
    (cond ((>= (+ caret-y 1) rows)
           (scroll terminal 1)
           )
          (t
           (incf caret-y)
           (incf pixel-y glyph-height))))
  (values))

(defun newline (terminal)
  (with-terminal (caret-x pixel-x) terminal
    (setf caret-x 0
          pixel-x 0)
    (down-line terminal))
  (values))

(defun clamp (n min max)
  (cond ((< n min) min)
        ((< max n) max)
        (t n)))

(defun save-previous-pixel (terminal)
  (with-terminal (pixel-x pixel-y pre-pixel-x pre-pixel-y) terminal
    (setf pre-pixel-x pixel-x
          pre-pixel-y pixel-y)))

(defun %move-caret-x (terminal x)
  (with-terminal (caret-x caret-y pixel-x cols half-char-width characters) terminal
    (setf x (clamp x 0 (1- cols)))
    (setf caret-x x
          pixel-x (* half-char-width caret-x))))

(defun %move-caret-y (terminal y)
  (with-terminal (caret-y pixel-y rows glyph-height) terminal
    (setf caret-y (clamp y 0 (1- rows))
          pixel-y (* glyph-height caret-y))))

(defun move-caret-x (terminal x)
  (%move-caret-x terminal (1- x)))

(defun move-caret-y (terminal y)
  (%move-caret-y terminal (1- y)))

(defun move-caret (terminal y x)
  (move-caret-y terminal y)
  (move-caret-x terminal x))

(defun move-caret-x-relative (terminal dx)
  (%move-caret-x terminal (+ (terminal-caret-x terminal) dx)))

(defun move-caret-y-relative (terminal dy)
  (%move-caret-y terminal (+ (terminal-caret-y terminal) dy)))

(defun fill-rect (terminal x y w h c)
  (let-terminal (width height segment) terminal
    (let ((data (shm:shm-segment-data segment))
          (start-y (clamp y 0 height))
          (end-y (clamp (+ y h) 0 height))
          (start-x (clamp x 0 width))
          (end-x (clamp (+ x w) 0 width)))
      (loop :for y :from start-y :below end-y
            :do (loop :for i :from (offset  start-x y width)
                        :below (offset end-x y width) :by 4
                      :do (set-color data i c)
                      )))))

(defun delete-char (terminal ps)
  (let-terminal (caret-x caret-y pixel-x pixel-y cols width background
                         glyph-height half-char-width characters segment)
      terminal
    (let ((n (- cols caret-x ps))
          (offset caret-x))
      (dotimes (i n)
        (setf (aref characters caret-y offset)
              (aref characters caret-y (+ offset ps)))
        (incf offset))
      (dotimes (i ps)
        (setf (aref characters caret-y offset) nil)
        (incf offset)))
    (let* ((data (shm:shm-segment-data segment))
           (diff-width (* half-char-width ps))
           (diff-offset (* 4 diff-width))
           (copy-width (- width pixel-x diff-width)))
      (loop :for y :from pixel-y :below (+ pixel-y glyph-height)
            :do (let ((offset (offset pixel-x y width)))
                  (dotimes (i (* 4 copy-width))
                    (setf (cffi:mem-ref data :uint8 offset)
                          (cffi:mem-ref data :uint8 (+ offset diff-offset)))
                    (incf offset))
                  (dotimes (i (* 4 diff-width))
                    (setf (cffi:mem-ref data :uint8 offset) background)
                    (incf offset)))))))

(defun clear-row (terminal ps)
  "ps 0 from cursor to end of row
ps 1 from start of row to cursor
ps 2 clear row entirely"
  (with-terminal (background pixel-x pixel-y glyph-height width) terminal
    (case ps
      (0 (fill-rect terminal pixel-x pixel-y width glyph-height background))
      (1 (fill-rect terminal 0 pixel-y pixel-x glyph-height background))
      (2 (fill-rect terminal 0 pixel-y width glyph-height background)))))

(defun clear-window (terminal ps)
  "ps 0 from cursor to end
ps 1 from start to cursor
ps 2 clear window entirely"
  (with-terminal (background pixel-y glyph-height width height) terminal
    (case ps
      (0 (clear-row terminal 0)
       (fill-rect terminal 0 (+ pixel-y glyph-height) width height background))
      (1 (fill-rect terminal 0 0 width pixel-y background)
       (clear-row terminal 1))
      (2 (fill-rect terminal 0 0 width height background)))))

(defun clear-terminal (terminal)
  (move-caret terminal 0 0)
  (let-terminal (background segment) terminal
    (let ((data (shm:shm-segment-data segment))
          (size (shm:shm-segment-size segment)))
      (loop :for i :from 0 :below size :by 4
            :do (set-color data i background)))))


(defun terminal-open-p (terminal)
  (and (terminal-p terminal)
       (x:client-open-p (terminal-client terminal))))


(defun find-visual-id (client depth)
  (if-let (depth (find depth (x:screen-allowed-depths
                              (elt (x:setup-roots (x:client-server-information client)) 0))
                       :key #'x:depth-depth))
    (if-let (visual-type (find x:+visual-class--true-color+
                               (x:depth-visuals depth)
                               :key
                               #'x:visualtype-class))
      (x:visualtype-visual-id visual-type))))

(defun read-char-safe (stream)
  (handler-bind ((error (lambda (c)
                          (declare (ignore c))
                          (return-from read-char-safe))))
    (read-char-no-hang stream nil nil nil)))

(defun parse-int-else (s &optional (else 0))
  (or (parse-integer s :junk-allowed t) else))

(defun parse-csi-args (chars)
  (if chars
      (map 'list #'parse-int-else
           (str:split #\; (coerce (nreverse chars) 'string)))
      nil))

(defun parse-csi-arg (chars &optional (else 1))
  (if chars
      (parse-int-else (coerce (nreverse chars) 'string) else)
      else))

(defun add-transparency (color transparency)
  (dpb (clamp (floor (* 255 transparency)) 0 255)
       (byte 8 24)
       color))

(defun set-background (terminal color)
  (with-terminal (background transparency) terminal
    (setf background
          (add-transparency color transparency))))

(defun set-background-from-index (terminal index)
  (set-background terminal (elt *colors* index)))

(defun set-background-from-rgb (terminal r g b)
  (set-background terminal (make-color 255 r g b)))

(defun set-foreground (terminal color)
  (setf (terminal-foreground terminal) (add-transparency color 1.0)))

(defun set-foreground-from-index (terminal index)
  (set-foreground terminal (elt *colors* index)))

(defun set-foreground-from-rgb (terminal r g b)
  (set-foreground terminal (make-color 255 r g b)))


(defun set-character-attribute (terminal args)
  (when args
    (labels ((f (i)
               (set-foreground-from-index terminal i))
             (b (i)
               (set-background-from-index terminal i))
             (sp (args rgb-fn index-fn)
               (case (second args)
                 (2 (when (>= (length args) 5)
                      (funcall rgb-fn terminal
                               (third args) (fourth args) (fifth args))
                      (set-character-attribute terminal (nthcdr 5 args))))
                 (5 (when (third args)
                      (funcall index-fn (third args))
                      (set-character-attribute terminal (nthcdr 3 args)))))))
      (let ((arg1 (first args)))
        (cond ((= arg1 38)
               (sp args #'set-foreground-from-rgb #'f))
              ((= arg1 48)
               (sp args #'set-background-from-rgb #'b))
              (t
               (cond ((= arg1 0)
                      (b *default-background-index*)
                      (f *default-foreground-index*))
                     ((<= 30 arg1 37)
                      (f (- arg1 30)))
                     ((= arg1 39)
                      (f *default-foreground-index*))
                     ((<= 90 arg1 97)
                      (f (- arg1 82)))
                     ((<= 40 arg1 47)
                      (b (- arg1 40)))
                     ((= arg1 49)
                      (b *default-background-index*))
                     ((<= 100 arg1 107)
                      (b (- arg1 92))))
               (set-character-attribute terminal (cdr args))))))))

(defun process-csi (terminal last-char chars)
  (flet ((arg (&optional (else 1))
           (parse-csi-arg chars else)))
    (case last-char
      ((#\A #\k) (move-caret-y-relative terminal (- (arg))))
      ((#\B #\e) (move-caret-y-relative terminal (arg)))
      ((#\C #\a) (move-caret-x-relative terminal (arg)))
      ((#\D #\j) (move-caret-x-relative terminal (- (arg))))
      (#\E (move-caret-y-relative terminal (arg))
       (move-caret-x terminal 1))
      (#\F (move-caret-y-relative terminal (- (arg)))
       (move-caret-x terminal 1))
      ((#\G #\`) (move-caret-x terminal (arg)))
      ((#\H #\f) (let ((args (parse-csi-args chars)))
               (move-caret terminal (or (first args) 1) (or (second args) 1))))
      ;;(#\I move after Ps tab stop)
      (#\J (clear-window terminal (arg 0)))
      (#\K (clear-row terminal (arg 0)))
      (#\P (delete-char terminal (arg 0)))
      (#\S (scroll terminal (arg)))
      (#\T (scroll terminal (- (arg))))
      (#\d (move-caret-y terminal (arg)))
      (#\m (set-character-attribute terminal (or (parse-csi-args chars) '(0))))
      )))

(defun process-esc (terminal last-char chars)
  (case last-char
    ;;(#\7 :save-cursor-position)
    ;;(#\8 :restore-cursor-position)
    (#\D (down-line terminal))
    (#\E (newline terminal))
    (#\M (up-line terminal))
    (#\P (read-dcs terminal nil))
    (#\X (read-sos terminal nil))
    ;;(#\Z :decid)
    (#\[ (read-csi terminal nil))
    (#\\ chars)
    (#\] (read-osc terminal nil))
    (#\^ (read-pm terminal nil))
    (#\_ (read-apc terminal nil))
    (#\c (clear-terminal terminal))))

(defun read-esc (terminal chars)
  (let ((ch (read-char-safe (terminal-pty terminal))))
    (cond ((null ch) nil)
          ((char= ch #\esc)
           (read-esc terminal nil))
          ((<= #x20 (char-code ch) #x2f)
           (read-esc terminal (cons ch chars)))
          ((<= #x30 (char-code ch) #x7e)
           (process-esc terminal ch chars)))))

(defun read-csi (terminal chars)
  (let ((ch (read-char-safe (terminal-pty terminal))))
    (cond ((null ch) nil)
          ((char= ch #\esc)
           (read-esc terminal nil))
          ((<= #x20 (char-code ch) #x3f)
           (read-csi terminal (cons ch chars)))
          ((<= #x40 (char-code ch) #x7f)
           (process-csi terminal ch chars)))))

(defun read-then-ignore (terminal chars)
  (let ((ch (read-char-safe (terminal-pty terminal))))
    (case ch
      (#\esc (let ((chars (read-esc terminal chars)))
               (when chars
                 ;;(print (nreverse chars))
                 )))
      (otherwise (read-then-ignore terminal (cons ch chars))))))

(defun read-dcs (terminal chars)
  (read-then-ignore terminal chars))

(defun read-sos (terminal chars)
  (read-then-ignore terminal chars))

(defun read-pm (terminal chars)
  (read-then-ignore terminal chars))

(defun read-apc (terminal chars)
  (read-then-ignore terminal chars))

(defun read-osc (terminal chars)
  (let ((ch (read-char-safe (terminal-pty terminal))))
    (case ch
      (#\esc (let ((osc-chars (read-esc terminal chars)))
               (when osc-chars
                 ;;(print (nreverse osc-chars))
                              )))
      (#\bel ;;(print (nreverse chars))
       )
      (otherwise (read-osc terminal (cons ch chars))))))

(defun read-output (terminal)
  (let ((ch (read-char-safe (terminal-pty terminal))))
    (case ch
      ((nil) :eof)
      ((#\bel #\so #\si #\del) :ignore)
      (#\bs (move-caret-x-relative terminal -1))
      (#\cr (move-caret-x terminal 0))
      ((#\lf #\vt #\ff) (newline terminal))
      ((#\can #\sub) :ignore)
      (#\esc (read-esc terminal nil))
      (#\device-control-string (read-dcs terminal nil))
      (#\start-string (read-sos terminal nil))
      (#\control-sequence-introducer (read-csi terminal nil))
      (#\string-terminator nil)
      (#\operating-system-command (read-osc terminal nil))
      (#\privacy-message (read-pm terminal nil))
      (#\application-program-command (read-apc terminal nil))
      (otherwise (draw-glyph terminal ch)))))


(macrolet ((for-each-lisp-buffer (&body body)
             `(let ((w (array-dimension lisp-buffer 1))
                    (h (array-dimension lisp-buffer 0)))
                (loop :for y :from 0 :below h
                      :do (loop :for x :from 0 :below w
                                :for offset :from (offset (+ foreign-buffer-x x)
                                                     (+ foreign-buffer-y y)
                                                     foreign-buffer-width)
                                  :by 4
                                :do ,@body)))))
  (defun copy-foreign-buffer-to-lisp-buffer (lisp-buffer foreign-buffer
                                             foreign-buffer-x foreign-buffer-y
                                             foreign-buffer-width)
    (for-each-lisp-buffer (setf (aref lisp-buffer y x)
                                (cffi:mem-ref foreign-buffer :uint32 offset))))
  (defun copy-lisp-buffer-to-foreign-buffer (lisp-buffer foreign-buffer
                                             foreign-buffer-x foreign-buffer-y
                                             foreign-buffer-width)
    (for-each-lisp-buffer (set-color foreign-buffer offset (aref lisp-buffer y x)))))

(defun backup-caret-position-buffer (terminal)
  (let-terminal (pixel-x pixel-y width caret-backup-buffer segment) terminal
    (copy-foreign-buffer-to-lisp-buffer caret-backup-buffer
                                        (shm:shm-segment-data segment)
                                        pixel-x pixel-y width)))

(defun restore-caret-position-buffer (terminal)
  (let-terminal (pixel-x pixel-y width caret-backup-buffer segment) terminal
    (copy-lisp-buffer-to-foreign-buffer caret-backup-buffer
                                        (shm:shm-segment-data segment)
                                        pixel-x pixel-y width)))



(defun draw-caret (terminal)
  (let-terminal (pixel-x pixel-y glyph-height half-char-width segment width) terminal
    (let ((data (shm:shm-segment-data segment)))
      (loop :for y :from pixel-y :below (+ pixel-y glyph-height)
            :do (loop :repeat half-char-width
                      :for offset :from (offset pixel-x y width) :by 4
                      :do (let ((color (cffi:mem-ref data :uint32 offset)))
                            (set-color data offset
                                       (make-color (alpha color)
                                                   (- 255 (red color))
                                                   (- 255 (green color))
                                                   (- 255 (blue color))))))))))



(defun draw (terminal)
  (with-terminal (client window gc width height shmseg) terminal
    (shm:shm-put-image client window gc width height
                       0 0 width height
                       0 0 32 x:+image-format--zpixmap+ 0 shmseg 0)
    (x:flush client))
  (values))

(defun on-expose-handler (terminal)
  (lambda (e)
    (declare (ignore e))
    (draw terminal)))


(defun on-key-press (terminal e)
  (x:with-key-press-event (detail state) e
    (with-terminal (client pty) terminal
      (let ((ch (x:keycode->keysym client detail state)))
        (case ch
          (:return
            (write-char #\NewLine pty)
            )
          (:tab (write-char #\tab pty))
          (:backspace
           (write-char #\bs pty))
          (:escape (write-char #\esc)
           (write-char #\esc pty))
          (otherwise
           (when (characterp ch)
             
             (cond ((logbitp x:+key-but-mask--control+ state)
                    (cond ((char<= #\@ ch #\_)
                           (write-byte (- (char-code ch) 64) pty))
                          ((char<= #\a ch #\z)
                           (write-byte (- (char-code ch) 96) pty))
                          (t
                           (write-char ch pty))))
                   ((logbitp x:+key-but-mask--mod1+ state)
                    (write-char #\esc pty)
                    (write-char ch pty))
                   (t
                    (write-char ch pty))))))
        (finish-output pty)))))

(defun on-key-press-handler (terminal)
  (lambda (e)
    (on-key-press terminal e)))


(defun main-loop (terminal)
  (let-terminal (pty client) terminal
    (x:process-input client)
    (when (open-stream-p pty)
      (when (listen pty)
        (restore-caret-position-buffer terminal)
        (loop :until (eql (read-output terminal) :eof))
        (backup-caret-position-buffer terminal)
        (draw-caret terminal)
        (draw terminal))
      (sleep 0.017)
      (main-loop terminal))))

(defvar *terminal* nil)

(defun open-terminal (&key font (font-size 12))
  (when *terminal*
    (error "terminal has already opened."))
  (unless font
    (error "font is required."))
  (x:with-connected-client (client)
    (x:set-keycode-keysym-table client)
    (x:set-default-error-handler client (lambda (e) (print e) (finish-output)))
    (let ((terminal (make-terminal :client client
                                   :font font
                                   :font-size font-size))
          (screen (elt (x:setup-roots (x:client-server-information client)) 0)))
      (setf *terminal* terminal)
      (zpb-ttf:with-font-loader (loader font)
        (with-terminal (font-loader window pixmap gc shmseg colormap
                                    width height rows cols characters
                                    glyph-height half-char-width
                                    segment pty caret-backup-buffer)
            terminal
          (set-background terminal (terminal-background terminal))
          (setf font-loader loader)
          (set-font-size terminal font-size)
          (setf characters (make-character-buffer cols rows))
          (let ((half-width (array-dimension (get-glyph terminal #\M) 1)))
            (setf width (* half-width cols)
                  height (* glyph-height rows)
                  half-char-width half-width))
          (setf window (x:allocate-resource-id client)
                gc (x:allocate-resource-id client)
                shmseg (x:allocate-resource-id client)
                colormap (x:allocate-resource-id client)
                segment (shm:make-shm-segment (* 4 width height))
                caret-backup-buffer (make-array (list glyph-height half-char-width)
                                                :element-type '(unsigned-byte 32)
                                                :initial-element 0))
          (unwind-protect
               (let ((visual-id (find-visual-id client 32)))
                 (unless visual-id
                   (error "not found 32 depth visual type."))
                 (x:create-colormap client x:+colormap-alloc--none+ colormap
                                    (x:screen-root screen) visual-id)
                 (x:create-window client 32 window (x:screen-root screen)
                                  0 0 width height 0 0 visual-id
                                  (x:make-mask x:+cw--event-mask+
                                      x:+cw--colormap+ x:+cw--override-redirect+
                                    x:+cw--back-pixel+ x:+cw--border-pixel+)
                                  0 0 0 0 0 0 0 0 0 0 0
                                  (x:make-mask x:+event-mask--exposure+
                                      x:+event-mask--key-press+)
                                  0 colormap 0)
                 (x:create-gc client gc window 0 0 0 0 0 0 0 0
                              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                 (x:map-window client window)
                 (shm:init client)
                 (shm:x-attach client shmseg (shm:shm-segment-id segment) 0)
                 (clear-terminal terminal)
                 (setf pty (swterm.run-shell:run-shell cols rows
                            "bash"))
                 
                 (x:set-event-handler client x:+expose-event+
                                      (on-expose-handler terminal))
                 (x:set-event-handler client x:+key-press-event+
                                      (on-key-press-handler terminal))
                 (x:flush client)
                 (unwind-protect
                      (handler-bind ((stream-error (lambda (c)
                                                     (print c)
                                                     (return-from open-terminal)
                                                     )))
                        (main-loop terminal))
                   (close pty)))
            (sb-posix:wait)
            (shm:free-shm-segment segment)
            (setf *terminal* nil)))))))

