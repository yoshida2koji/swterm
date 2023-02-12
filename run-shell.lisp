(defpackage :swterm.run-shell
  (:use :cl)
  (:shadow :open :close)
  (:export :run-shell))

(in-package :swterm.run-shell)

(cffi:defcfun "open" :int
  (path (:pointer :char))
  (flags :int))

(cffi:defcfun "close" :int
  (fd :int))

(cffi:defcfun "ptsname_r" (:int)
  (fd :int)
  (buf (:pointer :char))
  (len :uint))

(cffi:defcfun "grantpt" :int (fd :int))

(cffi:defcfun "unlockpt" :int (fd :int))

(cffi:defcfun "fork" :int)

(cffi:defcfun "setsid" :int)

(cffi:defcfun "dup2" :int
  (oldfd :int)
  (newfd :int))

(cffi:defcfun "execvp" :int
  (file (:pointer :char))
  (argv (:pointer (:pointer :char))))

(cffi:defcstruct winsize
  (row :ushort)
  (col :ushort)
  (xpixel :ushort)
  (ypixel :ushort))


(cffi:defcfun ("ioctl" %ioctl-set-winsize) :int
  (fd :int)
  (request :ulong)
  (winsize (:pointer (:struct winsize))))

(defun set-winsize (fd w h)
  (cffi:with-foreign-object (winsize '(:struct winsize))
    (cffi:with-foreign-slots ((row col) winsize (:struct winsize))
      (setf row h
            col w)
      (%ioctl-set-winsize fd #x5414 winsize))))

(defun run-shell (w h &optional path)
  (cffi:with-foreign-string (ptmx-name "/dev/ptmx")
    (let ((ptmx (open ptmx-name 2))) ; O_RDWR
      (when (= ptmx -1)
        (error "failed open /dev/ptmx"))
      (when (= -1 (grantpt ptmx))
        (error "faild grantpt"))
      (when (= -1 (unlockpt ptmx))
        (error "faild unlockpt"))
      (cffi:with-foreign-pointer (pts-name 256 len)
        (when (= -1 (ptsname-r ptmx pts-name len))
          (error "faild ptsname"))
        (let ((pid (fork)))
          (cond ((< pid 0)
                 (error "faild fork"))
                ((= pid 0)
                 (sb-posix:unsetenv "INSIDE_EMACS")
                 (close ptmx)
                 (let ((sid (setsid)))
                   (when (= sid -1)
                     (error "setsid")))
                 (let ((pts (open pts-name 2))) ; O_RDWR
                   (when (= pts -1)
                     (error "faild open ~a" (cffi:foreign-string-to-lisp pts-name)))
                   (dup2 pts 0) ; stdin
                   (dup2 pts 1) ; stdout
                   (dup2 pts 2) ; stderr
                   (sb-posix:ioctl pts #x540E) ; TIOCSCTTY 0x540E
                   (set-winsize pts w h)

                   (close pts)
                   (sb-posix:setenv "TERM" "xterm-256color" 1)
                   (cffi:with-foreign-object (argv :pointer 2)
                     (cffi:with-foreign-string (shell-path (or path (sb-posix:getenv "SHELL")))
                       (setf (cffi:mem-aref argv :pointer 0) shell-path
                             (cffi:mem-aref argv :pointer 1) (cffi:null-pointer))
                       (execvp shell-path argv))))
                 (error "failed execvp"))
                (t
                 (sb-sys:make-fd-stream ptmx
                                        :input t :output t
                                        :external-format :default
                                        :element-type :default
                                        :dual-channel-p t))))))))
