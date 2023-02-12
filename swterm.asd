(defsystem "swterm"
  :version "0.1"
  :author "yoshida koji"
  :license "MIT"
  :depends-on ("xclhb"
               "xclhb-shm"
               "cffi"
               "cl-vectors"
               "cl-paths-ttf"
               "cl-aa-misc"
               "str")
  :serial t
  :components ((:file "ansi-256-color")
               (:file "run-shell")
               (:file "swterm")))
