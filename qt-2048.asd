(in-package #:cl-user)
(asdf:defsystem qt-2048
  :version "1.0.0"
  :serial T
  :components ((:file "2048")
               (:file "2048-ai")
               (:file "2048-list-qt"))
  :depends-on (:qtools
               :qtcore
               :qtgui)
  :defsystem-depends-on (:qtools)
  :build-operation "qt-program-op"
  :build-pathname "qt-2048"
  :entry-point "qt-2048:main")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; qt-2048.asd ends here
