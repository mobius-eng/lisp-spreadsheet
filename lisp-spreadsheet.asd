#|
  This file is a part of lisp-spreadsheet project.
|#

(in-package :cl-user)
(defpackage lisp-spreadsheet-asd
  (:use :cl :asdf))
(in-package :lisp-spreadsheet-asd)

(defsystem lisp-spreadsheet
  :version "0.1"
  :author "Alexey Cherkaev"
  :license "BSD"
  :depends-on ("qtools" "qtgui" "qtcore")
  :components ((:module "src"
                :components
                ((:file "lisp-spreadsheet"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.org"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op lisp-spreadsheet-test))))
