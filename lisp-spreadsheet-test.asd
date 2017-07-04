#|
  This file is a part of lisp-spreadsheet project.
|#

(in-package :cl-user)
(defpackage lisp-spreadsheet-test-asd
  (:use :cl :asdf))
(in-package :lisp-spreadsheet-test-asd)

(defsystem lisp-spreadsheet-test
  :author "Alexey Cherkaev"
  :license "BSD"
  :depends-on (:lisp-spreadsheet
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "lisp-spreadsheet"))))
  :description "Test system for lisp-spreadsheet"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
