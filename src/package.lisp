;; Create a phony MAXIMA package
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (unless (find-package "MAXIMA")
;;     (make-package "MAXIMA")))

(defpackage lisp-spreadsheet
  (:use :cl+qt :qtools-ui)
  (:export
   #:cell
   #:recalculate-spreadsheet
   #:refresh-spreadsheet
   #:save-spreadsheet
   #:load-spreadsheet
   #:read-in-spreadsheet
   #:run-spreadsheet))
