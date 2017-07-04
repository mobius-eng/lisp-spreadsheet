;; * Simple spreadsheet for LISP
;; ** Preambule
(in-package :cl-user)

(defpackage lisp-spreadsheet
  (:use :cl+qt))

(in-package :lisp-spreadsheet)

(in-readtable :qtools)

;; ** Internal data representation
;; *** DEFAULT-OUTPUT-FORMATTER
(defun default-output-formatter (x)
  (format nil "~A" x))

;; *** CELL-DATA
(defclass cell-data ()
  ((input
    :initarg :input
    :initform ""
    :accessor cell-data-input
    :documentation "Contents of the cell when it is edited")
   (output
    :initarg :output
    :initform ""
    :accessor cell-data-output
    :documentation "Contents of the cell as shown in the table")
   (value
    :initarg :value
    :initform nil
    :accessor cell-data-value
    :documentation "Actual LISP value of the cell")
   (output-formatter
    :initarg :output-formatter
    :initform #'default-output-formatter
    :accessor cell-data-output-formatter
    :documentation "Function formatting the output")
   (input-reader
    :initarg :input-reader
    :initform #'read-from-string
    :accessor cell-data-input-reader
    :documentation "Function capable of reading the input from the cell input")
   (input-evaluator
    :initarg :input-evaluator
    :initform #'eval
    :accessor cell-data-input-evaluator
    :documentation "Evaluator of the cell input after it was processed by a reader"))
  (:documentation
   "Individual spreadsheet cell data representation"))

;; *** INITIALIZE-INSTANCE
;; Input was set, but the value and the output need to processed when the
;; cell is created:
(defmethod initialize-instance :after ((instance cell-data) &key)
  (process-input instance))

;; *** PROCESS-INPUT
;; Computes the value and produces output.
;; Snag: =READ-FROM-STRING= on empty string produces error. Also, in
;; general might have wrong input -- need to control for it.
;; This needs improvement in the future.
(defmethod process-input ((cell cell-data))
  (with-accessors ((input cell-data-input)
                   (value cell-data-value)
                   (output cell-data-output)
                   (reader cell-data-input-reader)
                   (evaluator cell-data-input-evaluator)
                   (formatter cell-data-output-formatter))
      cell
    (let ((input-expression (handler-case
                                (funcall reader input)
                              (error nil))))
      (setf value (funcall evaluator input-expression))
      (setf output (funcall formatter value))
      value)))

;; ** Spreadsheet widget
;; *** SPREADSHEET
;; Class spreadsheet is the main class. It is not a proper spreadsheet
;; yet. Main deficiency: it doesn't track dependencies and it does not
;; update depedent cells. This needs to be improved in the future.
(define-widget spreadsheet (QTableWidget)
  ((data
    :initform (make-hash-table :test 'equal)
    :initarg :data
    :accessor spreadsheet-data
    :documentation "Data backing spreadsheet representation")
   (editing
    :initform nil
    :accessor spreadsheet-editing-p
    :documentation
    "Indicator if a cell being currently edited. It is used to track
    when to initiate cell content recalculation and what effect should
    different inputs have")))

;; *** SET-CELL-TEXT-NO-SIGNALS
;; Sets the text of the cell to a particular value. Blocks any signals
;; that may trigger the cell contents being updated event.
(defun set-cell-text-no-signals (spreadsheet row col text)
  (let (blocked)
    (setf blocked (q+:block-signals spreadsheet t))
    (q+:set-text (q+:item spreadsheet row col) text)
    (q+:block-signals spreadsheet blocked)))

;; *** ENSURE-SPREADSHEET-CELL
;; Initially all the cells in the spreadsheet are empty (As a note, they
;; can be created when the spreadsheet is created). However, to process
;; all the updates in the cell text, =CELL-DATA= backing and
;; =QTableWidgetItem= need to be initialized
(defun ensure-spreadsheet-cell (spreadsheet row col)
  (with-accessors ((data spreadsheet-data)) spreadsheet
    (let ((cell-data (gethash (list row col) data))
          blocked)
      (when cell-data
        (format t "~&ENSURE-SPREADSHEET-CELL: Cell exists~%"))
      (unless cell-data
        (setf (gethash (list row col) data) (make-instance 'cell-data))
        (format t "~&ENSURE-SPREADSHEET-CELL: ~A~%" (gethash (list row col) data))
        (setf cell-data (gethash (list row col) data))
        (setf blocked (q+:block-signals spreadsheet t))
        (q+:set-item spreadsheet
                     row col
                     (q+:make-qtablewidgetitem (cell-data-output cell-data)))
        (q+:block-signals spreadsheet blocked)))))

;; *** Spreadsheet initializer SETUP
;; The single most important part of initialization is to connect the
;; signal of closing the cell editor to =CELL-FINISHED-EDIT= signal. This
;; allows update cells value even if there was no change made in the cell.
(define-initializer (spreadsheet setup)
  (q+:set-row-count spreadsheet 20)
  (q+:set-column-count spreadsheet 10)
  (with-accessors ((data spreadsheet-data)) spreadsheet
    (let (blocked)
      (setf blocked (q+:block-signals spreadsheet t))
      (loop for loc being the hash-keys of data
         using (hash-value cell-data)
         do (q+:set-item spreadsheet
                         (car loc) (cadr loc)
                         (q+:make-qtablewidgetitem (cell-data-output cell-data))))
      (q+:block-signals spreadsheet blocked))
    (qt:connect (q+:item-delegate spreadsheet)
                "closeEditor(QWidget*,QAbstractItemDelegate::EndEditHint)"
                spreadsheet
                (lambda (spreadsheet x y)
                  (declare (ignore x y))
                  (let ((row (q+:current-row spreadsheet))
                        (col (q+:current-column spreadsheet)))
                    (setf editing nil)
                    (signal! spreadsheet (cell-finished-edit "int" "int") row col))))))

;; *** CELL-BEGIN-EDIT signal
(define-signal (spreadsheet cell-begin-edit) ("int" "int"))

;; *** CELL-FINISHED-EDIT signal
(define-signal (spreadsheet cell-finished-edit) ("int" "int"))

;; *** KEY-PRESS-EVENT
;; =QTableWidget= in many ways is very primitive and signals that it
;; supplies are not safficient to do what is expected from a
;; spreadsheet. So, it is necessary to track key pressed event to react
;; appropriatly. Consider it like working with modes in vi:

;; - general mode allows to move across cells
;; - editing mode is to edit a cell
;; - command mode --- not impelemented, maybe.

;; General to Editing mode: press [Enter]

;; Editing to General:

;; - To accept: [Enter], [Up], [Down], [Left], [Right];
;; - To revert changes: [Esc]
(define-override (spreadsheet key-press-event) (event)
  (format t "~&----------------------~%")
  ;; (format t "~&Detected key press ~A~%" (q+:key event))
  ;; (format t "~&Modifiers: ~A~%" (q+:modifiers event))
  (format t "~&Key press event text: \"~A\"" (q+:text event))
  (format t "~&----------------------~%")
  (format t "~&EDITING: ~A~%" editing)
  (let ((row (q+:current-row spreadsheet))
        (col (q+:current-column spreadsheet))
        (key (q+:key event)))
    (cond ((and (not editing) (or (= key (q+:qt.key_enter))
                                  (= key (q+:qt.key_return))))
           (format t "~&Start editing cell~%")
           (setf editing t)
           (ensure-spreadsheet-cell spreadsheet row col)
           (signal! spreadsheet (cell-begin-edit "int" "int") row col)
           (q+:edit-item spreadsheet (q+:item spreadsheet row col))
           (format t "~&After EDIT-ITEM call~%"))
          ((and (not editing) (plusp (length (q+:text event))))
           (format t "~&Textual input in general mode: press ENTER to start editing~%"))
          ((and editing (or (= key (q+:qt.key_enter))
                            (= key (q+:qt.key_return))
                            (= key (q+:qt.key_up))
                            (= key (q+:qt.key_down))
                            (= key (q+:qt.key_left))
                            (= key (q+:qt.key_right))))
           (setf editing nil)
           (call-next-qmethod))
          ((and editing (= key (q+:qt.key_escape)))
           (setf editing nil)
           (q+:close-persistent-editor spreadsheet row col))
          (t (call-next-qmethod)))))

;; *** CELL-CHANGED
;; It is dangerous to react to this signal. It can be emitted when the
;; contents is changed programmatically or sometimes the cell text need
;; an update even when the input hasn't changed. So, it is here, but does
;; nothing and will probably be removed.
(define-slot (spreadsheet cell-changed-slot) ((row "int") (col "int"))
  (declare (connected spreadsheet (cell-changed "int" "int")))
  (format t "~&Finished editing cell~%")
  ;; it is signalled when editor is closed
  ;;(signal! spreadsheet (cell-finished-edit "int" "int") row col)
  )

;; *** DOUBLE-CLICKED Slot
;; Itiates editing on the cell double click. Not sure what will happen on
;; double click when editing is going..
(define-slot (spreadsheet cell-doulble-clicked) ((row "int") (col "int"))
  (declare (connected spreadsheet (cell-double-clicked "int" "int")))
  (format t "~&Double-click detected ~A ~A~%" row col)
  (ensure-spreadsheet-cell spreadsheet row col)
  (setf editing t)
  (signal! spreadsheet (cell-begin-edit "int" "int") row col))

;; *** CELL-BEGIN-EDIT Slot
;; Start of the editing. Need to swap the contents of the inactive cell
;; (which is its back-end data output) with the input of its back-end
;; data. It is the input that will be eventually evaluated.
(define-slot (spreadsheet cell-begin-edit) ((row "int") (col "int"))
  (declare (connected spreadsheet (cell-begin-edit "int" "int")))
  (format t "~&BEGIN-EDIT detected on ~A ~A~%" row col)
  (format t "~&Input ~A~%" (cell-data-input (gethash (list row col) data)))
  (set-cell-text-no-signals spreadsheet
                            row col
                            (cell-data-input (gethash (list row col) data))))

;; *** CELL-FINISHED-EDIT Slot
;; The signal =CELL-FINISHED-EDIT= must be invoked *only* after the cell
;; editor is closed, otherwise it is impossible to read the input from
;; the cell. The slot is replacing the input with the back-end data
;; output after processing the input.
(define-slot (spreadsheet cell-finished-edit spreadsheet-cell-finished-edit)
    ((row "int") (col "int"))
  (declare (connected spreadsheet (cell-finished-edit "int" "int")))
  (format t "~&FINISHED-EDIT detected on ~A ~A~%" row col)
  (let ((cell-data (gethash (list row col) data))
        (item (q+:item spreadsheet row col)))
    (format t "~&Current-input: ~A~%" (q+:text item))
    (setf (cell-data-input cell-data) (q+:text item))
    (process-input cell-data)
    (format t "~&Output ~A~%" (cell-data-output cell-data))
    (set-cell-text-no-signals spreadsheet
                              row col
                              (cell-data-output cell-data))
    (q+:resize-row-to-contents spreadsheet row)))

;; ** Demo
(defvar *h* (make-hash-table :test 'equal))

(defun run-spreadsheet (&optional (h *h*))
  (with-main-window
      (window (make-instance 'spreadsheet
                :data h))
    (format t "~&Running~%")))

;; ** End
