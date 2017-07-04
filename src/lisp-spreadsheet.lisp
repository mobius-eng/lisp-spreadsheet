;; * Simple spreadsheet for LISP
;; ** Preambule
(in-package :cl-user)

(defpackage lisp-spreadsheet
  (:use :cl+qt :qtools-ui))

(in-package :lisp-spreadsheet)

(in-readtable :qtools)

;; ** Internal data representation
;; *** DEFAULT-OUTPUT-FORMATTER
(defun default-output-formatter (x)
  (format nil "~A" x))

;; *** CELL-DATA
(defclass cell-data ()
  ((empty-p
    :initform t
    :documentation "Empty cells need special treatment"
    :accessor cell-data-empty-p)
   (presentation-mode
    :initform :output
    :accessor cell-data-presentation-mode
    :documentation "Way the cell is presented. Possible values :OUTPUT, :VALUE, and :INPUT")
   (input
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
(defmethod initialize-instance :after ((instance cell-data) &key input &allow-other-keys)
  (process-input instance input))

;; *** PROCESS-INPUT
;; Computes the value and produces output.
;; Snag: =READ-FROM-STRING= on empty string produces error. Also, in
;; general might have wrong input -- need to control for it.
;; This needs improvement in the future.
(defmethod process-input ((cell cell-data) new-input)
  (with-accessors ((input cell-data-input)
                   (value cell-data-value)
                   (output cell-data-output)
                   (reader cell-data-input-reader)
                   (evaluator cell-data-input-evaluator)
                   (formatter cell-data-output-formatter)
                   (empty-p cell-data-empty-p))
      cell
    (if (or (null new-input) (= (length new-input) 0))
        (setf input "" value nil output "" empty-p t)
        (handler-case
            (let* ((input-expression (funcall reader new-input))
                   (new-value (funcall evaluator input-expression))
                   (new-output (funcall formatter new-value)))
              (setf input new-input value new-value output new-output empty-p nil))
          (reader-error ()
            (vom:error "Error in reader on ~S" new-input))
          (stream-error ()
            (vom:error "Error while consuming a string ~S" new-input))
          (error ()
            (vom:error "Unknown error while reading/evaluating ~S" new-input))))
    value))

;; *** presentation
(defmethod cell-data-presentation ((cell cell-data))
  (with-slots (presentation-mode input output value) cell
    (case presentation-mode
      (:output output)
      (:value (with-output-to-string (s)
                (prin1 value s)))
      (:input input))))

;; ** Spreadsheet widget
;; *** *CURRENT-SPREADSHEET*
;; There can be only one spreadsheet at a time. Otherwise references to
;; the cells make no sense.
(defvar *current-spreadsheet* nil)

;; *** SPREADSHEET
;; Class spreadsheet is the main class. It is not a proper spreadsheet
;; yet. Main deficiency: it doesn't track dependencies and it does not
;; update depedent cells. This needs to be improved in the future.
(define-widget spreadsheet (QTableWidget)
  ((cell-reader
    :initarg :cell-reader
    :accessor spreadsheet-cell-reader
    :documentation "Function reading the input from a cell")
   (cell-evaluator
    :initarg :cell-evaluator
    :accessor spreadsheet-cell-evaluator
    :documentation "Function evaluating the parsed (read) input")
   (cell-formatter
    :initarg :cell-formatter
    :accessor spreadsheet-cell-formatter
    :documentation "Function formatting the value of a cell")
   (rows-number
    :initarg :rows-number
    :reader spreadsheet-rows-number
    :documentation "Number of rows in the spreadsheet")
   (cols-number
    :initarg :cols-number
    :reader spreadsheet-cols-number
    :documentation "Number of columns in the spreadsheet")
   (data
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
;; WON'T NEED IT
;; Initially all the cells in the spreadsheet are empty (As a note, they
;; can be created when the spreadsheet is created). However, to process
;; all the updates in the cell text, =CELL-DATA= backing and
;; =QTableWidgetItem= need to be initialized
;; (defun ensure-spreadsheet-cell (spreadsheet row col)
;;   (with-accessors ((data spreadsheet-data)) spreadsheet
;;     (let ((cell-data (gethash (list row col) data))
;;           blocked)
;;       (when cell-data
;;         (format t "~&ENSURE-SPREADSHEET-CELL: Cell exists~%"))
;;       (unless cell-data
;;         (setf (gethash (list row col) data) (make-instance 'cell-data))
;;         (format t "~&ENSURE-SPREADSHEET-CELL: ~A~%" (gethash (list row col) data))
;;         (setf cell-data (gethash (list row col) data))
;;         (setf blocked (q+:block-signals spreadsheet t))
;;         (q+:set-item spreadsheet
;;                      row col
;;                      (q+:make-qtablewidgetitem (cell-data-output cell-data)))
;;         (q+:block-signals spreadsheet blocked)))))

;; *** Spreadsheet initializer SETUP
;; The single most important part of initialization is to connect the
;; signal of closing the cell editor to =CELL-FINISHED-EDIT= signal. This
;; allows update cells value even if there was no change made in the cell.
(define-initializer (spreadsheet setup)
  (q+:set-row-count spreadsheet rows-number)
  (q+:set-column-count spreadsheet cols-number)
  (setf data (make-array (list rows-number cols-number)
                         :initial-contents
                         (loop for i from 0 below rows-number
                            collect (loop for j from 0 below cols-number
                                       collect (make-instance 'cell-data
                                                 :input-reader cell-reader
                                                 :input-evaluator cell-evaluator
                                                 :output-formatter cell-formatter)))))
  (with-signals-blocked (spreadsheet)
    (dotimes (i rows-number)
      (dotimes (j cols-number)
        (q+:set-item spreadsheet i j
                     (q+:make-qtablewidgetitem
                      (cell-data-presentation (aref data i j)))))))
  (qt:connect (q+:item-delegate spreadsheet)
              "closeEditor(QWidget*,QAbstractItemDelegate::EndEditHint)"
              spreadsheet
              (lambda (spreadsheet x y)
                (declare (ignore x y))
                (let ((row (q+:current-row spreadsheet))
                      (col (q+:current-column spreadsheet)))
                  (setf editing nil)
                  (signal! spreadsheet (cell-finished-edit "int" "int") row col))))
  (setf *current-spreadsheet* spreadsheet))

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

;; General mode:
;; - to edit [Enter]
;; - to switch cell view to output (default) [O]
;; - to switch cell view to input [I]
;; - to switch cell view to value [V]

;; Editing to General:

;; - To accept: [Enter], [Up], [Down], [Left], [Right];
;; - To revert changes: [Esc]
(define-override (spreadsheet key-press-event) (event)
  (vom:info "Key press event text: \"~A\"" (q+:text event))
  (vom:info "EDITING: ~A" editing)
  (let ((row (q+:current-row spreadsheet))
        (col (q+:current-column spreadsheet))
        (key (q+:key event)))
    (cond ((and (not editing) (or (= key (q+:qt.key_enter))
                                  (= key (q+:qt.key_return))))
           (vom:info "Start editing cell")
           (setf editing t)
           (signal! spreadsheet (cell-begin-edit "int" "int") row col)
           (q+:edit-item spreadsheet (q+:item spreadsheet row col)))
          ((and (not editing) (= key (q+:qt.key_v)))
           (vom:info "Key V is pressed. Cell ~A ~A in :VALUE mode" row col)
           (make-cell-show-value spreadsheet row col))
          ((and (not editing) (= key (q+:qt.key_o)))
           (vom:info "Key O is pressed. Cell ~A ~A in :OUTPUT mode" row col)
           (make-cell-show-output spreadsheet row col))
          ((and (not editing) (= key (q+:qt.key_i)))
           (vom:info "Key I is pressed. Cell ~A ~A in :INPUT mode" row col)
           (make-cell-show-input spreadsheet row col))
          ((and (not editing) (plusp (length (q+:text event))))
           (vom:warn "Textual input in general mode: press ENTER to start editing"))
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

;; *** DOUBLE-CLICKED Slot
;; Itiates editing on the cell double click. Not sure what will happen on
;; double click when editing is going..
(define-slot (spreadsheet cell-doulble-clicked) ((row "int") (col "int"))
  (declare (connected spreadsheet (cell-double-clicked "int" "int")))
  (vom:info "Double-click detected ~A ~A" row col)
  ;; (ensure-spreadsheet-cell spreadsheet row col)
  (setf editing t)
  (signal! spreadsheet (cell-begin-edit "int" "int") row col))

;; *** CELL-BEGIN-EDIT Slot
;; Start of the editing. Need to swap the contents of the inactive cell
;; (which is its back-end data output) with the input of its back-end
;; data. It is the input that will be eventually evaluated.
(define-slot (spreadsheet cell-begin-edit) ((row "int") (col "int"))
  (declare (connected spreadsheet (cell-begin-edit "int" "int")))
  (vom:info "BEGIN-EDIT detected on ~A ~A" row col)
  (vom:info "Input ~A" (cell-data-input (aref data row col)))
  (set-cell-text-no-signals spreadsheet
                            row col
                            (cell-data-input (aref data row col))))

;; *** UPDATE-CELL-TEXT
(defun update-cell-text (spreadsheet row col)
  "Updates the context of the cell according to its presentation
  without firing any signals of changed content"
  (let ((cell-data (aref (spreadsheet-data spreadsheet) row col)))
    (set-cell-text-no-signals spreadsheet
                              row col
                              (cell-data-presentation cell-data))
    (q+:resize-row-to-contents spreadsheet row)))

;; *** CELL-FINISHED-EDIT Slot
;; The signal =CELL-FINISHED-EDIT= must be invoked *only* after the cell
;; editor is closed, otherwise it is impossible to read the input from
;; the cell. The slot is replacing the input with the back-end data
;; output after processing the input.
(define-slot (spreadsheet cell-finished-edit spreadsheet-cell-finished-edit)
    ((row "int") (col "int"))
  (declare (connected spreadsheet (cell-finished-edit "int" "int")))
  (vom:info "FINISHED-EDIT detected on ~A ~A" row col)
  (let ((cell-data (aref data row col))
        (item (q+:item spreadsheet row col)))
    (vom:info "Current-input: ~A" (q+:text item))
    (process-input cell-data (q+:text item))
    (vom:info "Output ~A" (cell-data-output cell-data))
    (update-cell-text spreadsheet row col)))

;; *** Changing the presentation: let's change background color
(defun make-cell-show-* (spreadsheet row col show-what color)
  "Makes the cell show SHOW-WHAT (:INPUT, :OUTPUT, :VALUE).
  Sets the cell background to provided color"
  (let ((data (spreadsheet-data spreadsheet)))
    (setf (cell-data-presentation-mode (aref data row col)) show-what)
    (q+:set-background (q+:item spreadsheet row col)
                       (q+:make-qbrush color))
    (update-cell-text spreadsheet row col)))

(defun make-cell-show-input (spreadsheet row col)
  (make-cell-show-* spreadsheet row col :input (q+:make-qcolor 255 165 0)))

(defun make-cell-show-output (spreadsheet row col)
  (make-cell-show-* spreadsheet row col :output (q+:make-qcolor 255 255 255)))

(defun make-cell-show-value (spreadsheet row col)
  (make-cell-show-* spreadsheet row col :value (q+:make-qcolor 100 149 237)))


;; ** Cell access methods =CELL=
(defmethod cell ((row fixnum) (col fixnum))
  (cell-data-value (aref (spreadsheet-data *current-spreadsheet*)
                         (1- row)
                         (1- col))))

(defmethod cell ((row fixnum) (cols cons))
  (loop for c in cols
     collect (cell row c)))

(defmethod cell ((rows cons) (col fixnum))
  (loop for r in rows
     collect (cell r col)))

(defmethod cell ((rows cons) (cols cons))
  (loop for r in rows
     collect (cell r cols)))

(defmethod cell ((row t) (cols t))
  nil)

(defmethod cell-input ((row fixnum) (col fixnum))
  (cell-data-input (aref (spreadsheet-data *current-spreadsheet*)
                         (1- row)
                         (1- col))))

;; ** Demo
;; (defvar *h* (make-hash-table :test 'equal))

(define-widget spreadsheet-window (QWidget)
  ())

;; a bit of extra in read and formatter - to ease modification for MAXIMA
(define-subwidget (spreadsheet-window spreadsheet)
    (make-instance 'spreadsheet
      :cols-number 10
      :rows-number 20
      :cell-reader (lambda (x) (read-from-string (format nil "#$~A$" x)))
      :cell-evaluator #'eval
      :cell-formatter (lambda (x)
                        (with-output-to-string (s)
                          (let ((*standard-output* s))
                            (maxima::displa x)))))
  (q+:set-parent spreadsheet spreadsheet-window)
  (q+:set-font spreadsheet (q+:make-qfont "Monospace")))

(define-subwidget (spreadsheet-window input) (q+:make-qlabel spreadsheet-window)
  (q+:set-font input (q+:make-qfont "Monospace")))    ;; why not?

(defun update-input-info (input spreadsheet row col)
  (q+:set-text input
               (format nil
                       "CELL[~A, ~A]: ~A"
                       (1+ row) (1+ col)
                       (cell-data-input (aref (spreadsheet-data spreadsheet)
                                              row col)))))

(define-slot (spreadsheet-window cell-changed) ((cur-row "int")
                                                (cur-col "int")
                                                (prev-row "int")
                                                (prev-col "int"))
  (declare (connected spreadsheet (current-cell-changed "int" "int" "int" "int")))
  (update-input-info input spreadsheet cur-row cur-col))

(define-slot (spreadsheet-window cell-finished-edit) ((row "int") (col "int"))
  (declare (connected spreadsheet (cell-finished-edit "int" "int")))
  (update-input-info input spreadsheet row col))

(define-subwidget (spreadsheet-window repl) (make-instance 'qtools-ui:repl
                                              :error-stream *error-output*
                                              :output-stream *standard-output*))


(define-subwidget (spreadsheet-window layout) (q+:make-qgridlayout spreadsheet-window)
  (q+:set-column-stretch layout 0 2)
  (q+:set-column-stretch layout 1 1)
  (q+:add-widget layout spreadsheet 0 0)
  (q+:add-widget layout input 1 0)
  (q+:add-widget layout repl 0 1 2 1))

(defun run-spreadsheet ()
  (with-main-window
      (window 'spreadsheet-window)
    (format t "~&Running~%")))

;; ** End
