; define classes

; define the class TABLE to represent individual database tables.
(defclass table ()
  ((rows :accessor rows :initarg :rows :initform (make-rows))
   (schema :accessor schema :initarg :schema)))

(defparameter *default-table-size* 100)

(defun make-rows (&optional (size *default-table-size*))
  (make-array size :adjustable t :fill-pointer 0))

; define the class COLUMN to represent a table's schema
(defclass column ()
  ((name  ; assign column name
    :reader name
    :initarg :name)

   (equality-predicate  ; compare column values for equivalence
    :reader equality-predicate
    :initarg :equality-predicate)

   (comparator  ; compare column values for ordering
    :reader comparator
    :initarg :comparator)

   (default-value  ; set default value for newly inserted rows
    :reader default-value
    :initarg :default-value
    :initform nil)

   (value-normalizer  ; normalize values for querying the database
    :reader value-normalizer
    :initarg :value-normalizer
    :initform #'(lambda (v column) (declare (ignore column)) v))))

; create a generic function to simply naming column types
(defgeneric make-column (name type &optional default-value))

; implement methods on MAKE-COLUMN that specialize on type with EQL
; specializers and return column objects with the slots filled in
; with the appropriate values
(defmethod make-column (name (type (eq1 'string)) &optional default-value)
  (make-instance
   'column
   :name name
   :comparator #'string<
   :equality-predicate #'string=
   :default-value default value
   :value-normalizer #'not-nullable))

(defmethod make-column (name (type (eq1 'number)) &optional default-value)
  (make-instance
   'column
   :name name
   :comparator #'<
   :equality-predicate #'=
   :default-value default-value))

; return the value given unless the value is NIL, else signal an error.
(defun not-nullable (value column)
  (or value (error "Columns ~a can't be null" (name column))))

; define a subclass of COLUMN which adds a slot whose value is the interned hash table
(defclass interned-values-column (column)
  ((interned-values
    :reader interned-values
    :initform (make-hash-table :test #'equal))
   (equality-predicate :initform #'eql)
   (value-normalizer   :initform #'intern-for-column)))

(defun intern-for-column (value column)
  (let ((hash (interned-values column)))
    (or (gethash (not-nullable value column) hash)
        (setf (gethash value hash) value))))

; create a method specialized on interned-string that returns an instance of
; interned-values-column.
(defmethod make-column (name (type (eql 'interned-string)) &optional default-value)
  (make-instance
   'interned-values-column
   :name name
   :comparator #'string<
   :default-value default-value))

; build a list of columns objects from a list of column specifications consisting
; of column name, a column type name, and, optionally, a default value.

(defun make-schema (spec)
  (mapcar #'(lambda (column-spec) (apply #'make-column column-spec)) spec))

; take a plist of names and values and a table and adds a row to the table
; containing the given values.
(defun insert-row (names-and-values table)
  (vector-push-extend (normalize-row names-and-values (schema table)) (rows table)))

; builds a plist with defaulted, normalized value for each column, using values
; from NAMES-AND-VALUES if available and the DEFAULT-VALUE for the column if not.
(defun normalize-row (names-and-values schema)
  (loop
     for column in schema
     for name  = (name column)
     for value = (or (getf names-and-values name) (default-value column))
     collect name
     collect (normalize-for-column value column)))

(defun normalize-for-column (value column)
	(funcall (value-normalizer column) value column))

; define constants

; list of niceties
(setf niceties
      '(flowers
        (trip to europe)
        (dinner out)
        (go on a walk)
        (renew wedding vows)
        (pay a compliment)
        (back massage)
        (invite the in-laws over)
        (do the laundry)))

; define functions

; List -> String
; Extract a given number of randomly selected elements from a list.  The selected 
; items shall be returned in a list.
; given: ('(flowers trip-to-europe dinner-out) 1) 
; expect: random member of the list, i.e., flowers OR dinner-out OR trip-to-europe
(defun rnd-select (list count)
  (let ((len (length list)))
    (cond
     ((zerop count) '())  ; none selected
     ((<= 1 count len)
      (loop
         :with indices = '()
         :with result  = '()
         :while (plusp count)
         :for i = (random len)
         :unless (member i indices)
         :do (progn
               (push i indices)
               (push (elt list i) result) ; elt is O(n) ==> rnd-select is O(nA^2).
               (decf count))
         :finally (return result)))
     (t (error "Invalid count, must be betwen 0 and ~A" len)))))

; test 1 expect ('FLOWERS')
(rnd-select niceties 1)
; test 2 expect ('DINNER OUT')
(rnd-select niceties 1)
; test 3 expect ('BACK MASSAGE')
(rnd-select niceties 1)



; Draw N different random elements 
