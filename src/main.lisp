(defpackage #:chimelisp
  (:use :cl))

(in-package #:chimelisp)

(defclass literal ()
  ((context
    :initarg :context
    :accessor context)
   (value
    :initarg :value
    :accessor value)))

(defvar start_string_index 0)

(defmethod emit ((obj literal))
  (let ((value (value obj)))
    (cond
      ((integerp value) (setf (context obj) (format nil "(i32.const ~a)" value)))
      ((floatp value) (setf (context obj) (format nil "(f32.const ~a)" value)))
      ((stringp value)
       (progn
	 (setf (context obj)
	       (concatenate 'string
			    (context obj)
			    "(global $start_string (import \"env\" \"start_string\") i32)"))
	 (setf (context obj)
	       (concatenate 'string
			    (context obj)
			    (format nil "(global $string_len i32 (i32.const ~a))" (length value))))
	 (setf (context obj)
	       (concatenate 'string
			    (context obj)
			    (format nil "(data (global.get $start_string) ~a)" value)))))
      (t (error "Received a literal with content other than float32 and integer32.")))))

(defclass abstraction ()
    ((return-type
      :initarg :return-type
      :accessor return-type)
     (name
      :initarg :name
      :accessor name)
     (arguments
      :initarg :arguments
      :accessor arguments)
     (body
      :initarg :body
      :accessor body)
     (context
      :initarg :context
      :accessor context)))

(defun generate-param-list (param-names)
  (format nil "~{(param $~A i32)~^ ~}" param-names))

(defmethod emit ((obj abstraction))
  (let ((name (name obj)))
    (emit (body obj))
    (let ((body (context (body obj))))
      (if name
	  (setf (context obj)
		(concatenate 'string
			     (context obj)
			     (if (return-type obj)
				 (format nil
					 "(func $~a (export \"~a\") ~a (result ~a) ~a)"
					 name
					 name
					 (generate-param-list (arguments obj))
					 (return-type obj)
					 body)
				 (format nil "(func $~a (export \"~a\") ~a ~a)"
					 name
					 name
					 (generate-param-list (arguments obj))
					 body))))))))

(defclass application ()
  ((arguments
    :initarg :arguments
    :accessor arguments)
   (func
    :initarg :func
    :accessor func)
   (context
    :initarg :context
    :accessor context)))

(defun generate-arg-list (args)
  (format nil "~{~A~^ ~}" args))

(defmethod emit ((obj application))
  (let* ((func-name (name (func obj)))
	 (arguments (arguments obj))
	 (initial-term (cond
			 ((string= func-name "+") "(i32.add")
			 ((string= func-name "-") "(i32.sub")
			 ((string= func-name "*") "(i32.mul")
			 ((string= func-name "/") "(i32.div")
			 (t (format nil "(call $~a" func-name)))))
    (map 'list #'emit arguments)
    (setf (context obj)
	  (eval `(format nil ,(concatenate 'string
					   "~a"
					   (generate-arg-list (map 'list #'context arguments))
					   ")")
			 ,initial-term)))))

(defclass substitution ()
  ((label
    :initarg :label
    :accessor label)
   (global?
    :initarg :global?
    :accessor global?)
   (context
    :initarg :context
    :accessor context)))

(defmethod emit ((obj substitution))
  (if (global? obj)
      (setf (context obj)
	    (format nil "(global.get $~a)" (label obj)))
      (setf (context obj)
	    (format nil "(local.get $~a)" (label obj)))))

(defun chimelisp/compile ()
  (let* ((context "")
	 (literal
	   (make-instance 'literal :value 123 :context context))
	 (abstraction-1 (make-instance 'abstraction :context "" :body (make-instance 'substitution :label "x" :global? nil :context "") :arguments '("x") :name "xyz" :return-type "i32"))
	 (application (make-instance 'application :context ""
						  :func abstraction-1
						  :arguments (list (make-instance 'literal :value 1 :context ""))))
	 (abstraction
	   (make-instance 'abstraction :name "abc" :context context :body application :arguments '() :return-type "i32"))
	 (_ (emit abstraction-1))
	 (_ (emit abstraction))
	 (compiled-context (context abstraction))
	 (file-name "./WASM/test.wat"))
    (if (uiop:file-exists-p file-name)
	(delete-file file-name))
    (with-open-file (stream file-name :direction :output)
      (format stream
	      "(module (import \"env\" \"print_string\" (func $print_string (param i32))) (import \"env\" \"buffer\" (memory 1)) ~a ~a)"
	      (context abstraction-1)
	      compiled-context))
    ;; (uiop:run-program (list "wat2wasm" file-name "-o" "./WASM/test.wasm")
		      ;; :error-output :string
		      ;; :ignore-error-status nil)))
    (if (zerop (uiop:wait-process
		(uiop:launch-program (list "wat2wasm" file-name "-o" "./WASM/test.wasm"))))
	(format t "Compiled successfully!")
	(format t "Failed compiling!"))))

(defmacro comment (&rest a)
  nil)

#|
 (module
  (func (export "AddInt")
	(param $value_1 i32) (param $value_2 i32)
	(result i32)
	local.get $value_1
	local.get $value_2
	i32.add
	)
  )
|#

(defconstant +left-bracket+ #\[)
(defconstant +right-bracket+ #\])
(defconstant +colon+ #\:)

(defun read-next-object (delimiter &optional (input-stream *standard-input*))
  (flet ((peek-next-char () (peek-char t input-stream t nil t))
         (discard-next-char () (read-char input-stream t nil t)))
    (if (and delimiter (char= (peek-next-char) delimiter))
        (progn
          (discard-next-char)
          nil)
	(read input-stream t nil t))))

(defun read-left-bracket (stream char)
  (declare (ignore char))
  (let ((*readtable* (copy-readtable)))
    (setf (readtable-case *readtable*) :preserve)
    ;; (set-macro-character +comma+ 'read-separator)
    (loop
       :for object := (read-next-object +right-bracket+ stream)
       :while object
       :collect object :into objects
       :finally
	  (return (cond
		    ((and objects (eq (car objects)
				      '|defun|)) (cons 'chimelisp/defun (cdr objects)))
		    (t `',objects))))))

(set-macro-character +left-bracket+ 'read-left-bracket)
(set-macro-character +right-bracket+ 'read-delimiter)

(defmacro chimelisp/defun (name args body)
  (format t "Hello!")
  ''haha)

#|
[module
[func [export "AddInt"]
[param $value_1 i32] [param $value_2 i32]
[result i32]
local.get $value_1
local.get $value_2
i32.add]]
|#

(let ((ast [module [defun inc [x] [+ 1 x]]]))
  (write ast))
