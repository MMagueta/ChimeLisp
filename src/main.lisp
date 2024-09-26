(defpackage #:chimelisp
  (:use :cl))

(in-package #:chimelisp)

(defconstant +left-bracket+ #\[)
(defconstant +right-bracket+ #\])
(defconstant +colon+ #\:)

(defun chimelisp-symbolp (sym)
  (cond
    ((eq sym '+) nil)
    ((eq sym '|defun|) nil)
    ((eq sym '|module|) nil)
    ((eq sym '|nil|) nil)
    (t (symbolp sym))))

(defun read-next-object (delimiter &optional (input-stream *standard-input*))
  (flet ((peek-next-char () (peek-char t input-stream t nil t))
         (discard-next-char () (read-char input-stream t nil t)))
    (if (and delimiter (char= (peek-next-char) delimiter))
        (progn
          (discard-next-char)
          nil)
	(let ((object (read input-stream t nil t)))
	  (cond
	    ((numberp object) (list 'chimelisp/number object))
	    ((chimelisp-symbolp object) (list 'chimelisp/symbol object))
	    (t object))))))

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
		    ((and objects (eq (car objects)
				      '|module|)) (cons 'chimelisp/module (cdr objects)))
		    ((and objects (eq (car objects)
				      '|+|)) (cons 'chimelisp/+ (cdr objects)))
		    (t `',objects))))))

(set-macro-character +left-bracket+ 'read-left-bracket)
(set-macro-character +right-bracket+ 'read-delimiter)

(defmacro chimelisp/number (number)
  `'(|i32.const| ,number))

(defmacro chimelisp/defun (name args body)
  (let ((bare-name (cadr (eval name))))
    `'(|func| ,bare-name
       (|export| ,(symbol-name bare-name))
       ,@(mapcar #'(lambda (x) `(|param| ,(cadr (eval x)) |i32|)) (eval args))
       (|result| |i32|)
       ,(eval body))))

(defmacro chimelisp/symbol (sym)
  `'(|local.get| ,(intern (concatenate 'string "$" (symbol-name sym)))))

(defmacro chimelisp/+ (&rest args)
  (if args
      `'(|i32.add| ,(eval (car args)) ,(eval `(chimelisp/+ ,@(cdr args))))
      `'(|i32.const| 0)))

(defmacro chimelisp/module (&rest body)
  `'(|module| ,@(mapcar #'eval body)))


#|
(module
    ;; add(a, b) returns a+b
    (func $add (export "add") (param $a i32) (param $b i32) (result i32)
        (i32.add (local.get $a) (local.get $b))
    )
)
|#

(defmethod print-object :around ((object symbol) stream)
  (format stream "~a" (symbol-name object)))

(let ((ast [module [defun inc [x] [+ 1 x]]])
      (file-name "./WASM/test.wat"))
  (if (uiop:file-exists-p file-name)
      (delete-file file-name))
  (with-open-file (stream file-name :direction :output)
    (write ast :stream stream)))

(remove-method #'print-object
	       (find-method #'print-object
			    (list :around)
			    (list (find-class 'symbol)
				  (find-class t))))
