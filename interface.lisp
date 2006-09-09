;; -*- lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;**************************************************************************************************
;; FOMUS
;; interface.lisp
;;**************************************************************************************************

(in-package :fomus)
(compile-settings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERFACE SINGLE FUNCTION CALL

(defparameter *fomus-args* nil)

(defun run-fomus (&rest args &key allow-other-keys &allow-other-keys)
  (macrolet ((fma () (let ((n (mapcar (lambda (x) (declare (type cons x)) (intern (symbol-name (first x)) :fomus)) +settings+))
			   (v (mapcar (lambda (k) (declare (type cons k)) (find-symbol (conc-strings "*" (symbol-name (first k)) "*") :fomus)) +settings+)))
		       #+debug (when (position nil v) (error "Error in FOMUS")) 
		       `(destructuring-bind (&key ,@(mapcar (lambda (x y) (list x y)) n v) &allow-other-keys) args
			 (progv (quote ,v) (list ,@n)
			   (fomus-main)))))
	     (fm () (let ((n (mapcar (lambda (x) (declare (type cons x)) (intern (symbol-name (first x)) :fomus)) +settings+))
			  (v (mapcar (lambda (k) (declare (type cons k)) (find-symbol (conc-strings "*" (symbol-name (first k)) "*") :fomus)) +settings+)))
		      #+debug (when (position nil v) (error "Error in FOMUS")) 
		      `(destructuring-bind (&key ,@(mapcar (lambda (x y) (list x y)) n v) other-keys) args
			(declare (ignore other-keys))
			(progv (quote ,v) (list ,@n)
			  (fomus-main))))))
    (if allow-other-keys
	#+(or cmu sbcl) (muffwarn (fma)) #-(or cmu sbcl) (fma)
	#+(or cmu sbcl) (muffwarn (fm)) #-(or cmu sbcl) (fm))))

(defun fomus (&rest args)
  (typecase (first args)
    (string (fomus-text (first args) (rest args) #'fomus-textexec))
    (list (apply #'run-fomus :chunks (first args) (rest args)))
    (t (apply #'run-fomus args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERFACE MULTIPLE FUNCTION CALL

(declaim (type list *fomus-global* *fomus-parts* *fomus-events*))
;; (defparameter *fomus-timesigs* nil)
;; (defparameter *fomus-keysigs* nil)
(defparameter *fomus-global* nil)
(defparameter *fomus-parts* nil)
(defparameter *fomus-events* nil)

(defun fomus-init (&rest args)
  (progn (setf *fomus-args* args *fomus-global* nil *fomus-parts* nil *fomus-events* nil) t))

(defun fomus-newtimesig (&rest args)
  (let ((ts (apply #'make-instance 'timesig args)))
    (push ts *fomus-global*)
    t))
(defun fomus-newpart (partid &rest args)
  (declare (type (or symbol real) partid))
  (let ((pa (apply #'make-instance 'part :partid partid args)))
    (push pa *fomus-parts*)
    t))
(defun fomus-newnote (partid &rest args)
  (declare (type (or symbol real) partid))
  (let ((no (apply #'make-instance 'note :partid partid args)))
    (push no *fomus-events*)
    t))
(defun fomus-newrest (partid &rest args)
  (declare (type (or symbol real) partid))
  (let ((re (apply #'make-instance 'rest :partid partid args)))
    (push re *fomus-events*)
    t))
(defun fomus-newmark (partid &rest args)
  (declare (type (or symbol real) partid))
  (let ((re (apply #'make-instance 'mark :partid partid args)))
    (push re *fomus-events*)
    t))

;;(declaim (inline fomus-part))
(defun fomus-part (sym)
  (declare (type (or symbol real) sym))
  (find sym *fomus-parts* :key #'part-partid))

;; should this function save additional objects for future calls?
(defun fomus-exec (&rest args) 
  (unwind-protect
       (apply #'run-fomus
	      :global (append *global* *fomus-global*)
	      :parts (append *parts* (nreverse *fomus-parts*))
	      :events (append *events* *fomus-events*)
	      (append args *fomus-args*))
    (fomus-init)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEXT INTERFACE

(defun fomus-textexec (args) 
  (apply #'run-fomus
	 :global (append *global* *fomus-global*)
	 :parts (append *parts* (nreverse *fomus-parts*))
	 :events (append *events* *fomus-events*)
	 (nconc *fomus-args* args)))
(defun fomus-textret (args) 
  (values
   (append *parts* (nreverse *fomus-parts*))
   (append *events* *fomus-events*)
   (append *global* *fomus-global*)
   (nconc *fomus-args* args)))

(defun fomus-text (filename args exe)
  (let ((*fomus-args* args)
	(*fomus-global* nil)
	(*fomus-parts* nil)
	(*fomus-events* nil))
    (when (and (numberp *verbose*) (>= *verbose* 1)) (out ";; Loading input file ~S...~%" filename))
    (funcall exe
     (with-open-file (f filename :direction :input)
       (flet ((git (rs rrs)
		(unless (symbolp rs) (error "Invalid tag ~S" rs))
		(case (intern (symbol-name rs) :keyword)
		  (:init (if (find (first rrs) +settings+ :key #'first) rrs (progn (format t ";; WARNING: Unknown setting ~A~%" (first rrs)) nil)))
		  (:timesig (apply #'fomus-newtimesig rrs) nil)
		  (:part (apply #'fomus-newpart rrs) nil)
		  (:note (apply #'fomus-newnote rrs) nil)
		  (:rest (apply #'fomus-newrest rrs) nil)
		  (:mark (apply #'fomus-newmark rrs) nil)
		  (otherwise (error "Invalid tag ~S" rs)))))
	 (loop
	  for re = (read f nil 'eof) until (eq re 'eof)
	  if (listp re) nconc (git (first re) (rest re))
	  else nconc (with-input-from-string (st (loop
						  with st = (read-line f)
						  for s = (string-right-trim " " st)
						  while (char= (aref s (1- (length s))) #\\)
						  do (setf st (conc-strings (subseq s 0 (1- (length s))) " " (read-line f)))
						  finally (return st)))
		       (git re (loop for e = (read st nil 'eof) until (eq e 'eof) collect e)))))))))

(defun fomus-file (filename &optional args)
  (fomus-text filename args #'fomus-textret))

