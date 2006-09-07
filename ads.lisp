;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;; Copyright (c) 2006, Kilian Sprotte. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; this is currently a very preliminary version

;;; reference
;;; ftp://ftp.inria.fr/INRIA/Projects/contraintes/publications/ADAPTIVE/saga01.html

(defpackage :ads
  (:use :cl :iterate)
  (:export #:make-int-var #:post #:ads))

(in-package :ads)

(setf *print-circle* t)	; not that I am really reading all this #1# stuff...

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defstruct var
  domain constraints value (tabu 0) (proj-cost most-positive-fixnum))

(defun var-set-randomly (var)
  (setf (var-value var)
	(nth (random (length (var-domain var))) (var-domain var))))

(defun var-cost-projection (var)
  (iter
    (for c in (var-constraints var))
    (summing (constraint-cost c))))

(defstruct constraint
  vars costfn
  cost			      ; when solving, the actual computed cost
  )

(defun constraint-compute (constraint)
  (setf (constraint-cost constraint)
	(apply (constraint-costfn constraint)
	       (constraint-vars constraint))))

(defun post (form &rest vars)
  (flet ((var-syms (num)
	   (iter
	     (for i from 1 to num)
	     (collect (intern (format nil "V~A" i))))))
    (let* ((var-syms (var-syms (length vars)))
	   (c (make-constraint
	       :vars vars
	       :costfn (compile nil
				`(lambda (,@var-syms)
				   (let (,@(iter
					    (for s in var-syms)
					    (collect `(,s (var-value ,s)))))
				     ,form))))))
      (dolist (v vars)
	(push c (var-constraints v)))
      c)))

(defun make-int-var (from to)
  (make-var :domain (iter (for i from from to to) (collect i))))


(defun collect-constraints (vars)
  (iter
    top
    (for v in vars)
    (iter
      (for c in (var-constraints v))
      (in top (adjoining c)))))

(defun ads (vars &key (tabu-tenure 5))
  (let ((constraints (collect-constraints vars)))
    (iter
      (for v in vars)
      (var-set-randomly v))
    (iter
      (for c in constraints)
      (constraint-compute c))
    ;; the big search loop
    (iter
      (repeat 20)
      (print (list (mapcar #'var-value vars)
		   (iter
		     (for c in constraints)
		     (summing (constraint-cost c)))))
      ;; (print (mapcar #'var-proj-cost vars))
      ;; (print (mapcar #'var-tabu vars))
      ;; (terpri)
      (when (zerop (iter
		     (for c in constraints)
		     (summing (constraint-cost c))))
	(terminate))
      (let ((worst-var (iter
			 (for v in vars)
			 (unless (zerop (var-tabu v))
			   (decf (var-tabu v)))
			 (when (zerop (var-tabu v))
			   ;; (print (list 'looking 'at (var-value v)))
			   (finding v maximizing (var-cost-projection v))))))
	(unless worst-var (terminate))
	;; (print (list 'found (var-value worst-var)))
	(iter
	  (with old-value = (var-value worst-var))
	  (with proj)
	  (for possible-move in (var-domain worst-var))
	  (setf (var-value worst-var) possible-move)
	  (dolist (c (var-constraints worst-var)) (constraint-compute c))
	  (finding possible-move minimizing (setf proj (var-cost-projection worst-var))
		   into move)
	  (finally (cond
		     ((< proj (var-proj-cost worst-var))
		      (setf (var-value worst-var) move
			    (var-proj-cost worst-var) proj
			    (var-tabu worst-var) 2)
		      ;; (print (list 'moving 'to move))
		      (iter
			(for c in (var-constraints worst-var))
			(constraint-compute c)))
		     (t
		      ;; (print 'tabu)
		      (setf (var-value worst-var) old-value
			    (var-tabu worst-var) tabu-tenure)
		      (dolist (c (var-constraints worst-var)) (constraint-compute c)))))))
      (mapcar #'var-value vars))))

(let ((l (list (make-int-var 1 5)
	       (make-int-var 1 5)
	       (make-int-var 1 5))))
  ;; smallest distance as possible
  ;; between first and second
  (post
   '(abs (- v1 v2))
   (first l) (second l))
  ;; biggest distance as possible
  ;; between second and third
  (post
   '(max (- 10 (abs (- v1 v2))) 0)
   (second l) (third l))
  ;; the entire sum should
  ;; be greater than 10
  (post
   '(let ((sum (+ v1 v2 v3)))
     (if (< 10 sum)
	 0
	 (- 10 sum)))
   (first l) (second l) (third l))
  (ads l))

;; the optimal sol is ((5 5 1) 6)
;; which is already reached sometimes