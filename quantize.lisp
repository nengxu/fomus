;; -*- lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;**************************************************************************************************
;; FOMUS
;; quantize.lisp
;;**************************************************************************************************

(in-package :fomus)
(compile-settings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; QUANTIZE

(declaim (type symbol *auto-quantize-mod*))
(defparameter *auto-quantize-mod* t)
(declaim (inline auto-quantize-fun))
(defun auto-quantize-fun () (if (truep *auto-quantize-mod*) :quantize1 *auto-quantize-mod*))

(declaim (type boolean *auto-quantize*) 
	 (type integer *default-grace-num*))
(defparameter *auto-quantize* t)
(defparameter *default-grace-num* 0) 

(defun byfit-score (evpts qpts)
  (declare (type list evpts) (type list qpts))
  (sqrt (loop for e of-type (real 0) in evpts sum (let ((x (diff (loop-return-firstmin (diff i e) for i of-type (rational 0) in qpts) e))) (* x x)))))

(defun quantize-byfit (timesigs parts)
  (let ((h (get-timesigs timesigs parts)))
    (flet ((adj (l)			; list?
	     (declare (type list l))
	     #+debug (check-order (delete-duplicates (mapcan #'copy-list (mapcar #'cdr l))) "QUANTIZE-BYFIT (1)" #'<)
	     #+debug (check-order (delete-duplicates (mapcan #'copy-list (mapcar #'car l))) "QUANTIZE-BYFIT (2)" #'<)
	     (loop for (e0 . e1) of-type (list . list) in l
		   append e0 into l0
		   nconc e1 into l1
		   finally (return (cons (remove-duplicates l0) (delete-duplicates l1)))))
	   (sel (l) ; select best (orig-point-list . quant-point-list) match
	     (declare (type list l))
	     (loop-return-argmin
	      (byfit-score (car x) (cdr x))
	      for x of-type (cons list list) in (remove-if (lambda (x) (declare (type (cons list list) x)) (or (null (car x)) (null (cdr x)))) l))))
      (labels ((dv (o1 o2 pts rl lm)
		 (let ((du (- o2 o1)))
		   (when (and pts (>= du lm))
		     (sel (cons (cons pts (list o1 o2))
				(loop for (s . r) in (split-rules-bylevel rl
									  (and (or (null *min-tuplet-dur*) (>= du *min-tuplet-dur*))
									       (or (null *max-tuplet-dur*) (<= du *max-tuplet-dur*))))
				      unless (some (lambda (x) (or (unit-nodiv-p x) (sig-nodiv-p x))) r) ; no nodivs
				      collect (flet ((of (o) (declare (type (rational 0) o)) (+ o1 (* o du))))
						(adj
						 (loop for (o1 o2) of-type ((rational 0 1) (or (rational 0 1) null)) on (cons 0 (append (force-list s) '(1)))
						       #-clisp while #-clisp o2
						       for oo1 = #-clisp (of o1) #+clisp (if o2 (of o1) (loop-finish))
						       #-clisp and #+clisp for oo2 = (of o2) and ru in r
						       for di = (dv oo1 oo2 (remove-if (lambda (e) (or (< e oo1) (> e oo2))) pts) ru lm)
						       #+debug when #+debug (and (unitp ru) (rule-sim ru)) #+debug do #+debug (error "Error in QUANTIZE-BYFIT")
						       if di collect di #|else do (return-from qua)|#))))))))))
	(loop for p of-type partex in parts
	      for ph = (gethash p h)	; ph = timesigs for part
	      do (let* ((ee (sort (delete-duplicates (loop for e of-type (or noteex restex) in (part-events p) collect (event-off e) collect (event-endoff e))) #'<)) ; offset points
			(qs (sort
			     (delete-duplicates
			      (mapcan #'cdr
				      (loop with ep = ee
					    for (e n) of-type (timesig (or timesig null)) on (reverse ph) nconc
					    (loop with eo = (or (if n (timesig-off n) (last-element ep)) (return nil)) ; last offset
						  for o from (timesig-off e) below eo by (timesig-nbeats e)
						  do (loop until (or (null ep) (>= (first ep) o)) do (pop ep))
						  collect (let ((o2 (+ o (timesig-nbeats e)))) (dv o o2
												   (loop for e in ep while (<= e o2) collect e)
												   (first-splitrule e)
												   (* (/ (beat-division e)) 3/4)))
						  do (print-dot)))))
			     #'<)))
		   (loop with mg = (or (max-list (loop for e in (part-events p) when (event-grace e) collect (event-grace e)))
				       (1- *default-grace-num*))
			 and ad
			 for e of-type (or noteex restex) in (part-events p) do
			 (let ((o (event-off e)))
			   (loop while (and (list>1p qs) (< (second qs) o)) do (pop qs))
			   (let ((e1 (loop-return-firstmin (diff x o) for x of-type (rational 0) in qs))) 
			     (flet ((aa (oo ee)
				      (declare (type (real 0) oo) (type (rational 0) ee))
				      (cond ((< oo ee) (push (cons (cons #'>= oo) ee) ad)) ; -->
					    ((> oo ee) (push (cons (cons #'< oo) ; <--
								   (loop for (x1 x2) of-type ((rational 0) (or (rational 0) null)) on qs until (or (null x2) (>= x2 ee))
									 finally (return x1)))
							     ad)))))
			       (if (event-grace e)
				   (progn
				     (cond ((< (event-off e) e1) (push (cons (cons #'>= (event-off e)) e1) ad))	; -->
					   ((> (event-off e) e1) (push (cons (cons #'<= (event-off e)) e1) ad))) ; <--
				     (setf (event-off e) e1
					   (event-dur* e) (let ((bd (/ (beat-division (loop for s of-type timesig in ph until (<= (timesig-off s) e1) finally (return s))))))
							    (max bd (roundto (event-gracedur e) bd)))))
				   (let ((e2 (let ((o (event-endoff e))) (loop-return-lastmin (diff x o) for x of-type (rational 0) in qs))))
				     (aa (event-off e) e1)
				     (setf (event-off e) e1)
				     (let ((x (- e2 e1)))
				       (if (<= x 0)
					   (progn
					     (aa (event-endoff e) e1)
					     (setf (event-dur e)
						   (cons *default-grace-dur* (incf mg))))
					   (progn
					     (aa (event-endoff e) e2)
					     (setf (event-dur* e) x)))))))))
			 (print-dot)
			 finally
			 (addprop p (cons :quant ; temporary prop: collection of all movements to points
					  (merge-all ad (lambda (x y)
							  (declare (type (cons (cons (function ((rational 0) (rational 0)) boolean) (real 0)) (rational 0)) x y))
							  (let ((x1 (cdar x)) (x2 (cdr x))
								(y1 (cdar y)) (y2 (cdr y)))
							    (cond ((and (< x1 x2) (< y1 y2)) ; -->
								   (when (and (>= x2 y1) (>= y2 x1)) ; always #'>=
								     (cons (if (< x1 y1) (car x) (car y))
									   (max x2 y2))))
								  ((and (> x1 x2) (> y1 y2)) ; <-- 
								   (cond ((or (and (> x1 y2) (> y1 x2))	; overlap
									      (and (= x1 y2) (eq (caar x) #'<=)) ; touching
									      (and (= y1 x2) (eq (caar y) #'<=)))
									  (cons (cond ((= x1 y1) (cons (if (or (eq (caar x) #'<=)
													       (eq (caar y) #'<=))
													   #'<= #'<)
												       x1))
										      ((> x1 y1) (car x))
										      (t (car y)))
										(min x2 y2))))))))
						     :call-rev nil)))
			 (setf (part-events p) (sort (part-events p) #'sort-offdur)))))))))

(defun quantize (timesigs parts)
  (case (auto-quantize-fun)
    (:quantize1 (quantize-byfit timesigs parts))
    (otherwise (error "Unknown quantize function ~S" *auto-quantize-mod*))))

;; (defparameter *quantize-adjust-grace-durs* t)

;; (defun clean-quantize (parts)
;;   (when *quantize-adjust-grace-durs*
;;     (loop for p in parts do
;; 	  (loop for v in (split-into-groups (part-events p) #'event-voice*) do
;; 		(loop with d and do
;; 		      for e in (sort (copy-list v) (complement #'sort-offdur))
;; 		      if (and d (getmark e :grace) (eql (event-off e) do))
;; 		      do (setf (event-dur* e) (let for x = *default-grace-dur* then (/ x 2) until (<= x d) finally (return x)))
;; 		      else if (notep e) do (setf d (event-dur* e))
;; 		      else do (setf d nil)
;; 		      do (setf do (event-off e))))
;; 	  ;; 	      (loop with g and di = (>= *default-grace-num* 0)	; di = t if forward and default grace >= 0
;; 	  ;; 		    for e in (sort v (if di #'sort-offdur (complement #'sort-offdur)))
;; 	  ;; 		    if (popmark e :grace)
;; 	  ;; 		    do (setf (event-grace* e) (setf g (if g
;; 	  ;; 							  (if di (max (1+ g) *default-grace-num*) (min (1- g) *default-grace-num*))
;; 	  ;; 							  *default-grace-num*)))
;; 	  ;; 		    else if (event-grace e) do (setf g (event-grace e)))
;; 	  )))

(defun quantize-generic (parts)
  (loop for p in parts do
	(loop for e in (part-events p) do
	      (setf (event-dur* e) (rationalize (or (event-gracedur e) (event-dur* e))) (event-off e) (rationalize (event-off e))))))

		   #|(cons pts (list o1 o2))|# #|(cons nil nil)|#
	;; 	       (uu00 (i)
	;; 		 (declare (type cons i))
;; 		 (cons i
;; 		       (loop for (i1 i2 . i3) of-type ((integer 1) (or (integer 1) null) . list) on i while i2
;; 			     when (or i0 i3) nconc (uu (append i0 (cons (+ i1 i2) i3)))
;; 			     collect i1 into i0))) 
;; 	       (mt00 (tu o du nx #|nxs|# ep) ; du = rest of tuplet list, o = off, du = dur, nx = number of div units, nxs = list of ii's to not bother considering
;; 		 (declare (type list tu) (type (rational 0) o) (type (rational (0)) du nx) #|(type cons nxs)|# (type list ep))
;; 		 (sel
;; 		  (loop with tp = (first tu) and rt = (rest tu) and nxl = (+ nx (/ nx 2))
;; 			for d in (delete-if (lambda (x) (declare (type (integer 2) x)) (>= x nxl)) (primes2 tp)) ; possible prime tuplet divisions, delete ones too big for nx
;; 			for ii = (loop-return-lastmin (diff i nx) for i = d then (* i 2)) ; nearest *2 multiple to beat-div
;; 					#|unless (find ii nxs)|# #|(or (>= ii nxl) (find ii nxs))|# ; collect all divisionpoint-lists
;; 			collect (cons ep (loop for i from 0 to ii collect (+ o (/ (* du i) ii))))
;; 			when rt		; another nested tuplet
;; 			nconc (loop for i of-type cons in (delete-duplicates (loop for e of-type cons in (tuplet-division d) nconc (uu e)) :test #'equal) ; ... fix ; iterate through ways how to divide up tuplet 
;; 				    for z = (apply #'adj
;; 						   (loop for p = o then (+ p dd)
;; 							 and e of-type (integer 1) in i	; e = each division
;; 							 for dd = (* du (/ e d))
;; 							 for y = (mt rt p dd (/ (* nx e) d) #|(cons (/ (* ii e) d) nxs)|#
;; 								     (loop with p2 = (+ p dd) for i of-type (real 0) in ep when (and (>= i p) (<= i p2)) collect i))
;; 							 if y collect y else do (return)))
;; 				    when z collect z))))
;; 	       (dv00 (o ep dl ts) ; process divisions at offset o (dl is one element div-list from timesig)--return one best-fit selection
;; 		 (declare (type (rational 0) o) (type list ep) (type cons dl) (type timesig-repl ts))
;; 		 (if (list1p dl)	; entire measure, one du
;; 		     (sel (let ((du (first dl)))
;; 			    (nconc
;; 			     (let ((nx (* du (beat-division ts))))
;; 			       (unless (integerp nx) (error "Cannot quantize with beat division ~S and measure division ~S at offset ~S"
;; 							    *beat-division* (timesig-div* ts) (timesig-foff ts)))
;; 			       (let ((x (loop for i from o to (+ o du) by (/ (beat-division ts)) collect i))) ; collect ((list of beat-div points from 0 to span-dur) (...))
;; 				 (if (and (>= du *min-tuplet-dur*) (<= du *max-tuplet-dur*) *max-tuplet*)
;; 				     (let ((y (mt *max-tuplet* o du nx #|(list nx)|# ep)))
;; 				       (if y (list (cons ep x) y) (list (cons ep x))))
;; 				     (list (cons ep x))))) ; assuming o & du is valid division into tuplets
;; 			     (let* ((nd1 (let ((x 1))
;; 					   (loop until (> x du) do (setf x (* x 2)))
;; 					   (loop until (and (< x du) (or (>= (- du x) *min-tuplet-dur*) (< x *min-tuplet-dur*)))
;; 						 do (setf x (if (and (= x 1) (timesig-comp ts)) (* x 2/3) (/ x 2))))
;; 					   x))
;; 				    (nd2 (- du nd1)))
;; 			       (when (and (>= (min nd1 nd2) *min-tuplet-dur*) (<= (max nd1 nd2) *max-tuplet-dur*))
;; 				 (if (= nd1 nd2)
;; 				     (list
;; 				      (let ((os (+ o nd1)))
;; 					(adj (dv o (loop for i of-type (real 0) in ep when (<= i os) collect i) (list nd1) ts)
;; 					     (dv os (loop for i of-type (real 0) in ep when (>= i os) collect i) (list nd2) ts))))
;; 				     (list
;; 				      (let ((os (+ o nd1)))
;; 					(adj (dv o (loop for i of-type (real 0) in ep when (<= i os) collect i) (list nd1) ts)
;; 					     (dv os (loop for i of-type (real 0) in ep when (>= i os) collect i) (list nd2) ts)))
;; 				      (let ((os (+ o nd2)))
;; 					(adj (dv o (loop for i of-type (real 0) in ep when (<= i os) collect i) (list nd2) ts)
;; 					     (dv os (loop for i of-type (real 0) in ep when (>= i os) collect i) (list nd1) ts))))))))))
;; 		     (sel (cons
;; 			   (dv o ep (list (apply #'+ dl)) ts) ; entire measure
;; 			   (loop with e2 = dl ; at least 2 in e2
;; 				 for e of-type (rational (0)) = (pop e2)
;; 				 while e2
;; 				 for os = (+ o e) then (+ os e)
;; 				 collect e into e1 ; split into 2-parts along divisions
;; 				 collect (adj (dv o (loop for i of-type (real 0) in ep when (<= i os) collect i) e1 ts)
;; 					      (dv os (loop for i of-type (real 0) in ep when (>= i os) collect i) e2 ts))))))))
