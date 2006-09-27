;; -*- lisp -*-

;; TIMEDUMP--Backend Plugin
;; David Psenicka

;;   A very simple backend that sends timing information (measure onsets, offsets, durations and time signatures) to a user-defined callback
;;   function for dumping timing info into a data file or generating a click track, etc..

;;   The user callback function looks like this:

;;     (defun my-callback (list-of-structs filename) ...

;;   "list-of-structs" is a list of timedump structs (see the defstruct definition below).  Pass the function to TIMEDUMP with a :CALLBACK
;;   keyword argument.  Each timedump struct represents a measure of the score.
;;   "filename" is the base filename (w/o extension), given as a string

;; Options:
;;   callback = user callback function
;;   partids = optional (for polymetric scores): partid or list of partids (callback is called once for each partid in list)


;; Example:

;; (use-package :fomus-timedump)

;; (defun savetimes (list filename)
;;   (format t ";; Dumping measure downbeats to ~S~%" (concatenate 'string filename ".txt"))
;;   (with-open-file (f (concatenate 'string filename ".txt") :direction :output :if-exists :supersede)
;;     (loop for e in (cons 0 (mapcar #'timedump-endoff list)) do (format f "~A;~%" (float e)))))

;; (fomus "/home/david/code/projects/perc/score1.fms" :filename "/home/david/code/projects/perc/score1" :output (list :timedump :callback #'savetimes))


(deffomusplugin
    (:keyname :timedump) (:type :backend) (:entryfun do-timedump)
    (:export #:timedump #:timedump-off #:timedump-endoff #:timedump-dur #:timedump-timesig #:timedump-barline)
    (:import-from #:fomus #:force-list)
    (:documentation "A simple backend that sends time info to a user callback function (for storing time info, creating click tracks, etc.--see timedump.lisp for info)"))

;; timesig is given in the form of a cons
;; barline is the barline property (:double, etc.) for the measure if present (applies to the BEGINNING of the measure)
(defstruct (timedump (:copier nil) (:predicate timedumpp))
  off endoff dur timesig barline)
  
(defun do-timedump (parts filename options process view)
  (declare (ignore process view))
  (destructuring-bind (&key callback partids &allow-other-keys) options
    (if callback
	(loop for e in (or (force-list partids) (list (part-partid (first parts))))
	      for p = (find e parts :key #'part-partid)
	      do (funcall callback
			  (loop with bl for m in (part-meas p) 
				collect (make-timedump :off (meas-off m) :endoff (meas-endoff m)
						       :dur (- (meas-endoff m) (meas-off m)) :timesig (timesig-time (meas-timesig m))
						       :barline bl)
				do (setf bl (second (getprop m :barline))))
			  filename))
	(format t ";; WARNING: Need a user callback function for timedump~%"))))
