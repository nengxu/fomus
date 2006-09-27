;; -*- lisp -*-
;;
;; TIMEDUMP--Backend Plugin
;; David Psenicka
;;
;;   A very simple backend that sends timing information (measure onsets, offsets, durations and time signatures) to a user-defined callback
;;   function for dumping timing info into a data file or generating a click track, etc..
;;
;;   The user callback function looks like this:
;;
;;     (defun my-callback (list-of-structs) ...
;;
;;   "list-of-structs" is a list of timedump structs (see the defstruct definition below).  Pass the function to TIMEDUMP with a :CALLBACK
;;   keyword argument.  Each timedump struct represents a measure of the score.
;;
;; Options:
;;   callback = user callback function
;;   partids = optional (for polymetric scores): partid or list of partids (callback is called once for each partid in list)

(deffomusplugin
    (:keyname :timedump) (:type :backend) (:entryfun do-timedump)
    (:export #:timedump)
    (:import-from #:fomus #:force-list)
    (:documentation "A simple backend that sends timing info to a user callback function (for storing timing info, creating click tracks, etc.)"))

;; timesig is given in the form of a cons
;; barline is the barline property (:double, etc.) for the measure if present (applies to the BEGINNING of the measure)
(defstruct (timedump (:copier nil) (:predicate timedumpp))
  off endoff dur timesig barline)
  
(defun do-timedump (parts filename options process view)
  (declare (ignore process view filename))
  (destructuring-bind (&key callback partids &allow-other-keys) options
    (if callback
	(loop for e in (or (force-list partids) (list (part-partid (first parts))))
	      for p = (find e parts :key #'part-partid)
	      do (funcall callback
			  (loop with bl for m in (part-meas p) 
				collect (make-timedump :off (meas-off m) :endoff (meas-endoff m)
						       :dur (- (meas-endoff m) (meas-off m)) :timesig (timesig-time (meas-timesig m))
						       :barline bl)
				do (setf bl (second (getprop m :barline))))))
	(format t ";; WARNING: Need a user callback function for timedump~%"))))
