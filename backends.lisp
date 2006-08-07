;; -*- lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;**************************************************************************************************
;; FOMUS
;; backends.lisp
;;**************************************************************************************************

(in-package :fomus)
(compile-settings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+sbcl (eval-when (:compile-toplevel :load-toplevel :execute) (require :sb-posix))

(declaim (type cons +backendexts+))
(defparameter +backendexts+
  '((:data . "fms") (:fomus . "fms") (:raw . "fmr")
    #-fomus-nocmn (:cmn . "cmn") #-fomus-nolilypond (:lilypond . "ly")
    #-fomus-nomusicxml (:musicxml . "xml") #-fomus-nomusicxml (:musicxml-sibelius . "xml") #-fomus-nomusicxml (:musicxml-finale . "xml")
    #-fomus-nomidi (:midi . "mid")))

(declaim (type (or symbol list) *backend* *output*))
(defparameter *backend* nil)
(defparameter *output* (list (first (first +backendexts+))))
(defparameter *filename* (namestring (merge-pathnames "fomus" +tmp-path+)))

(defun save-raw (filename parts)
  (declare (type list parts))
  (when (>= *verbose* 1) (out ";; Saving raw output file ~S...~%" filename))
  (with-open-file (f filename :direction :output :if-exists :supersede)
    (format f ";; -*-lisp-*-~%;; ~A v~A.~A.~A Raw Output File~%~%" +title+ (first +version+) (second +version+) (third +version+))
    (prin1 +version+ f)
    (fresh-line f)
    (prin1 parts f)
    (fresh-line f)))

(defun backend (backend filename dir parts options process play view)
  (declare
   (ignorable options process play view)
   (type symbol backend) (type list parts) (type list options) (type boolean process) (type boolean view))
  (unwind-protect
       (case backend
	 ((:data :fomus))
	 ((:raw) (save-raw filename parts))
	 #-fomus-nocmn
	 (:cmn (save-cmn parts (format nil +cmn-comment+ +title+ (first +version+) (second +version+) (third +version+)) filename options process view))
	 #-fomus-nolilypond
	 (:lilypond (save-lilypond parts (format nil +lilypond-comment+ +title+ (first +version+) (second +version+) (third +version+)) filename options process view))
	 #-fomus-nomusicxml
	 (:musicxml (save-xml parts (format nil +xml-comment+ +title+ (first +version+) (second +version+) (third +version+)) filename options))
	 #-fomus-nomusicxml
	 (:musicxml-sibelius (save-xmlsibelius parts (format nil +xml-comment+ +title+ (first +version+) (second +version+) (third +version+)) filename options))
	 #-fomus-nomusicxml
	 (:musicxml-finale (save-xmlfinale parts (format nil +xml-comment+ +title+ (first +version+) (second +version+) (third +version+)) filename options))
	 #-fomus-nomidi (:midi (save-midi parts filename options play))
	 (otherwise (error "Invalid backend ~S" backend)))
    (#+cmu unix:unix-chdir #+sbcl sb-posix:chdir #+openmcl ccl:cwd #+allegro excl:chdir #+lispworks hcl:change-directory #+clisp ext:cd (namestring dir))))

