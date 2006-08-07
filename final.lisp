;; -*- lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;**************************************************************************************************
;; FOMUS
;; final.lisp
;;**************************************************************************************************

(in-package :fomus)
(compile-settings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +initfilename+ (namestring (merge-pathnames ".fomus" (user-homedir-pathname))))

;; initfile
(defun load-initfile (&optional (filename +initfilename+) (info t))
  (with-open-file (f filename :direction :input :if-does-not-exist nil)
    (when f
      (when info (out ";; Loading initialization file ~S...~%" filename))
      (loop
       with nt0
       for x = (read f nil 'eof)
       #-clisp until #-clisp (eq x 'eof)
       for y = #-clisp (read f nil 'eof) #+clisp (if (eq x 'eof) (loop-finish) (read f nil 'eof))
       when (eq y 'eof) do (error "KEYWORD/ARGUMENT-PAIRS expected in initialization file")
       do (setf nt0 (find-symbol (conc-strings "*" (symbol-name x) "*") :fomus))
       if nt0 do (unless (ignore-errors (eval `(progn (setf ,(find-symbol (conc-strings "*" (symbol-name x) "*") :fomus) ,y) t)))
		   (format t ";; WARNING: Error setting ~S to value ~S in initialization file~%" x y))
       else do (format t ";; WARNING: Unknown setting ~S in initialization file~%" x)
       finally
       (return t)))))

(eval-when (:load-toplevel :execute)
  (export (mapcar (lambda (x) (find-symbol (conc-strings "*" (symbol-name (first x)) "*") :fomus)) +settings+) :fomus))

(eval-when (:load-toplevel :execute) (provide :fomus))

;; feature
(eval-when (:load-toplevel :execute)
  (pushnew :fomus *features*))

;; print load greeting
(eval-when (:load-toplevel :execute)
  (when (>= *verbose* 1) (format t "~&;; ~A v~A.~A.~A~%~A~%"
				 +title+
				 (first +version+) (second +version+) (third +version+)
				 (conc-stringlist (loop for e in +banner+ collect (format nil ";; ~A~%" e))))))

(eval-when (:load-toplevel :execute)
  (find-cm) (find-cmn))

(eval-when (:load-toplevel :execute)
  (load-initfile))

(defun fomus-exe (filename initfile)
  (load-initfile initfile nil)
  (catcherr (fomus-text filename nil #'fomus-textexec))
  (fresh-line)
  (finish-output)
  #+cmu (ext:quit) #+sbcl (sb-ext:quit) #+openmcl (ccl:quit))