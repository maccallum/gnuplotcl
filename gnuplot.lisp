(in-package #:gnuplot)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *gp-command* "/usr/local/bin/gnuplot")
  (defvar *gp-options* '("-persist"))
  (defvar *gp-term-commands* '((x11 "set term x11")
				    (eps "set term postscript eps color enhanced rounded font \"Times\" 12")
			       	    (eps-tabloid-landscape "set term postscript eps size 17,11 color enhanced rounded font \"Times\" 12")
				    (png "set term png")))
  (defparameter *gp-proc* nil)
  (defparameter *gp-external-methods* '())
  (defparameter *gp-internal-methods* '()))

(defun run-program (cmd args)
  (let ((full-cmd (apply 'concatenate
			 (butlast
			  (cons 'string
				(mapcan (lambda (st) (list st " "))
					(cons cmd args)))))))
    (declare (ignore full-command))
    #+sbcl (sb-ext:run-program cmd args :wait nil :input :stream :output :stream :error :output)
    #+lispworks (system:run-shell-command full-cmd :wait nil :input :stream :output :stream :error-output :output)))

(defun process-close (proc)
  #+sbcl (sb-ext:process-close proc)
  #+lispworks (process-kill proc))

(defun process-input (proc)
  #+sbcl (sb-ext:process-input proc)
  #+lispworks proc)

(defun process-p (proc)
  #+sbcl (sb-ext:process-p proc)
  #+lispworks (if (null proc) nil (and (input-stream-p proc) (open-stream-p proc))))

(defun process-output (proc)
  #+sbcl (sb-ext:process-output proc)
  #+lispworks proc)

(defun process-wait (proc)
  #+sbcl (sb-ext:process-wait proc)
  #+lispworks proc)

(defun process-kill (proc)
  #+sbcl (sb-ext:process-kill proc sb-posix:sigterm)
  #+lispworks (progn (princ "quit" proc) (terpri proc) (finish-output proc) (setf *stream* nil)))

(defun process-status (proc)
  #+sbcl (sb-ext:process-status proc)
  #+lispworks (if (open-stream-p proc) :running :exited))

(defun list-gp-functions ()
  (mapcar #'(lambda (m) (print (car m))) *gp-external-methods*)
  (mapcar #'(lambda (m) (print m)) *gp-internal-methods*)
  nil)

(defmacro defun* (name args &rest body)
  "Just like defun except it has two implicit variables called current-function-name
and current-function-args"
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (defun ,name ,args
	 (let ((current-function-name ',name)
	       (current-function-args ',args))
	   (declare (ignorable current-function-name))
	   (declare (ignorable current-function-args))
	   ,@body)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun add-internal-method-to-list (name)
    "Add an internal method (ie one defined in the gnuplot object) to a list of internal methods"
    (push name *gp-internal-methods*)
    (delete-duplicates *gp-internal-methods*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun add-external-method-to-list (name func)
    "Add an external method to the list or replace the old lambda with a new one if it already exists"
    (let ((entry (assoc name *gp-external-methods*)))
      (if (not (null entry))
	  (setf (cdr entry) func)
	  (push (cons name func) *gp-external-methods*)))))

(defmacro gp (proc func &rest args)
  "The main interface to gnuplot.  All functions call this to interface with the gnuplot process."
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((external-method (assoc ,func *gp-external-methods*)))
	 (if (not (null external-method))
	     (funcall (cdr external-method) ,proc ,@args)
	     (funcall ,proc ,func ,@args))))))

(defmacro def-gp-method (name args docstring &rest body)
  "(def-gp-method foo (arg1 arg2) \"docstring\" (do some stuff...)) 
this will define a function called foo that will look like this: 
	(progn 
  		(gp *gp-proc* 'foo arg1 arg2) 
		(do some stuff...))"
  `(progn
     (eval-when (:compile-toplevel :load-toplevel)
       (add-internal-method-to-list ',name)
       (defun* ,name ,args ,docstring
	       (progn
		 (when (eq *gp-proc* nil)
		   (make-gp))
		 (gp *gp-proc* current-function-name ,@args)
		 ,@body)))))

(defmacro def-gp-ext-method (name proc args docstring &rest body)
  "(def-gp-ext-method foo proc (arg1 arg2) \"docstring\" (do some stuff))
this will define a function bound to the symbol foo that can be called like this:
(foo arg1 arg2)"
  `(progn
     (eval-when (:compile-toplevel :load-toplevel)
       (add-external-method-to-list ',name (lambda (,proc ,@args) ,@body))
       (let ((func-name ',name))
         (defmacro ,name (&rest args) ,docstring
		   `(progn
		      (when (eq *gp-proc* nil)
		        (make-gp))
		      (gp *gp-proc* ',func-name ,@args)))))))

(defmacro expand-args (func arg-list)
  `(,@func ,@arg-list))

(def-gp-method start-gp () "Start the gnuplot process")
(def-gp-method quit-gp () "Quit the gnuplot process")
(def-gp-method get-input-stream () "Return the input stream of the gnuplot process")
(def-gp-method get-output-stream () "Return the output stream of the gnuplot process")
(def-gp-method get-status () "Return the status of the gnuplot process")
(def-gp-method gp-new-window () "Create a new X11 window")
(def-gp-method gp-get-term-type() "Get the current term type")
(def-gp-method gp-set-term-type(term-type) "Change the term type.  Term type must be associated with a \"set term ...\" string in *gp-term-types*")
(def-gp-method gp-set-output-file(output-file) "Set the output file.")
(def-gp-method gp-read () "Read from the gnuplot output stream")
(def-gp-method gp-princ (command) "Send input to the gnuplot process using princ")
(def-gp-method gp-terpri () "Send a newline to the gnuplot process")
(def-gp-method gp-finish-output () "Flush the input stream to the gnuplot process")


(def-gp-ext-method is-gp-running? proc ()
			"t or nil if the gnuplot process is running or not"
			(let ((status (gp proc 'get-status)))
			  (cond ((eql status :running) t)
				(t nil))))

(def-gp-ext-method restart-gp proc ()
			"Restart the gnuplot process"
			(gp proc 'quit-gp)
			(gp proc 'start-gp))

(def-gp-ext-method gp-flush proc ()
			"Sends a newline and finish-output command to the gnuplot process and then
reads from its output stream"
			(gp proc 'gp-terpri)
			(gp proc 'gp-finish-output)
			(gp proc 'gp-read))

(def-gp-ext-method gp-command proc (command)
			"Send a command to the gnuplot process.

Example:

(gp-command (format nil \"set xrange [~A:~A]\" min max))"
			(gp proc 'gp-princ command)
			(gp-flush))

(def-gp-ext-method lplot proc (data)
			"Send data to gnuplot to be plotted.  Similar to the plot function
except that the arguments should all be in one list of lists.  See plot for more info."
			(when (eq (is-gp-running?) nil)
			  (start-gp))
			(labels ((rec (data acc comma)
				   (if (null data)
				       (reverse acc)
				       (progn
					 (if comma
					     (gp proc 'gp-princ ", '-' ")
					     (gp proc 'gp-princ "'-' "))
					 (cond
					   ((null (second data))
					    (reverse (cons (list (car data)) acc)))
					   ((listp (second data))
					    ;; second thing is a list--must be y vals
					    (if (stringp (third data))
						(progn
						  (gp proc 'gp-princ (third data))
						  (rec (cdddr data)
						       (cons (list (first data) (second data)) acc)
						       t))
						(rec (cddr data)
						     (cons (list (first data) (second data)) acc)
						     t)))
					   ((stringp (second data))
					    (gp proc 'gp-princ (second data))
					    (rec (cddr data) (cons (list (first data)) acc) t))
					   (t (error (format nil "Unrecognized argument after list: ~a" (second data)))))))))
			  (gp proc 'gp-princ "plot ")
			  (let ((data (rec data '() nil)))
			    (gp proc 'gp-flush)
			    (mapcar #'(lambda (data)
					(let ((data (apply #'mapcar (cons #'list data))))
					  (mapcar #'(lambda (&rest lambda-args)
						      (mapcar #'(lambda (val)
								  (gp proc 'gp-princ (format nil "~{~A ~}" val))
								  (gp proc 'gp-flush))
							      lambda-args))
						  data))
					(gp proc 'gp-princ "e")
					(gp proc 'gp-flush))
				    data))
			  (gp proc 'gp-flush)))

(def-gp-ext-method plot proc (&rest data)
			"Send data to gnuplot to be plotted.  
Examples:

	(plot '(0 1 2)) ; y values only
	(plot '(0 1 2) (3 4 5)) ; x and y values
	(plot '(0 1 2) (3 4 5)) \"with lines\")) ; x and y values with a formatting command
	(plot '(0 1 2) \"with lines\")) ; y values with a formatting command
	(plot '(0 1 2) (3 4 5) \"with lines\" (0 1 2) (3 4 5) \"with impulses\") ; 2 plots"
			(gp proc 'lplot data))

;; returns a closure containing a gnuplot process and all
;; methods necessary to communicate with it
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-gp (&key
			 (gp-command *gp-command*)
			 (gp-options *gp-options*)
			 (gp-term 'x11)
			 (output-file))
    (let ((proc nil)
	  (term gp-term)
	  (x11-wincount 0)
	  (output-file output-file))
      (let ((func (lambda (selector &rest args)
		    (macrolet ((vtab (selector)
				 `(cond
				    ;; start-gp
				    ((eql 'start-gp ,selector)
				     (if (and (process-p proc) (eql (process-status proc) :running))
					 (lambda ())
					 (lambda ()
					   (setf proc
						 (run-program gp-command
							      gp-options))
					   (force-output (process-input proc)))))
				    ;; quit-gp
				    ((eql 'quit-gp ,selector)
				     (lambda ()
				       (when (process-p proc)
					 (when (eql (process-status proc) :running)
					   (process-kill proc)
					   (process-wait proc)
					   (process-close proc)					   
					   (setf proc nil)
					   (setf x11-wincount 0)
					   nil))))
				    ;; get-input-stream
				    ((eql 'get-input-stream ,selector)
				     (lambda ()
				       (when (eq proc nil)
					 (start-gp))
				       (process-input proc)))
				    ;; get-output-stream
				    ((eql 'get-output-stream ,selector)
				     (lambda ()
				       (when (eq proc nil)
					 (start-gp))
				       (process-output proc)))
				    ;; get-status
				    ((eql 'get-status ,selector)
				     (if (process-p proc)
					 (lambda () (process-status proc))
					 (lambda () nil)))
				    ;; new x11 window
				    ((eql 'gp-new-window ,selector)
				     (lambda ()
				       (when (eq proc nil)
					 (start-gp))
				       (when (not (eql 'x11 term))
				       	 (cerror "Set term type to X11."
				       		 "Term type is ~A"
				       		 term)
				       	 (setf term 'x11))
				       (gp-command (format nil "~A ~D persist"
				       			     (cadr (assoc 'x11 *gp-term-commands*))
				       			     (incf x11-wincount)))))
				    ;; get-term-type
				    ((eql 'gp-get-term-type ,selector)
				     (lambda ()
				       (when (eq proc nil)
					 (start-gp))
				       term))
				    ;; set-term-type
				    ((eql 'gp-set-term-type ,selector)
				     (lambda (term-type)
				       (when (eq proc nil)
					 (start-gp))
				       (unless (assoc term-type *gp-term-commands*)
					 (cerror "Leave term type unchanged"
						 "Unrecognized term type ~A."
						 term-type))
				       (setf term term-type)
				       (gp-command (cadr (assoc term-type *gp-term-commands*)))))
				    ;; set output file
				    ((eql 'gp-set-output-file ,selector)
				     (lambda (of)
				       (when (eq proc nil)
					 (start-gp))
				       (when (eql 'x11 term)
					 (cerror "Change term type to EPS."
						 "Term type is set to X11.")
					 (setf term 'eps)
					 (gp-command (cadr (assoc term *gp-term-commands*))))
				       (setf output-file of)
				       (gp-command (format nil "set output \"~A\"" output-file))))
				    ;; read
				    ((eql 'gp-read ,selector)
				     (if (eq proc nil)
					 (lambda () (start-gp))
					 (lambda ()
					   ;; (sleep .01)
					   (let ((output-stream (process-output proc)))
					     (when (listen output-stream)
					       (labels ((rec (acc depth)
							  (let ((r (princ (read-line output-stream nil 'eof))))
							    (terpri)
							    (if (and (listen output-stream) (not (eq r 'eof)))
								(rec (cons r acc) (1+ depth))
								nil))))
						 ;;(reverse acc)))))
						 (rec '() 0)))))))
				    ;; gp-princ
				    ((eql 'gp-princ ,selector)
				     (lambda (command)
				       (when (eq proc nil)
					 (start-gp))
				       (princ command (process-input proc))))
				    ;; gp-terpri
				    ((eql 'gp-terpri ,selector)
				     (lambda ()
				       (when (eq proc nil)
					 (start-gp))
				       (terpri (process-input proc))))
				    ;; gp-finish-output
				    ((eql 'gp-finish-output ,selector)
				     (lambda ()
				       (when (eq proc nil)
					 (start-gp))
				       (finish-output (process-input proc))))
				    ;; default
				    (t (error (format nil "gnuplot: Unrecognized selector \"~a\"" ,selector))))))
		      (apply (vtab selector) args)))))
	(apply func '(start-gp))
	(setf *gp-proc* func)
	func))))

