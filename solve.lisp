#!/usr/bin/sbcl --script 
(require "asdf")
(require "cl-ppcre")
(require "sb-posix")
(defun string+ (&rest strs) 
  (with-output-to-string (so) 
	(dolist (str strs) 
	  (format so "~A" str))))
(defun sym+ (&rest strs) 
  (read-from-string
	(apply 'string+ strs)))
(defparameter *path* 
  (list 
	"/bin/" 
	"/usr/bin/"
	"/usr/local/bin/"
	))
(defun exec (output &rest l1)
  (let ((r (list)) (name (car l1)) (args (cdr l1)))
	(dolist (path *path*)
	  (push 
		(if (probe-file name) 
		  name 
		  (probe-file (merge-pathnames path name))) 
		r))
	(let ((paths (eval (push 'or r))))
	  (if output
		(sb-ext:run-program paths args :output output)
		(sb-ext:run-program paths args)))))
(defmacro with-fp-w ((s file) &body body)
  `(with-open-file (,s ,file
		      :direction :output
		      :if-does-not-exist :create
		      :if-exists :supersede)
    ,@body))
(defmacro with-bfp-w ((s file) &body body)
  `(with-open-file (,s ,file
		      :direction :output
		      :if-does-not-exist :create
		      :if-exists :supersede
			  :element-type '(unsigned-byte 8)
			  )
    ,@body))
(defmacro with-fp-w+ ((s file) &body body)
  `(with-open-file (,s ,file
		      :direction :output
		      :if-does-not-exist :create
		      :if-exists :append)
    ,@body))
(defmacro with-bfp-w+ ((s file) &body body)
  `(with-open-file (,s ,file
		      :direction :output
		      :if-does-not-exist :create
		      :if-exists :append
			  :element-type '(unsigned-byte 8)
			  )
    ,@body))
(defmacro with-fp-r ((s file) &body body)
  `(with-open-file (,s ,file
		      :direction :input)
    ,@body))
(defun pwd () (sb-posix:getcwd))
(defun cd (path) (sb-posix:chdir path))
(defun pid () (sb-posix:getpid))
(defmacro argv () `sb-ext:*posix-argv*)
(defmacro pscan (e s) `(cl-ppcre:scan-to-strings ,e ,s))
(defun pscan-p (e s) 
  (if (pscan 
		(if (stringp e) (string-upcase e) (string-upcase (string+ e))) 
		(if (stringp s) (string-upcase s) (string-upcase (string+ s))) 
		) 
	t nil))
(defmacro pmatch (e s) `(cl-ppcre:all-matches-as-strings ,e ,s))
(defmacro preplace (e s w) `(cl-ppcre:regex-replace-all ,e ,s ,w))
(defmacro psplit (e s) `(cl-ppcre:split ,e ,s))
(defmacro pquote (b) `(cl-ppcre:quote-meta-chars ,b))
(defparameter *musk-keys*
  (psplit "\\s+" 
		  "KDE gtk3"
		  ))
(defparameter *digested* (list))
(defun digest (uri)
  (format t "digest ~A~%" uri)
  (with-fp-w+
	(f "solve.sh")
  (format f "echo -e \"\\033]0;~A\\007\"~%" uri)
	(let ((h (with-output-to-string (s) (exec s "curl" uri)))
		  (record nil))
	  (with-input-from-string (s h)
		(do ((line (read-line s nil 'end) (read-line s nil 'end)))
		  ((equalp line 'end))
		  (if record
			(if (pscan"</pre>" line)
			  (setf record nil)
			  (format f "~A~%" line)
			  )
			(if (pscan "<pre id='(fetch|install|clean)'>" line)
			  (setf record t)
			  (if (and
					(pscan "<a id='depend'" line)
					(not(member
						  (string-trim '(#\< #\>) (pscan ">[^<,>]+<" line)) 
						  *musk-keys* 
						  :test 'equalp
						  ))
					)
				(let ((uri1 (string+
							  (car(psplit "solution" uri))
							  (nth 1 (psplit "[']+" (pscan "href='[^']+'" line)))
							  )))
				  (if (not(member uri1 *digested* :test 'equalp)) (digest uri1))))))
		  ))))
  (push uri *digested*)
  )
(let ((argvs (argv))
	  (uri "")
	  (jobs nil))
  (dotimes (k (length argvs))
	(cond
	  ((equalp (nth k argvs) "--help")
	   (let ()
		 (format t "--uri <solution html uri>~%")
		 (format t "--jobs <make -j>~%")
		 (format t "--digest~%")
		 (format t "--run~%")
		 ))
	  ((and (equalp (nth k argvs) "--uri") (nth (+ 1 k) argvs))
	   (setf uri (nth (+ 1 k) argvs)))
	  ((and (equalp (nth k argvs) "--jobs") (nth (+ 1 k) argvs))
	   (setf jobs (nth (+ 1 k) argvs)))
	  ((and (equalp (nth k argvs) "--digest") (pscan "\\S+" uri))
	   (let ()
		 (with-fp-w (f "solve.sh") (format f "export JOBS=-j~A~%" (if jobs jobs 0)))
		 (setf *digested* (list))
		 (digest uri)))
	  ((and (equalp (nth k argvs) "--run") (pscan "\\S+" uri) (probe-file "solve.sh"))
	   (exec t "bash" "solve.sh"))
	  )))
