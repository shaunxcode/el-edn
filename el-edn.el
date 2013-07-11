(defun in-chars (tc char-string)
  (let ((found nil))
    (mapcar (lambda (c)
	      (if (and (> (length c) 0) (string-equal c tc))
		  (setq found t)))
	    (split-string char-string ""))
    found))

(defun only-containing-chars (str char-string)
  (let ((found nil))
    (mapcar (lambda (c)
	      (if (and (> (length c) 0) (not (in-chars c char-string)))
		  (setq found t)))
	    (split-string str ""))
    (not found)))

(defun count-chars (str char)
  (let ((count 0))
    (mapcar (lambda (c)
	      (if (string-equal c char)
		  (setq count (+ count 1))))
	    (split-string str ""))
    count))

(defun valid-symbol (str)
  (let ((ucstr (upcase str))
        (fchar (substring str 0 1)))
    (and
     ;;can only have valid chars
     (only-containing-chars ucstr "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ.*+!-_?$%&=:#/")
     ;;cannot start with a number
     (not (in-chars fchar "0123456789"))
     ;;if first char is - + or . next must not be numeric
     (not (and (in-chars fchar "-+.")
	       (> (length ucstr) 1)
	       (in-chars (substring ucstr 1 2) "0123456789")))
     ;;only allow one slash per symbol
     (<= (count-chars str "/") 1))))

(defun valid-keyword (str)
  (and (string-equal (substring str 0 1) ":")
       (valid-symbol (substring str 1))))

(defun valid-nil (str)
  (string-equal str "nil"))

(defun valid-bool (str)
  (or (string-equal str "true")
      (string-equal str "false")))

(defun valid-int (str &optional disallow-sign)
  (progn
    (if (and (in-chars (substring str 0 1) "-+")
	     (> (length str) 1)
	     (not disallow-sign))
	(setq str (substring str 1)))
    (if (in-chars (substring str -1) "NM")
	(setq str (substring str 0 -1)))
    (only-containing-chars str "0123456789")))

(defun valid-float (str)
  (let ((front "")
	(back "")
	(e-pos 0)
	(period-pos (string-match "\\." str))
	(result t))
    (if period-pos
	(progn
	  (setq front (substring str 0 period-pos))
	  (setq back (substring str (+ 1 period-pos))))
        (setq back str))
    (if (or (= (length front) 0)
	    (valid-int front))
	(progn
	  (setq e-pos (string-match "E" back))
	  (if e-pos
	      (if (or (= e-pos (- (length back) 1))
		      (not (valid-int (substring back 0 e-pos) t))
		      (not (valid-int (substring back (+ e-pos 1)))))
		  (setq result nil))
	      (progn
		(if (in-chars (substring back -1) "M")
		    (setq back (substring back 0 -1)))
		(if (not (valid-int back t))
		    (setq result nil))))))
    result))

(defun valid-char (str)
  (and (string-equal (substring str 0 1) "\\")
       (= (length str) 2)))

(defun edn-token (type line value)
  (let ((token (make-hash-table :test 'equal)))
    (puthash 'type type token)
    (puthash 'line line token)
    (puthash 'value value token)
    token))

(defun edn-node (type line value)
  (let ((node (make-hash-table :test 'equal)))
    (puthash 'type type node)
    (puthash 'line line node)
    (puthash 'value value node)
    node))

(defun handle-atom (token)
  (let ((val (gethash 'value token)))
    (edn-node
      (cond ((valid-nil val) 'EdnNil)
	    ((eq (gethash 'type token) 'String) 'EdnString)
	    ((valid-char val) 'EdnChar)
	    ((valid-bool val) 'EdnBool)
	    ((valid-int val) 'EdnInt)
	    ((valid-float val) 'EdnFloat)
	    ((valid-keyword val) 'EdnKeyword)
	    ((valid-symbol val) 'EdnSymbol)
	    (t (error (concat "unknown type for " val))))
      (gethash 'line token)
      val)))

(defun handle-collection (token values)
  (let ((val (gethash 'value token)))
    (edn-node
      (cond ((string-equal val "(") 'EdnList)
	    ((string-equal val "[") 'EdnVector)
	    ((string-equal val "{") 'EdnMap))
      (gethash 'line token)
      values)))

(defun handle-tagged (token value)
  (let ((tag-name (substring (gethash 'value token) 1)))
    (edn-node
      (cond ((string-equal tag-name "_") 'EdnDiscard)
 	    ((string-equal tag-name "") 'EdnSet)
	    (t 'EdnTagged))
      (gethash 'line token)
      (let ((content (make-hash-table :test 'equal)))
	(puthash 'tag (edn-node 'Symbol (gethash 'line token) tag-name) content)
	(puthash 'value value content)
	content))))

(defun lex (edn-string)
  (let ((escaping nil)
        (in-string nil)
        (string-content "")
        (in-comment nil)
        (token "")
        (paren "")
	(escape-char "\\")
	(string-char "\"")
	(line 1)
        (tokens '())
	(create-token
	  (lambda (type line value)
	    (progn
	      (setq tokens
  	        (append tokens (list (edn-token type line value))))
	      (setq token "")
	      (setq string-content "")))))
    (mapcar
      (lambda (c)
	(progn
	  ;;keep track of line
	  (if (or (string-equal c "\n")
		  (string-equal c "\r"))
	      (setq line (+ line 1)))
	  (cond
            ((string-equal c "") nil)
	    ;;comments
	    (in-comment
	     (if (string-equal c "\n")
		 (progn
		   (setq in-comment nil)
		   (if (> (length token) 0)
		       (funcall create-token 'Atom line token)))))
	     ;;strings
	     ((and (string-equal c string-char) (not escaping))
	      (if in-string
		  (funcall create-token 'String line string-content)
		  (setq in-string t)))
	     ;;build-string
	     (in-string
	      (if (and (string-equal c escape-char) (not escaping))
		  (setq escaping t)
		  (if escaping
		      (progn
			(setq escaping nil)
			(if (in-chars c "tnfr")
			    (setq string-content (append string-content escape-char)))
			(setq string-content (append string-content c))))))
	     ;;paren or whitespace
	     ((in-chars c "()[]{}\t\n\r ,")
	      (progn
	        (if (> (length token) 0)
	  	    (funcall create-token 'Atom line token))
		(if (in-chars c "()[]{}")
		    (funcall create-token 'Paren line c))))
	     (t (progn
		  (if escaping
		      (setq escaping nil)
		      (if (string-equal c escape-char)
			  (setq escaping t)))
		  (if (or (string-equal token "#_")
			  (and (= (length token) 2)
			       (string-equal (substring token 0 1) escape-char)))
		      (funcall create-token 'Atom line token))
	          (setq token (concat token c)))))))
       (split-string edn-string ""))
    (if (> (length token) 0)
	(funcall create-token 'Atom line token))
    tokens))

(defun edn-read (edn-string1)
  (let ((tokens (lex edn-string1))
	(shift-token
	 (lambda ()
	   (let ((token (car tokens)))
	     (setq tokens (cdr tokens))
	     token)))
	(read-ahead
	 (lambda (token)
	   (let ((type (gethash 'type token))
		 (val (gethash 'value token)))
	     (cond
	      ((eq type 'Paren)
	       (let ((L '())
		     (close-paren (cond ((string-equal val "(") ")")
			      	        ((string-equal val "[") "]")
					((string-equal val "{") "}")))
		     (next-token nil))
		 (catch 'break
		   (while tokens
		     (setq next-token (funcall shift-token))
		     (if (string-equal (gethash 'value next-token) close-paren)
			 (throw 'break (handle-collection token L))
		         (setq L (append L (list (funcall read-ahead next-token))))))
		    (error "Unexpected end of list"))))
	      ((in-chars val ")]}")
	       (progn
		 (print (list token tokens))
		 (error "Unexpected closing paren")))
	      ((string-equal (substring val 0 1) "#")
	       (handle-tagged token (funcall read-ahead (funcall shift-token))))
	      (t (handle-atom token)))))))
    (funcall read-ahead (funcall shift-token))))

(defun edn-pprint (node)
  (let ((type (gethash 'type node))
        (value (gethash 'value node)))
    (cond ((member type '(EdnList EdnSet EdnVector EdnMap))
           (let ((vals (join-string (mapcar 'edn-pprint value))))
             (cond ((eq type 'EdnList) (concat "(" vals ")"))
                   ((eq type 'EdnVector) (concat "[" vals "]"))
                   ((eq type 'EdnMap) (concat "{" vals "}")))))
          ((eq type 'EdnTagged)
           (concat "#"
                   (edn-pprint (gethash 'tag value))
                   (edn-pprint (gethash 'value value))))
          (t value))))


(edn-pprint (edn-read "#people (33 33.33E-1)"))
