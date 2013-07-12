;;; el-edn.el --- EDN data format goodness

;; Copyright (C) 2013 Shaun Gilchrist

;; Author: Shaun Gilchrist <shaunxcode@gmail.com>
;; Maintainer: Shaun Gilchrist <shaunxcode@gmail.com>
;; Created: 11 Jul 2013
;; Keywords: EDN, Datomic, Clojure
;; Version: 0.0.1

(defun in-chars (tc char-string)
  (let ((found nil))
    (mapc (lambda (c)
	    (if (and (> (length c) 0) (string-equal c tc))
	        (setq found t)))
	  (split-string char-string ""))
    found))

(defun only-containing-chars (str char-string)
  (let ((found nil))
    (mapc (lambda (c)
	    (if (and (> (length c) 0) (not (in-chars c char-string)))
	        (setq found t)))
	  (split-string str ""))
    (not found)))

(defun count-chars (str char)
  (let ((count 0))
    (mapc (lambda (c)
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
        (tokens '()))
    (cl-labels
      ((create-token (type line value)
         (progn
	   (setq tokens
  	     (append tokens (list (edn-token type line value))))
	   (setq token "")
	   (setq string-content ""))))
      (mapc
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
		       (create-token 'Atom line token)))))
	     ;;strings
	     ((and (string-equal c string-char) (not escaping))
	      (if in-string
		  (progn
                    (setq in-string nil)
                    (create-token 'String line string-content))
		  (setq in-string t)))
	     ;;build-string
	     (in-string
	      (if (and (string-equal c escape-char) (not escaping))
		  (setq escaping t)
                  (progn
                    (if escaping
		      (progn
			(setq escaping nil)
			(if (in-chars c "tnfr")
			    (setq string-content (concat string-content escape-char)))))
		    (setq string-content (concat string-content c)))))
	     ;;paren or whitespace
	     ((in-chars c "()[]{}\t\n\r ,")
	      (progn
	        (if (> (length token) 0)
	  	    (create-token 'Atom line token))
		(if (in-chars c "()[]{}")
		    (create-token 'Paren line c))))
	     (t (progn
		  (if escaping
		      (setq escaping nil)
		      (if (string-equal c escape-char)
			  (setq escaping t)))
		  (if (or (string-equal token "#_")
			  (and (= (length token) 2)
			       (string-equal (substring token 0 1) escape-char)))
		      (create-token 'Atom line token))
	          (setq token (concat token c)))))))
       (split-string edn-string ""))
    (if (> (length token) 0)
	(create-token 'Atom line token))
    tokens)))

(defun edn-read (edn-string1)
  (let ((tokens (lex edn-string1)))
       (cl-labels
	 ((read-ahead (token)
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
                      (setq next-token (pop tokens))
                      (if (string-equal (gethash 'value next-token) close-paren)
                          (throw 'break (handle-collection token L))
                          (setq L (append L (list (read-ahead next-token))))))
		    (error "Unexpected end of list"))))
               ((in-chars val ")]}")
                (progn
                  (print (list token tokens))
                  (error "Unexpected closing paren")))
               ((and (> (length val) 0)
                     (string-equal (substring val 0 1) "#"))
                (handle-tagged token (read-ahead (pop tokens))))
               (t (handle-atom token))))))
         (read-ahead (pop tokens)))))

(defun node-is-collection (node)
  (member (gethash 'type node)
          '(EdnList EdnSet EdnVector EdnMap)))

(defun edn-pprint (node)
  (let ((type (gethash 'type node))
        (value (gethash 'value node)))
    (cond ((node-is-collection node)
           (let ((vals (join-string (mapcar 'edn-pprint value))))
             (cond ((eq type 'EdnList) (concat "(" vals ")"))
                   ((eq type 'EdnVector) (concat "[" vals "]"))
                   ((eq type 'EdnMap) (concat "{" vals "}")))))
          ((eq type 'EdnString)
           (concat "\"" value "\""))
          ((eq type 'EdnTagged)
           (concat "#"
                   (edn-pprint (gethash 'tag value)) " "
                   (edn-pprint (gethash 'value value))))
          (t value))))

;;memoized keywords
(setq edn-kws (make-hash-table :test 'equal))
(defun edn-kw (kw)
  (progn
    (when (not (gethash kw edn-kws))
      (puthash kw (make-symbol kw) edn-kws))
    (gethash kw edn-kws)))

;;memoized symbols
(setq edn-syms (make-hash-table :test 'equal))
(defun edn-sym (sym)
  (progn
    (when (not (gethash sym edn-syms))
      (puthash sym (make-symbol sym) edn-syms))
    (gethash sym edn-syms)))

;;handlers for reification
(setq edn-reify-handlers (make-hash-table :test 'equal))
(defun set-reify-handler (type handler)
  (puthash type handler edn-reify-handlers))

(set-reify-handler 'EdnInt (lambda (val) (string-to-number val)))
(set-reify-handler 'EdnFloat (lambda (val) (string-to-number val)))
(set-reify-handler 'EdnChar (lambda (val) (substring val 1)))
(set-reify-handler 'EdnString (lambda (val) val))
(set-reify-handler 'EdnSymbol (lambda (val) (edn-sym val)))
(set-reify-handler 'EdnKeyword (lambda (val) (edn-kw val)))
(set-reify-handler 'EdnNil (lambda () nil))
(set-reify-handler 'EdnBool (lambda (val) (string= val "true")))
(set-reify-handler 'EdnList (lambda (vals) vals))
(set-reify-handler 'EdnVector (lambda (vals) (coerce vals 'vector)))
(set-reify-handler 'EdnSet (lambda (vals) vals))
(set-reify-handler 'EdnMap
                   (lambda (vals)
                     (let ((M (make-hash-table :test 'equal)))
                       (while (> (length vals) 0)
                         (puthash (pop vals) (pop vals) M))
                       M)))

(setq edn-tag-handlers (make-hash-table :test 'equal))

(defun edn-reify (node)
  (let* ((type (gethash 'type node))
         (value (gethash 'value node)))
    (when (node-is-collection node)
      (setq value (mapcar 'edn-reify value)))
    (print (list "REIFY" type value))
    (funcall (gethash type edn-reify-handlers) value)))
