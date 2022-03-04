;;; ids-find.el --- search utility based on Ideographic-structures ;; -*- coding: utf-8-mcs-er -*-

;; Copyright (C) 2002, 2003, 2005, 2006, 2007, 2017, 2020, 2021
;;   MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <tomo@kanji.zinbun.kyoto-u.ac.jp>
;; Keywords: Kanji, Ideographs, search, IDS, CHISE, UCS, Unicode

;; This file is a part of CHISE-IDS.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(defun ids-index-store-char (product component)
  (let ((ret (get-char-attribute component 'ideographic-products)))
    (unless (memq product ret)
      (put-char-attribute component 'ideographic-products
			  (cons product ret))
      (when (setq ret (char-feature component 'ideographic-structure))
	(ids-index-store-structure product ret)))
    ))

(defun ids-index-store-structure (product structure)
  (let (ret)
    (dolist (cell (cdr structure))
      (if (char-ref-p cell)
	  (setq cell (plist-get cell :char)))
      (cond ((characterp cell)
	     (ids-index-store-char product cell))
	    ((setq ret (assq 'ideographic-structure cell))
	     (ids-index-store-structure product (cdr ret)))
	    ((setq ret (find-char cell))
	     (ids-index-store-char product ret))
	    ))))

;;;###autoload
(defun ids-update-index (&optional in-memory)
  (interactive)
  (map-char-attribute
   (lambda (c v)
     (ids-index-store-structure c v)
     nil)
   'ideographic-structure)
  (map-char-attribute
   (lambda (c v)
     (ids-index-store-structure c v)
     nil)
   'ideographic-structure@apparent)
  (map-char-attribute
   (lambda (c v)
     (ids-index-store-structure c v)
     nil)
   'ideographic-structure@apparent/leftmost)
  (unless in-memory
    (save-char-attribute-table 'ideographic-products)))


(mount-char-attribute-table 'ideographic-products)

;;;###autoload
(defun ids-find-all-products (char)
  (let (dest)
    (dolist (cell (char-feature char 'ideographic-products))
      (unless (memq cell dest)
	(setq dest (cons cell dest)))
      (setq dest (union dest (ids-find-all-products cell))))
    dest))

(defun of-component-features ()
  (let (dest)
    (dolist (feature (char-attribute-list))
      (when (string-match "^<-.*[@/]component\\(/[^*/]+\\)*$"
			  (symbol-name feature))
	(push feature dest)))
    (list* '<-mistakable '->mistakable
	   '<-formed '->formed
	   '<-same '->same
	   '<-original '->original
	   '<-ancient '->ancient
	   dest)))

(defun to-component-features ()
  (let (dest)
    (dolist (feature (char-attribute-list))
      (when (string-match "^->.*[@/]component\\(/[^*/]+\\)*$"
			  (symbol-name feature))
	(push feature dest)))
    dest))

;;;###autoload
(defun char-component-variants (char)
  (let ((dest (list char))
	ret uchr)
    (dolist (feature (to-component-features))
      (if (setq ret (get-char-attribute char feature))
	  (dolist (c ret)
	    (setq dest (union dest (char-component-variants c))))))
    (cond
     ;; ((setq ret (some (lambda (feature)
     ;;                    (get-char-attribute char feature))
     ;;                  (to-component-features)))
     ;;  (dolist (c ret)
     ;;    (setq dest (union dest (char-component-variants c))))
     ;;  )
     ((setq ret (get-char-attribute char '->ucs-unified))
      (setq dest (cons char ret))
      (dolist (c dest)
	(setq dest (union dest
                          (some (lambda (feature)
				  (get-char-attribute c feature))
				(of-component-features))
			  )))
      )
     ((and (setq ret (get-char-attribute char '=>ucs))
	   (setq uchr (decode-char '=ucs ret)))
      (setq dest (cons uchr (char-variants uchr)))
      (dolist (c dest)
	(setq dest (union dest
                          (some (lambda (feature)
				  (get-char-attribute c feature))
				(of-component-features))
			  )))
      )
     (t
      (map-char-family
       (lambda (c)
	 (unless (memq c dest)
	   (setq dest (cons c dest)))
	 (setq dest
	       (union dest
		      (some (lambda (feature)
			      (char-feature c feature))
			    (of-component-features))
		      ))
	 nil)
       char)
      ))
    dest))

;;;###autoload
(defun ideographic-products-find (&rest components)
  (if (stringp (car components))
      (setq components (string-to-char-list (car components))))
  (let (dest products)
    (dolist (variant (char-component-variants (car components)))
      (setq products
	    (union products
		   (get-char-attribute variant 'ideographic-products))))
    (setq dest products)
    (while (and dest
		(setq components (cdr components)))
      (setq products nil)
      (dolist (variant (char-component-variants (car components)))
	(setq products
	      (union products
		     (get-char-attribute variant 'ideographic-products))))
      (setq dest (intersection dest products)))
    dest))

(defun ideograph-find-products-with-variants (components &optional ignored-chars)
  (if (stringp components)
      (setq components (string-to-char-list components)))
  (let (dest products)
    (dolist (variant (char-component-variants (car components)))
      (setq products
	    (union products
		   (set-difference
		    (get-char-attribute variant 'ideographic-products)
		    ignored-chars))))
    (setq dest products)
    (while (and dest
		(setq components (cdr components)))
      (setq products nil)
      (dolist (variant (char-component-variants (car components)))
	(setq products
	      (union products
		     (set-difference
		      (get-char-attribute variant 'ideographic-products)
		      ignored-chars))))
      (setq dest (intersection dest products)))
    dest))

(defun ideograph-find-products (components &optional ignored-chars)
  (if (stringp components)
      (setq components (string-to-char-list components)))
  (let (dest products)
    ;; (dolist (variant (char-component-variants (car components)))
    ;;   (setq products
    ;;         (union products
    ;;                (get-char-attribute variant 'ideographic-products))))
    ;; (setq dest products)
    (setq dest (get-char-attribute (car components) 'ideographic-products))
    (while (and dest
		(setq components (cdr components)))
      ;; (setq products nil)
      ;; (dolist (variant (char-component-variants (car components)))
      ;;   (setq products
      ;;         (union products
      ;;                (get-char-attribute variant 'ideographic-products))))
      (setq products (get-char-attribute (car components) 'ideographic-products))
      (setq dest (intersection dest products)))
    dest))


(defun ideographic-structure-char= (c1 c2)
  (or (eq c1 c2)
      (and c1 c2
	   (let ((m1 (char-ucs c1))
		 (m2 (char-ucs c2)))
	     (or (and m1 m2
		      (eq m1 m2))
		 (memq c1 (char-component-variants c2)))))))

(defun ideographic-structure-member-compare-components (component s-component)
  (let (ret)
    (cond ((char-ref= component s-component #'ideographic-structure-char=))
	  ((listp s-component)
	   (if (setq ret (assq 'ideographic-structure s-component))
	       (ideographic-structure-member component (cdr ret))))
	  ((setq ret (get-char-attribute s-component 'ideographic-structure))
	   (ideographic-structure-member component ret)))))

;;;###autoload
(defun ideographic-structure-member (component structure)
  "Return non-nil if COMPONENT is included in STRUCTURE."
  (or (memq component structure)
      (progn
	(setq structure (cdr structure))
	(ideographic-structure-member-compare-components
	 component (car structure)))
      (progn
	(setq structure (cdr structure))
	(ideographic-structure-member-compare-components
	 component (car structure)))
      (progn
	(setq structure (cdr structure))
	(and (car structure)
	     (ideographic-structure-member-compare-components
	      component (car structure))))))


;;;###autoload
(defun ideographic-structure-repertoire-p (structure components)
  "Return non-nil if STRUCTURE can be constructed by a subset of COMPONENTS."
  (and structure
       (let (ret s-component)
	 (catch 'tag
	   (while (setq structure (cdr structure))
	     (setq s-component (car structure))
	     (unless (characterp s-component)
	       (if (setq ret (find-char s-component))
		   (setq s-component ret)))
	     (unless (cond
		      ((listp s-component)
		       (if (setq ret (assq 'ideographic-structure s-component))
			   (ideographic-structure-repertoire-p
			    (cdr ret) components)))
		      ((member* s-component components
				:test #'ideographic-structure-char=))
		      ((setq ret
			     (get-char-attribute s-component
						 'ideographic-structure))
		       (ideographic-structure-repertoire-p ret components)))
	       (throw 'tag nil)))
	   t))))


(defvar ids-find-result-buffer "*ids-chars*")

(defun ids-find-format-line (c v)
  (format "%c\t%s\t%s\n"
	  c
	  (or (let ((ucs (or (char-ucs c)
			     (encode-char c 'ucs))))
		(if ucs
		    (cond ((<= ucs #xFFFF)
			   (format "    U+%04X" ucs))
			  ((<= ucs #x10FFFF)
			   (format "U-%08X" ucs)))))
	      "          ")
	  (or (ideographic-structure-to-ids v)
	      v)))

(defun ids-insert-chars-including-components* (components
					       &optional level ignored-chars)
  (unless level
    (setq level 0))
  (let (is i as bs)
    (dolist (c (sort (copy-list (ideograph-find-products components
							 ignored-chars))
		     (lambda (a b)
		       (if (setq as (char-total-strokes a))
			   (if (setq bs (char-total-strokes b))
			       (if (= as bs)
				   (ideograph-char< a b)
				 (< as bs))
			     t)
			 (ideograph-char< a b)))))
      (unless (memq c ignored-chars)
	(setq is (char-feature c 'ideographic-structure))
	(setq i 0)
	(while (< i level)
	  (insert "\t")
	  (setq i (1+ i)))
	(insert (ids-find-format-line c is))
	(setq ignored-chars
	      (ids-insert-chars-including-components*
	       (char-to-string c) (1+ level)
	       (cons c ignored-chars))))
      )
    )
  ignored-chars)

(defun ids-insert-chars-including-components (components
					      &optional level ignored-chars)
  (unless level
    (setq level 0))
  (setq ignored-chars
	(nreverse
	 (ids-insert-chars-including-components* components
						 level ignored-chars)))
  (let (is i as bs)
    (dolist (c ignored-chars)
      (dolist (vc (char-component-variants c))
	(unless (memq vc ignored-chars)
	  (when (setq is (get-char-attribute vc 'ideographic-structure))
	    (setq i 0)
	    (while (< i level)
	      (insert "\t")
	      (setq i (1+ i)))
	    (insert (ids-find-format-line vc is))
	    (setq ignored-chars
		  (ids-insert-chars-including-components*
		   (char-to-string vc) (1+ level)
		   (cons vc ignored-chars)))))))
    (dolist (c (sort (copy-list (ideograph-find-products-with-variants
				 components ignored-chars))
		     (lambda (a b)
		       (if (setq as (char-total-strokes a))
			   (if (setq bs (char-total-strokes b))
			       (if (= as bs)
				   (ideograph-char< a b)
				 (< as bs))
			     t)
			 (ideograph-char< a b)))))
      (unless (memq c ignored-chars)
	(setq is (get-char-attribute c 'ideographic-structure))
	(setq i 0)
	(while (< i level)
	  (insert "\t")
	  (setq i (1+ i)))
	(insert (ids-find-format-line c is))
	(setq ignored-chars
	      (ids-insert-chars-including-components*
	       (char-to-string c) (1+ level)
	       (cons c ignored-chars))))
      )
    )
  ignored-chars)

;;;###autoload
(defun ids-find-chars-including-components (components)
  "Search Ideographs whose structures have COMPONENTS."
  (interactive "sComponents : ")
  (with-current-buffer (get-buffer-create ids-find-result-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (ids-insert-chars-including-components components 0 nil)
    ;; (let ((ignored-chars
    ;;        (nreverse
    ;;         (ids-insert-chars-including-components components 0 nil
    ;;                                                #'ideograph-find-products)))
    ;;       rest)
    ;;   (setq rest ignored-chars)
    ;;   ;; (dolist (c rest)
    ;;   ;;   (setq ignored-chars
    ;;   ;;         (union ignored-chars
    ;;   ;;                (ids-insert-chars-including-components
    ;;   ;;                 (list c) 0 ignored-chars
    ;;   ;;                 #'ideograph-find-products-with-variants))))
    ;;   (ids-insert-chars-including-components components 0 ignored-chars
    ;;                                          #'ideograph-find-products-with-variants))
    (goto-char (point-min)))
  (view-buffer ids-find-result-buffer))

;;;###autoload
(define-obsolete-function-alias 'ideographic-structure-search-chars
  'ids-find-chars-including-components)

;;;###autoload
(defun ids-find-chars-covered-by-components (components)
  "Search Ideographs which structures are consisted by subsets of COMPONENTS."
  (interactive "sComponents: ")
  (if (stringp components)
      (setq components (string-to-char-list components)))
  (with-current-buffer (get-buffer-create ids-find-result-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (map-char-attribute
     (lambda (c v)
       (when (ideographic-structure-repertoire-p v components)
	 (insert (ids-find-format-line c v))))
     'ideographic-structure)
    (goto-char (point-min)))
  (view-buffer ids-find-result-buffer))


(defun ideographic-structure-merge-components-alist (ca1 ca2)
  (let ((dest-alist ca1)
	ret)
    (dolist (cell ca2)
      (if (setq ret (assq (car cell) dest-alist))
	  (setcdr ret (+ (cdr ret)(cdr cell)))
	(setq dest-alist (cons cell dest-alist))))
    dest-alist))

(defun ideographic-structure-to-components-alist (structure)
  (apply #'ideographic-structure-to-components-alist* structure))

(defun ideographic-structure-to-components-alist* (operator component1 component2
							    &optional component3
							    &rest opts)
  (let (dest-alist ret)
    (setq dest-alist
	  (cond ((characterp component1)
		 (unless (encode-char component1 'ascii)
		   (list (cons component1 1)))
		 )
		((setq ret (assq 'ideographic-structure component1))
		 (ideographic-structure-to-components-alist (cdr ret))
		 )
		((setq ret (find-char component1))
		 (list (cons ret 1))
		 )))
    (setq dest-alist
	  (ideographic-structure-merge-components-alist
	   dest-alist
	   (cond ((characterp component2)
		  (unless (encode-char component2 'ascii)
		    (list (cons component2 1)))
		  )
		 ((setq ret (assq 'ideographic-structure component2))
		  (ideographic-structure-to-components-alist (cdr ret))
		  )
		 ((setq ret (find-char component2))
		  (list (cons ret 1))
		  ))))
    (if (memq operator '(?\u2FF2 ?\u2FF3))
	(ideographic-structure-merge-components-alist
	 dest-alist
	 (cond ((characterp component3)
		(unless (encode-char component3 'ascii)
		  (list (cons component3 1)))
		)
	       ((setq ret (assq 'ideographic-structure component3))
		(ideographic-structure-to-components-alist (cdr ret))
		)
	       ((setq ret (find-char component3))
		(list (cons ret 1))
		)))
      dest-alist)))

(defun ids-find-merge-variables (ve1 ve2)
  (cond ((eq ve1 t)
	 ve2)
	((eq ve2 t)
	 ve1)
	(t
	 (let ((dest-alist ve1)
	       (rest ve2)
	       cell ret)
	   (while (and rest
		       (setq cell (car rest))
		       (if (setq ret (assq (car cell) ve1))
			   (eq (cdr ret)(cdr cell))
			 (setq dest-alist (cons cell dest-alist))))
	     (setq rest (cdr rest)))
	   (if rest
	       nil
	     dest-alist)))))

;;;###autoload
(defun ideographic-structure-equal (structure1 structure2)
  (let (dest-alist ret)
    (and (setq dest-alist (ideographic-structure-character=
			   (car structure1)(car structure2)))
	 (setq ret (ideographic-structure-character=
		    (nth 1 structure1)(nth 1 structure2)))
	 (setq dest-alist (ids-find-merge-variables dest-alist ret))
	 (setq ret (ideographic-structure-character=
		    (nth 2 structure1)(nth 2 structure2)))
	 (setq dest-alist (ids-find-merge-variables dest-alist ret))
	 (if (memq (car structure1) '(?\u2FF2 ?\u2FF3))
	     (and (setq ret (ideographic-structure-character=
			     (nth 3 structure1)(nth 3 structure2)))
		  (setq dest-alist (ids-find-merge-variables dest-alist ret)))
	   dest-alist))))

;;;###autoload
(defun ideographic-structure-character= (c1 c2)
  (let (ret ret2)
    (cond ((characterp c1)
	   (cond ((encode-char c1 'ascii)
		  (list (cons c1 c2))
		  )
		 ((characterp c2)
		  (if (encode-char c2 'ascii)
		      (list (cons c2 c1))
		    (eq c1 c2))
		  )
		 ((setq ret2 (find-char c2))
		  (eq c1 ret2)
		  )
		 ((setq ret2 (assq 'ideographic-structure c2))
		  (and (setq ret (get-char-attribute c1 'ideographic-structure))
		       (ideographic-structure-equal ret (cdr ret2)))
		  ))
	   )
	  ((setq ret (assq 'ideographic-structure c1))
	   (cond ((characterp c2)
		  (if (encode-char c2 'ascii)
		      (list (cons c2 c1))
		    (and (setq ret2 (get-char-attribute c2 'ideographic-structure))
			 (ideographic-structure-equal (cdr ret) ret2)))
		  )
		 ((setq ret2 (find-char c2))
		  (and (setq ret2 (get-char-attribute ret2 'ideographic-structure))
		       (ideographic-structure-equal (cdr ret) ret2))
		  )
		 ((setq ret2 (assq 'ideographic-structure c2))
		  (ideographic-structure-equal (cdr ret)(cdr ret2))
		  ))
	   )
	  ((setq ret (find-char c1))
	   (cond ((characterp c2)
		  (if (encode-char c2 'ascii)
		      (list (cons c2 c1))
		    (eq ret c2))
		  )
		 ((setq ret2 (find-char c2))
		  (eq ret ret2)
		  )
		 ((setq ret2 (assq 'ideographic-structure c2))
		  (and (setq ret (get-char-attribute ret 'ideographic-structure))
		       (ideographic-structure-equal ret (cdr ret2))
		       )))))))

;;;###autoload
(defun ideographic-structure-find-chars (structure)
  (let ((comp-alist (ideographic-structure-to-components-alist structure))
	ret pl str)
    (dolist (pc (caar
		 (sort (mapcar (lambda (cell)
				 (if (setq ret (get-char-attribute
						(car cell) 'ideographic-products))
				     (cons ret (length ret))
				   (cons nil 0)))
			       comp-alist)
		       (lambda (a b)
			 (< (cdr a)(cdr b))))))
      (when (or (and (setq str
			   (get-char-attribute pc 'ideographic-structure))
		     (ideographic-structure-equal str structure))
		(and (setq str
			   (get-char-attribute pc 'ideographic-structure@apparent))
		     (ideographic-structure-equal str structure))
		(and (setq str
			   (get-char-attribute pc 'ideographic-structure@apparent/leftmost))
		     (ideographic-structure-equal str structure)))
	(setq pl (cons pc pl))
	))
    pl))

;;;###autoload
(defun ideographic-char-count-components (char component)
  (let ((dest 0)
	structure)
    (cond ((eq char component)
	   1)
	  ((setq structure (get-char-attribute char 'ideographic-structure))
	   (dolist (cell (ideographic-structure-to-components-alist structure))
	     (setq dest
		   (+ dest
		      (if (eq (car cell) char)
			  (cdr cell)
			(* (ideographic-char-count-components (car cell) component)
			   (cdr cell))))))
	   dest)
	  (t
	   0))))


;;;###autoload
(defun ideographic-character-get-structure (character)
  "Return ideographic-structure of CHARACTER.
CHARACTER can be a character or char-spec."
  (mapcar (lambda (cell)
	    (or (and (listp cell)
		     (find-char cell))
		cell))
	  (let (ret)
	    (cond ((characterp character)
		   (get-char-attribute character 'ideographic-structure)
		   )
		  ((setq ret (assq 'ideographic-structure character))
		   (cdr ret)
		   )
		  ((setq ret (find-char character))
		   (get-char-attribute ret 'ideographic-structure)
		   )))))

;;;###autoload
(defun ideographic-char-match-component (char component)
  "Return non-nil if character CHAR has COMPONENT in ideographic-structure.
COMPONENT can be a character or char-spec."
  (or (ideographic-structure-character= char component)
      (let ((str (ideographic-character-get-structure char)))
	(and str
	     (or (ideographic-char-match-component (nth 1 str) component)
		 (ideographic-char-match-component (nth 2 str) component)
		 (if (memq (car str) '(?\u2FF2 ?\u2FF3))
		     (ideographic-char-match-component (nth 3 str) component)))))))

(defun ideographic-structure-char< (a b)
  (let ((sa (get-char-attribute a 'ideographic-structure))
	(sb (get-char-attribute b 'ideographic-structure))
	tsa tsb)
    (cond (sa
	   (cond (sb
		  (setq tsa (char-total-strokes a)
			tsb (char-total-strokes b))
		  (if tsa
		      (if tsb
			  (or (< tsa tsb)
			      (and (= tsa tsb)
				   (ideograph-char< a b)))
			t)
		    (if tsb
			nil
		      (ideograph-char< a b))))
		 (t
		  nil))
	   )
	  (t
	   (cond (sb
		  t)
		 (t
		  (setq tsa (char-total-strokes a)
			tsb (char-total-strokes b))
		  (if tsa
		      (if tsb
			  (or (< tsa tsb)
			      (and (= tsa tsb)
				   (ideograph-char< a b)))
			t)
		    (if tsb
			nil
		      (ideograph-char< a b)))
		  ))
	   ))
    ))

(defun ideo-comp-tree-adjoin (tree char)
  (let ((rest tree)
	included ; other
	dest cell finished)
    (while (and (not finished)
		rest)
      (setq cell (pop rest))
      (cond ((ideographic-structure-character= char (car cell))
	     (setq finished t
		   dest tree
		   rest nil)
	     )
	    ((ideographic-char-match-component char (car cell))
	     (setq dest
		   (cons (cons (car cell)
			       (ideo-comp-tree-adjoin (cdr cell) char))
			 dest))
	     (setq finished t)
	     )
	    ((ideographic-char-match-component (car cell) char)
	     (setq included (cons cell included))
	     )
            ;; (included
            ;;  (setq other (cons cell other))
            ;;  )
	    (t
	     (setq dest (cons cell dest))
	     )))
    (cond (finished
	   (nconc dest rest)
	   )
	  (included
	   (cons (cons char included)
		 (nconc dest rest))
	   )
	  (t
	   (cons (list char) tree)
	   ))))

(defun ideographic-chars-to-is-a-tree (chars)
  (let (tree)
    (dolist (char (sort (copy-list chars) #'ideographic-structure-char<))
      (setq tree (ideo-comp-tree-adjoin tree char)))
    tree))

(defun ids-find-chars-including-ids (structure)
  (let (comp-alist comp-spec ret str rest)
    (cond
     ((characterp structure)
      (setq rest (copy-list (get-char-attribute structure 'ideographic-products)))
      )
     ((setq ret (ideographic-structure-find-chars structure))
      (dolist (pc ret)
	(setq rest
	      (union
	       rest
	       (copy-list (get-char-attribute pc 'ideographic-products)))))
      )
     (t
      (setq comp-alist (ideographic-structure-to-components-alist structure)
	    comp-spec (list (cons 'ideographic-structure structure)))
      (dolist (pc (caar
		   (sort (mapcar (lambda (cell)
				   (if (setq ret (get-char-attribute
						  (car cell) 'ideographic-products))
				       (cons ret (length ret))
				     (cons nil 0)))
				 comp-alist)
			 (lambda (a b)
			   (< (cdr a)(cdr b))))))
	(when (and (every (lambda (cell)
			    (>= (ideographic-char-count-components pc (car cell))
				(cdr cell)))
			  comp-alist)
		   (or (ideographic-char-match-component pc comp-spec)
		       (and (setq str (get-char-attribute pc 'ideographic-structure))
			    (ideographic-char-match-component
			     (list
			      (cons
			       'ideographic-structure
			       (functional-ideographic-structure-to-apparent-structure
				str)))
			     comp-spec))))
	  (push pc rest)))
      ))
    (ideographic-chars-to-is-a-tree rest)))

(defun functional-ideographic-structure-to-apparent-structure (structure)
  (ideographic-structure-compare-functional-and-apparent
   structure nil 'conversion-only))

;;;###autoload
(defun ideographic-structure-compact (structure)
  (let ((rest structure)
	cell
	ret dest sub)
    (while rest
      (setq cell (pop rest))
      (cond
       ((and (consp cell)
	     (cond ((setq ret (assq 'ideographic-structure cell))
		    (setq sub (cdr ret))
		    )
		   ((atom (car cell))
		    (setq sub cell)
		    )))
	(setq cell
	      (cond ((setq ret (ideographic-structure-find-chars sub))
		     (car ret)
		     )
		    ((setq ret (ideographic-structure-compact sub))
		     (list (cons 'ideographic-structure ret))
		     )
		    (t
		     (list (cons 'ideographic-structure sub))))
	      )
	))
      (setq dest (cons cell dest)))
    (nreverse dest)))

(defun ideographic-structure-compare-functional-and-apparent (structure
							      &optional char
							      conversion-only)
  (let (enc enc-str enc2-str enc3-str new-str new-str-c
	    f-res a-res ret code)
    (cond
     ((eq (car structure) ?⿸)
      (setq enc (nth 1 structure))
      (when (setq enc-str
		  (cond ((characterp enc)
			 (get-char-attribute enc 'ideographic-structure)
			 )
			((consp enc)
			 (cdr (assq 'ideographic-structure enc))
			 )))
	(cond
	 ((eq (car enc-str) ?⿰)
	  (unless conversion-only
	    (setq f-res (ids-find-chars-including-ids enc-str)))
	  (setq new-str (list ?⿱
			      (nth 2 enc-str)
			      (nth 2 structure)))
	  (setq new-str-c
		(if (setq ret (ideographic-structure-find-chars new-str))
		    (car ret)
		  (list (cons 'ideographic-structure new-str))))
	  (if conversion-only
	      (list ?⿰ (nth 1 enc-str) new-str-c)
	    (setq a-res (ids-find-chars-including-ids new-str))
	    (list enc
		  f-res
		  new-str-c
		  a-res
		  (list ?⿰ (nth 1 enc-str) new-str-c)
		  111))
	  )
	 ((and (eq (car enc-str) ?⿲)
	       (memq (char-ucs (nth 1 enc-str)) '(#x4EBB #x2E85))
	       (eq (nth 2 enc-str) ?丨))
	  (unless conversion-only
	    (setq f-res (ids-find-chars-including-ids enc-str)))
	  (setq new-str (list ?⿱
			      (nth 3 enc-str)
			      (nth 2 structure)))
	  (setq new-str-c
		(if (setq ret (ideographic-structure-find-chars new-str))
		    (car ret)
		  (list (cons 'ideographic-structure new-str))))
	  (if conversion-only
	      (list ?⿰ (decode-char '=big5-cdp #x8B7A) new-str-c)
	    (setq a-res (ids-find-chars-including-ids new-str))
	    (list enc
		  f-res
		  new-str-c
		  a-res
		  (list ?⿰ (decode-char '=big5-cdp #x8B7A) new-str-c)
		  112))
	  )
	 ((eq (car enc-str) ?⿱)
	  (unless conversion-only
	    (setq f-res (ids-find-chars-including-ids enc-str)))
	  (setq new-str
		(list
		 (cond
		  ((characterp (nth 2 enc-str))
		   (if (or (memq (encode-char (nth 2 enc-str) '=>ucs@component)
				 '(#x20087 #x5382 #x4E06))
			   (eq (encode-char (nth 2 enc-str) '=>ucs@iwds-1)
			       #x4E06)
			   (eq (encode-char (nth 2 enc-str) '=ucs-itaiji-001)
			       #x2E282)
			   (eq (encode-char (nth 2 enc-str) '=big5-cdp)
			       #x89CE)
			   (eq (encode-char (nth 2 enc-str) '=>big5-cdp)
			       #x88E2)
			   (eq (encode-char (nth 2 enc-str) '=big5-cdp)
			       #x88AD)
			   (eq (or (encode-char (nth 2 enc-str) '=>big5-cdp)
				   (encode-char (nth 2 enc-str) '=big5-cdp-itaiji-001))
			       #x8766)
			   (eq (car (get-char-attribute (nth 2 enc-str)
							'ideographic-structure))
			       ?⿸))
		       ?⿸
		     ?⿰))
		  ((eq (car (cdr (assq 'ideographic-structure (nth 2 enc-str))))
		       ?⿸)
		   ?⿸)
		  (t
		   ?⿰))
		 (nth 2 enc-str)
		 (nth 2 structure)))
	  (setq new-str-c
		(if (setq ret (ideographic-structure-find-chars new-str))
		    (car ret)
		  (list (cons 'ideographic-structure new-str))))
	  (if conversion-only
	      (list ?⿱ (nth 1 enc-str) new-str-c)
	    (setq a-res (ids-find-chars-including-ids new-str))
	    (list enc
		  f-res
		  new-str-c
		  a-res
		  (list ?⿱ (nth 1 enc-str) new-str-c)
		  (if (eq (car new-str) ?⿸)
		      121
		    122)))
	  )
	 ((eq (car enc-str) ?⿸)
	  (unless conversion-only
	    (setq f-res (ids-find-chars-including-ids enc-str)))
	  (setq new-str (list (cond
			       ((characterp (nth 2 enc-str))
				(if (memq (char-ucs (nth 2 enc-str))
					  '(#x5F73))
				    ?⿰
				  ?⿱)
				)
			       (t
				?⿱))
			      (nth 2 enc-str)
			      (nth 2 structure)))
	  (setq new-str-c
		(if (setq ret (ideographic-structure-find-chars new-str))
		    (car ret)
		  (list (cons 'ideographic-structure new-str))))
	  (if conversion-only
	      (list ?⿸ (nth 1 enc-str) new-str-c)
	    (setq a-res (ids-find-chars-including-ids new-str))
	    (list enc
		  f-res
		  new-str-c
		  a-res
		  (list ?⿸ (nth 1 enc-str) new-str-c)
		  (if (eq (car new-str) ?⿰)
		      131
		    132)))
	  )))
      )
     ((eq (car structure) ?⿹)
      (setq enc (nth 1 structure))
      (when (setq enc-str
		  (cond ((characterp enc)
			 (get-char-attribute enc 'ideographic-structure)
			 )
			((consp enc)
			 (cdr (assq 'ideographic-structure enc))
			 )))
	(cond
	 ((eq (car enc-str) ?⿰)
	  (unless conversion-only
	    (setq f-res (ids-find-chars-including-ids enc-str)))
	  (setq new-str (list ?⿱
			      (nth 1 enc-str)
			      (nth 2 structure)))
	  (setq new-str-c
		(if (setq ret (ideographic-structure-find-chars new-str))
		    (car ret)
		  (list (cons 'ideographic-structure new-str))))
	  (if conversion-only
	      (list ?⿰ new-str-c (nth 2 enc-str))
	    (setq a-res (ids-find-chars-including-ids new-str))
	    (list enc
		  f-res
		  new-str-c
		  a-res
		  (list ?⿰ new-str-c (nth 2 enc-str))
		  210))
	  )
	 ((eq (car enc-str) ?⿱)
	  (unless conversion-only
	    (setq f-res (ids-find-chars-including-ids enc-str)))
	  (setq new-str (list ?⿰
			      (nth 2 structure)
			      (nth 2 enc-str)))
	  (setq new-str-c
		(if (setq ret (ideographic-structure-find-chars new-str))
		    (car ret)
		  (list (cons 'ideographic-structure new-str))))
	  (if conversion-only
	      (list ?⿱ (nth 1 enc-str) new-str-c)
	    (setq a-res (ids-find-chars-including-ids new-str))
	    (list enc
		  f-res
		  new-str-c
		  a-res
		  (list ?⿱ (nth 1 enc-str) new-str-c)
		  220))
	  )
	 ))
      )
     ((eq (get-char-attribute (car structure) '=ucs-itaiji-001) #x2FF6)
      (setq enc (nth 1 structure))
      (when (setq enc-str
		  (cond ((characterp enc)
			 (get-char-attribute enc 'ideographic-structure)
			 )
			((consp enc)
			 (cdr (assq 'ideographic-structure enc))
			 )))
	(cond
	 ((eq (car enc-str) ?⿺)
	  (unless conversion-only
	    (setq f-res (ids-find-chars-including-ids enc-str)))
	  (setq new-str (list ?⿱
			      (nth 2 structure)
			      (nth 1 enc-str)))
	  (setq new-str-c
		(if (setq ret (ideographic-structure-find-chars new-str))
		    (car ret)
		  (list (cons 'ideographic-structure new-str))))
	  (if conversion-only
	      (list ?⿺ new-str-c (nth 2 enc-str))
	    (setq a-res (ids-find-chars-including-ids new-str))
	    (list enc
		  f-res
		  new-str-c
		  a-res
		  (list ?⿺ new-str-c (nth 2 enc-str))
		  310))
	  )
	 ((eq (car enc-str) ?⿱)
	  (unless conversion-only
	    (setq f-res (ids-find-chars-including-ids enc-str)))
	  (setq new-str (list ?⿰
			      (nth 2 structure)
			      (nth 1 enc-str)))
	  (setq new-str-c
		(if (setq ret (ideographic-structure-find-chars new-str))
		    (car ret)
		  (list (cons 'ideographic-structure new-str))))
	  (if conversion-only
	      (list ?⿱ new-str-c (nth 2 enc-str))
	    (setq a-res (ids-find-chars-including-ids new-str))
	    (list enc
		  f-res
		  new-str-c
		  a-res
		  (list ?⿱ new-str-c (nth 2 enc-str))
		  320))
	  )
	 ((eq (car enc-str) ?⿰)
	  (unless conversion-only
	    (setq f-res (ids-find-chars-including-ids enc-str)))
	  (setq new-str (list ?⿱
			      (nth 2 structure)
			      (nth 1 enc-str)))
	  (setq new-str-c
		(if (setq ret (ideographic-structure-find-chars new-str))
		    (car ret)
		  (list (cons 'ideographic-structure new-str))))
	  (if conversion-only
	      (list ?⿰ new-str-c (nth 2 enc-str))
	    (setq a-res (ids-find-chars-including-ids new-str))
	    (list enc
		  f-res
		  new-str-c
		  a-res
		  (list ?⿰ new-str-c (nth 2 enc-str))
		  330))
	  ))
	)
      )
     ((eq (car structure) ?⿴)
      (setq enc (nth 1 structure))
      (when (setq enc-str
		  (cond ((characterp enc)
			 (get-char-attribute enc 'ideographic-structure)
			 )
			((consp enc)
			 (cdr (assq 'ideographic-structure enc))
			 )))
	(cond
	 ((eq (car enc-str) ?⿱)
	  (cond
	   ((and (characterp (nth 2 enc-str))
		 (or (memq (char-ucs (nth 2 enc-str)) '(#x56D7 #x5F51 #x897F))
		     (eq (char-feature (nth 2 enc-str) '=>big5-cdp)
			 #x87A5)))
	    (unless conversion-only
	      (setq f-res (ids-find-chars-including-ids enc-str)))
	    (setq new-str (list ?⿴
				(nth 2 enc-str)
				(nth 2 structure)))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (if conversion-only
		(list ?⿱ (nth 1 enc-str) new-str-c)
	      (setq a-res (ids-find-chars-including-ids new-str))
	      (list enc
		    f-res
		    new-str-c
		    a-res
		    (list ?⿱ (nth 1 enc-str) new-str-c)
		    411))
	    )
	   ((and (characterp (nth 2 enc-str))
		 (eq (char-ucs (nth 2 enc-str)) #x51F5))
	    (unless conversion-only
	      (setq f-res (ids-find-chars-including-ids enc-str)))
	    (setq new-str (list ?⿶
				(nth 2 enc-str)
				(nth 2 structure)))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (if conversion-only
		(list ?⿱ (nth 1 enc-str) new-str-c)
	      (setq a-res (ids-find-chars-including-ids new-str))
	      (list enc
		    f-res
		    new-str-c
		    a-res
		    (list ?⿱ (nth 1 enc-str) new-str-c)
		    412))
	    )	    
	   ((and (characterp (nth 1 enc-str))
		 (eq (char-feature (nth 1 enc-str) '=>ucs@component)
		     #x300E6))
	    (unless conversion-only
	      (setq f-res (ids-find-chars-including-ids enc-str)))
	    (setq new-str (list ?⿵
				(nth 1 enc-str)
				(nth 2 structure)))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (if conversion-only
		(list ?⿱ new-str-c (nth 2 enc-str))
	      (setq a-res (ids-find-chars-including-ids new-str))
	      (list enc
		    f-res
		    new-str-c
		    a-res
		    (list ?⿱ new-str-c (nth 2 enc-str))
		    413))
	    )
	   (t
	    (unless conversion-only
	      (setq f-res (ids-find-chars-including-ids enc-str)))
	    (setq new-str (list ?⿱ (nth 2 structure) (nth 2 enc-str)))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (if conversion-only
		(list ?⿱ (nth 1 enc-str) new-str-c)
	      (setq a-res (ids-find-chars-including-ids new-str))
	      (list enc
		    f-res
		    new-str-c
		    a-res
		    (list ?⿱ (nth 1 enc-str) new-str-c)
		    414))
	    ))
	  )
	 ((eq (car enc-str) ?⿳)
	  (cond
	   ((and (characterp (nth 2 enc-str))
		 (eq (char-ucs (nth 2 enc-str)) #x56D7))
	    (unless conversion-only
	      (setq f-res (ids-find-chars-including-ids enc-str)))
	    (setq new-str (list ?⿴ (nth 2 enc-str) (nth 2 structure)))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (setq new-str (list ?⿱ (nth 1 enc-str) new-str-c))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (if conversion-only
		(list ?⿱  new-str-c (nth 3 enc-str))
	      (setq a-res (ids-find-chars-including-ids new-str))
	      (list enc
		    f-res
		    new-str-c
		    a-res
		    (list ?⿱  new-str-c (nth 3 enc-str))
		    415))
	    )
	   ((and (characterp (nth 2 enc-str))
		 (eq (char-ucs (nth 2 enc-str)) #x5196))
	    (unless conversion-only
	      (setq f-res (ids-find-chars-including-ids enc-str)))
	    (setq new-str (list ?⿱ (nth 1 enc-str) (nth 2 enc-str)))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (setq new-str (list ?⿱ new-str-c (nth 2 structure)))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (if conversion-only
		(list ?⿱ new-str-c (nth 3 enc-str))
	      (setq a-res (ids-find-chars-including-ids new-str))
	      (list enc
		    f-res
		    new-str-c
		    a-res
		    (list ?⿱ new-str-c (nth 3 enc-str))
		    416))
	    )
	   ((and (characterp (nth 2 enc-str))
		 (or (eq (encode-char (nth 2 enc-str) '=>big5-cdp)
			 #x89A6)
		     (eq (encode-char (nth 2 enc-str) '=>gt-k)
			 146)
		     (eq (char-ucs (nth 2 enc-str)) #x2008A)))
	    (unless conversion-only
	      (setq f-res (ids-find-chars-including-ids enc-str)))
	    (setq new-str (list ?⿱ (nth 2 structure) (nth 2 enc-str)))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (setq new-str (list ?⿸ new-str-c (nth 3 enc-str)))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (if conversion-only
		(list ?⿱ (nth 1 enc-str) new-str-c)
	      (setq a-res (ids-find-chars-including-ids new-str))
	      (list enc
		    f-res
		    new-str-c
		    a-res
		    (list ?⿱ (nth 1 enc-str) new-str-c)
		    417))
	    )
	   (t
	    (unless conversion-only
	      (setq f-res (ids-find-chars-including-ids enc-str)))
	    (setq new-str (list ?⿻ (nth 2 enc-str) (nth 2 structure)))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (setq new-str (list ?⿱ (nth 1 enc-str) new-str-c))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (if conversion-only
		(list ?⿱  new-str-c (nth 3 enc-str))
	      (setq a-res (ids-find-chars-including-ids new-str))
	      (list enc
		    f-res
		    new-str-c
		    a-res
		    (list ?⿱  new-str-c (nth 3 enc-str))
		    419))
	    ))
	  )
	 ((eq (car enc-str) ?⿰)
	  (cond
	   ((equal (nth 1 enc-str)(nth 2 enc-str))
	    (unless conversion-only
	      (setq f-res (ids-find-chars-including-ids enc-str)))
	    (setq new-str (list ?⿲
				(nth 1 enc-str)
				(nth 2 structure)
				(nth 2 enc-str)))
	    (setq new-str-c
		  (list (cons 'ideographic-structure new-str)))
	    (if conversion-only
		new-str
	      (setq a-res (ids-find-chars-including-ids new-str))
	      (list enc
		    f-res
		    new-str-c
		    a-res
		    new-str
		    421))
	    )
	   (t
	    (unless conversion-only
	      (setq f-res (ids-find-chars-including-ids enc-str)))
	    (setq new-str (list ?⿰
				(nth 2 structure)
				(nth 2 enc-str)))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (if conversion-only
		(list ?⿰ (nth 1 enc-str) new-str-c)
	      (setq a-res (ids-find-chars-including-ids new-str))
	      (list enc
		    f-res
		    new-str-c
		    a-res
		    (list ?⿰ (nth 1 enc-str) new-str-c)
		    422))
	    ))
	  ))
	)
      )
     ((eq (car structure) ?⿶)
      (setq enc (nth 1 structure))
      (when (setq enc-str
		  (cond ((characterp enc)
			 (get-char-attribute enc 'ideographic-structure)
			 )
			((consp enc)
			 (cdr (assq 'ideographic-structure enc))
			 )))
	(cond
	 ((eq (car enc-str) ?⿱)
	  (setq enc2-str (ideographic-character-get-structure (nth 1 enc-str)))
	  (when (and enc2-str
		     (eq (car enc2-str) ?⿰))
	    (unless conversion-only
	      (setq f-res (ids-find-chars-including-ids enc-str)))
	    (setq new-str (list ?⿲
				(nth 1 enc2-str)
				(nth 2 structure)
				(nth 2 enc2-str)))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (if conversion-only
		(list ?⿱ new-str-c (nth 2 enc-str))
	      (setq a-res (ids-find-chars-including-ids new-str))
	      (list enc
		    f-res
		    new-str-c
		    a-res
		    (list ?⿱ new-str-c (nth 2 enc-str))
		    511))
	    )
	  )
	 ((eq (car enc-str) ?⿳)
	  (setq enc2-str (ideographic-character-get-structure (nth 1 enc-str)))
	  (when (and enc2-str
		     (eq (car enc2-str) ?⿰))
	    (unless conversion-only
	      (setq f-res (ids-find-chars-including-ids enc-str)))
	    (setq new-str (list ?⿲
				(nth 1 enc2-str)
				(nth 2 structure)
				(nth 2 enc2-str)))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (if conversion-only
		(list ?⿳ new-str-c (nth 2 enc-str) (nth 3 enc-str))
	      (setq a-res (ids-find-chars-including-ids new-str))
	      (list enc
		    f-res
		    new-str-c
		    a-res
		    (list ?⿳ new-str-c (nth 2 enc-str) (nth 3 enc-str))
		    512))
	    )
	  )
	 ((eq (car enc-str) ?⿲)
	  (unless conversion-only
	    (setq f-res (ids-find-chars-including-ids enc-str)))
	  (setq new-str (list ?⿱
			      (nth 2 structure)
			      (nth 2 enc-str)))
	  (setq new-str-c
		(if (setq ret (ideographic-structure-find-chars new-str))
		    (car ret)
		  (list (cons 'ideographic-structure new-str))))
	  (if conversion-only
	      (list ?⿲ (nth 1 enc-str) new-str-c (nth 3 enc-str))
	    (setq a-res (ids-find-chars-including-ids new-str))
	    (list enc
		  f-res
		  new-str-c
		  a-res
		  (list ?⿲ (nth 1 enc-str) new-str-c (nth 3 enc-str))
		  520))
	  )
	 ((eq (car enc-str) ?⿴)
	  (setq enc2-str (ideographic-character-get-structure (nth 1 enc-str)))
	  (when (and enc2-str
		     (eq (car enc2-str) ?⿰))
	    (unless conversion-only
	      (setq f-res (ids-find-chars-including-ids enc-str)))
	    (setq new-str (list ?⿱
				(nth 2 structure)
				(nth 2 enc-str)))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (if conversion-only
		(list ?⿲ (nth 1 enc2-str) new-str-c (nth 2 enc2-str))
	      (setq a-res (ids-find-chars-including-ids new-str))
	      (list enc
		    f-res
		    new-str-c
		    a-res
		    (list ?⿲ (nth 1 enc2-str) new-str-c (nth 2 enc2-str))
		    530))
	    )
	  )))
      )
     ((eq (car structure) ?⿵)
      (setq enc (nth 1 structure))
      (when (setq enc-str
		  (cond ((characterp enc)
			 (get-char-attribute enc 'ideographic-structure)
			 )
			((consp enc)
			 (cdr (assq 'ideographic-structure enc))
			 )))
	(cond
	 ((eq (car enc-str) ?⿱)	  
	  (cond
	   ((and (characterp (nth 2 enc-str))
		 (memq (char-ucs (nth 2 enc-str))
		       '(#x9580 #x9B25)))
	    (unless conversion-only
	      (setq f-res (ids-find-chars-including-ids enc-str)))
	    (setq new-str (list ?⿵
				(nth 2 enc-str)
				(nth 2 structure)))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (if conversion-only
		(list ?⿱ (nth 1 enc-str) new-str-c)
	      (setq a-res (ids-find-chars-including-ids new-str))
	      (list enc
		    f-res
		    new-str-c
		    a-res
		    (list ?⿱ (nth 1 enc-str) new-str-c)
		    601))
	    )
	   ((and (setq enc2-str (ideographic-character-get-structure (nth 2 enc-str)))
		 (cond
		  ((eq (car enc2-str) ?⿰)
		   (setq code 611)
		   )
		  ((eq (car enc2-str) ?⿲)
		   (setq code 614)
		   )
		  ((and (eq (car enc2-str) ?⿱)
			(setq enc3-str
			      (ideographic-character-get-structure (nth 2 enc2-str)))
			(eq (car enc3-str) ?⿰))
		   (setq code 613)
		   )))
	    (unless conversion-only
	      (setq f-res (ids-find-chars-including-ids enc-str)))
	    (setq new-str
		  (cond ((eq code 611)
			 (list ?⿲
			       (nth 1 enc2-str)
			       (nth 2 structure)
			       (nth 2 enc2-str))
			 )
			((eq code 613)
			 (list ?⿲
			       (nth 1 enc3-str)
			       (nth 2 structure)
			       (nth 2 enc3-str))
			 )
			((eq code 614)
			 (list ?⿲
			       (nth 1 enc2-str)
			       (list (list 'ideographic-structure
					   ?⿱
					   (nth 2 enc2-str)
					   (nth 2 structure)))
			       (nth 3 enc2-str))
			 )))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure
				(ideographic-structure-compact new-str)))))
	    (if conversion-only
		(cond ((or (eq code 611)
			   (eq code 614))
		       (list ?⿱ (nth 1 enc-str) new-str-c)
		       )
		      ((eq code 613)
		       (list ?⿳ (nth 1 enc-str)(nth 1 enc2-str) new-str-c)
		       ))
	      (setq a-res (ids-find-chars-including-ids new-str))
	      (list enc
		    f-res
		    new-str-c
		    a-res
		    (cond ((or (eq code 611)
			       (eq code 614))
			   (list ?⿱ (nth 1 enc-str) new-str-c)
			   )
			  ((eq code 613)
			   (list ?⿳ (nth 1 enc-str)(nth 1 enc2-str) new-str-c)
			   ))
		    code))
	    ))
	  )
	 ((eq (car enc-str) ?⿳)
	  (setq enc2-str (ideographic-character-get-structure (nth 3 enc-str)))
	  (when (and enc2-str
		     (eq (car enc2-str) ?⿰))
	    (unless conversion-only
	      (setq f-res (ids-find-chars-including-ids enc-str)))
	    (setq new-str (list ?⿲
				(nth 1 enc2-str)
				(nth 2 structure)
				(nth 2 enc2-str)))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (if conversion-only
		(list ?⿳ (nth 1 enc-str) (nth 2 enc-str) new-str-c)
	      (setq a-res (ids-find-chars-including-ids new-str))
	      (list enc
		    f-res
		    new-str-c
		    a-res
		    (list ?⿳ (nth 1 enc-str) (nth 2 enc-str) new-str-c)
		    612))
	    )
	  )
	 ((eq (car enc-str) ?⿲)
	  (unless conversion-only
	    (setq f-res (ids-find-chars-including-ids enc-str)))
	  (setq new-str (list ?⿱
			      (nth 2 enc-str)
			      (nth 2 structure)))
	  (setq new-str-c
		(if (setq ret (ideographic-structure-find-chars new-str))
		    (car ret)
		  (list (cons 'ideographic-structure new-str))))
	  (if conversion-only
	      (list ?⿲ (nth 1 enc-str) new-str-c (nth 3 enc-str))
	    (setq a-res (ids-find-chars-including-ids new-str))
	    (list enc
		  f-res
		  new-str-c
		  a-res
		  (list ?⿲ (nth 1 enc-str) new-str-c (nth 3 enc-str))
		  620))
	  )
	 ((eq (car enc-str) ?⿴)
	  (setq enc2-str (ideographic-character-get-structure (nth 1 enc-str)))
	  (when (and enc2-str
		     (eq (car enc2-str) ?⿰))
	    (unless conversion-only
	      (setq f-res (ids-find-chars-including-ids enc-str)))
	    (setq new-str (list ?⿱
				(nth 2 enc-str)
				(nth 2 structure)))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (if conversion-only
		(list ?⿲ (nth 1 enc2-str) new-str-c (nth 2 enc2-str))
	      (setq a-res (ids-find-chars-including-ids new-str))
	      (list enc
		    f-res
		    new-str-c
		    a-res
		    (list ?⿲ (nth 1 enc2-str) new-str-c (nth 2 enc2-str))
		    630))
	    )
	  )))
      )
     ((eq (car structure) ?⿷)
      (setq enc (nth 1 structure))
      (when (setq enc-str
		  (cond ((characterp enc)
			 (get-char-attribute enc 'ideographic-structure)
			 )
			((consp enc)
			 (cdr (assq 'ideographic-structure enc))
			 )))
	(cond
	 ((eq (car enc-str) ?⿺)
	  (unless conversion-only
	    (setq f-res (ids-find-chars-including-ids enc-str)))
	  (setq new-str (list ?⿱
			      (nth 2 enc-str)
			      (nth 2 structure)))
	  (setq new-str-c
		(if (setq ret (ideographic-structure-find-chars new-str))
		    (car ret)
		  (list (cons 'ideographic-structure new-str))))
	  (if conversion-only
	      (list ?⿺ (nth 1 enc-str) new-str-c)
	    (setq a-res (ids-find-chars-including-ids new-str))
	    (list enc
		  f-res
		  new-str-c
		  a-res
		  (list ?⿺ (nth 1 enc-str) new-str-c)
		  710))
	  )))
      )
     ((eq (car structure) ?⿺)
      (setq enc (nth 1 structure))
      (when (setq enc-str
		  (cond ((characterp enc)
			 (or (get-char-attribute enc 'ideographic-structure)
			     (get-char-attribute enc 'ideographic-structure@apparent)
			     (get-char-attribute enc 'ideographic-structure@apparent/leftmost))
			 )
			((consp enc)
			 (or (cdr (assq 'ideographic-structure enc))
			     (cdr (assq 'ideographic-structure@apparent enc))
			     (cdr (assq 'ideographic-structure@apparent/leftmost enc)))
			 )))
        ;; (setq enc-str
        ;;       (mapcar (lambda (cell)
        ;;                 (or (and (listp cell)
        ;;                          (find-char cell))
        ;;                     cell))
        ;;               enc-str))
	(cond
	 ((eq (car enc-str) ?⿱)
	  (cond
	   ((and (characterp (nth 1 enc-str))
		 (or (and (eq (char-ucs (nth 1 enc-str)) #x200CA)
			  (setq code 811))
		     (and (eq (char-feature (nth 1 enc-str) '=>iwds-1) 233)
			  (characterp (nth 2 structure))
			  (eq (char-ucs (nth 2 structure)) #x4E36)
			  (setq code 812))))
	    (unless conversion-only
	      (setq f-res (ids-find-chars-including-ids enc-str)))
	    (setq new-str (list ?⿺
				(nth 1 enc-str)
				(nth 2 structure)))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (if conversion-only
		(list ?⿱ new-str-c (nth 2 enc-str))
	      (setq a-res (ids-find-chars-including-ids new-str))
	      (list enc
		    f-res
		    new-str-c
		    a-res
		    (list ?⿱ new-str-c (nth 2 enc-str))
		    code))
	    )
	   ((and (characterp (nth 2 enc-str))
		 (or (memq (char-ucs (nth 2 enc-str))
			   '(#x4E00
			     #x706C
			     #x65E5 #x66F0 #x5FC3
			     #x2123C #x58EC #x738B #x7389))
		     (memq (encode-char (nth 2 enc-str) '=>ucs@component)
			   '(#x2123C #x58EC))
		     (eq (encode-char (nth 2 enc-str) '=>ucs@iwds-1)
			 #x7389)))
	    (unless conversion-only
	      (setq f-res (ids-find-chars-including-ids enc-str)))
	    (setq new-str (list ?⿰
				(nth 1 enc-str)
				(nth 2 structure)))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (if conversion-only
		(list ?⿱ new-str-c (nth 2 enc-str))
	      (setq a-res (ids-find-chars-including-ids new-str))
	      (list enc
		    f-res
		    new-str-c
		    a-res
		    (list ?⿱ new-str-c (nth 2 enc-str))
		    813))
	    )
	   ))))
      )
     ((eq (car structure) ?⿻)
      (setq enc (nth 1 structure))
      (when (setq enc-str
		  (cond ((characterp enc)
			 (get-char-attribute enc 'ideographic-structure)
			 )
			((consp enc)
			 (cdr (assq 'ideographic-structure enc))
			 )))
	(cond
	 ((eq (car enc-str) ?⿱)
	  (unless conversion-only
	    (setq f-res (ids-find-chars-including-ids enc-str)))
	  (if conversion-only
	      (list ?⿳ (nth 1 enc-str) (nth 2 structure) (nth 2 enc-str))
	    (list enc
		  f-res
		  new-str
		  nil
		  (list ?⿳
			(nth 1 enc-str)
			(nth 2 structure)
			(nth 2 enc-str))
		  911))
	  )))
      ))
    ))


;;; @ End.
;;;

(provide 'ids-find)

;;; ids-find.el ends here
