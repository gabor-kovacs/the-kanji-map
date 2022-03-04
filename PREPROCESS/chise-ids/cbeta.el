;;; cbeta.el --- Parser for CBETA Ideographs representation.

;; Copyright (C) 2001,2002,2006,2007 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <tomo@kanji.zinbun.kyoto-u.ac.jp>
;; Keywords: CBETA, IDS, Ideographs, UCS, Unicode

;; This file is a part of the CHISE-IDS package.

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

(defvar cbeta-replacement-char-alist
  (list '(?\u2502 . ?\u4E28)
	'(?\u251C . ?\u2E8A)
	(cons ?\u2524 (decode-char '=gt-k 00153))
	(cons ?\u3026 (decode-char 'japanese-jisx0208 #x5035))
	'(?\u3039 . ?\u8279)
	'(?\u3106 . ?\u6535)
	'(?\u3113 . ?\u37A2)
	'(?\u3117 . ?\u5369)
	'(?\u3128 . ?\u3405)
	'(?\u3120 . ?\u5E7A)
	))

(defun cbeta-parse-element (string simplify robust strict-component)
  (let ((chr (aref string 0))
	ret)
    (cond ((eq chr ?\()
	   (if (> (length string) 1)
	       (let* ((ret (cbeta-parse-1 (substring string 1) simplify
					  robust strict-component))
		      (str (cdr ret)))
		 (if (and str
			  (>= (length str) 1)
			  (eq (aref str 0) ?\)))
		     (cons (car ret)
			   (if (> (length str) 1)
			       (substring str 1)))))))
	  ((eq chr ?\))
	   nil)
	  (t
	   (cons (if (setq ret (assq chr cbeta-replacement-char-alist))
		     (cdr ret)
		   chr)
		 (if (> (length string) 1)
		     (substring string 1)))))))

(defun cbeta-parse-component (string simplify robust strict-component)
  (let ((ret (cbeta-parse-1 string simplify robust strict-component))
	rret)
    (when ret
      (if (and simplify
	       (listp (car ret))
	       (setq rret (ideographic-structure-find-char
			   (cdr (assq 'ideographic-structure (car ret))))))
	  (cons rret (cdr ret))
	ret))))

(defun cbeta-parse-horizontal (l-chr string simplify
				     robust strict-component)
  (let ((ret (cbeta-parse-component
	      string simplify robust strict-component))
	rc)
    (when ret
      (if (and simplify
	       (listp l-chr)
	       (setq rc (ideographic-structure-find-char
			 (cdr (assq 'ideographic-structure l-chr)))))
	  (setq l-chr rc))
      (cons (list
	     (list 'ideographic-structure
                   ;; '(:cdp-combinator 1 :char #x2FF0)
		   ?\u2FF0
		   l-chr (car ret)))
	    (cdr ret)))))

(defun cbeta-parse-vertical (u-chr string simplify
				   robust strict-component)
  (let ((ret (cbeta-parse-component
	      string simplify robust strict-component))
	rc)
    (when ret
      (if (and simplify
	       (listp u-chr)
	       (setq rc (ideographic-structure-find-char
			 (cdr (assq 'ideographic-structure u-chr)))))
	  (setq u-chr rc))
      (cons (list
	     (list 'ideographic-structure
                   ;; '(:cdp-combinator 2 :char #x2FF1)
		   ?\u2FF1
		   u-chr (car ret)))
	    (cdr ret)))))

(defun cbeta-parse-other (u-chr string simplify
				robust strict-component)
  (let ((ret (cbeta-parse-component
	      string simplify robust strict-component))
	rc)
    (when ret
      (if (and simplify
	       (listp u-chr)
	       (setq rc (ideographic-structure-find-char
			 (cdr (assq 'ideographic-structure u-chr)))))
	  (setq u-chr rc))
      (cons (list
	     (list 'ideographic-structure
		   (cond ((memq u-chr '(?\u56D7))
			  ?\u2FF4)
			 ((memq u-chr '(?\u51E0))
			  ?\u2FF5)
			 ((memq u-chr '(?\u51F5))
			  ?\u2FF6)
			 ((memq u-chr '(?\u531A))
			  ?\u2FF7)
			 ((memq u-chr '(?\u5382 ?\u5C38))
			  ?\u2FF8)
			 (t
			  ?\u2FFB))
		   u-chr (car ret)))
	    (cdr ret)))))

(defun cbeta-substitute-char (s-chr old-chr new-chr)
  (let ((structure
	 (if (characterp s-chr)
	     (get-char-attribute s-chr 'ideographic-structure)
	   (cdr (assq 'ideographic-structure s-chr))))
	component dest ret)
    (catch 'tag
      (while structure
	(setq component (car structure)
	      structure (cdr structure))
	(cond ((equal component old-chr)
	       (setq ret (nconc (nreverse dest)
				(cons new-chr structure)))
	       (throw 'tag
		      (if (cdr (cdr ret))
			  (list (cons 'ideographic-structure ret))
			(car (cdr ret)))))
	      ((setq ret (cbeta-substitute-char component old-chr new-chr))
	       (setq ret (nconc (nreverse dest)
				(cons ret structure)))
	       (throw 'tag
		      (if (cdr (cdr ret))
			  (list (cons 'ideographic-structure ret))
			(car (cdr ret)))))
	      (t
	       (setq dest (cons component dest))))))))

(defun cbeta-delete-char (s-chr d-chr &optional strict-component)
  (let ((dcl (if strict-component
		 (list d-chr)
	       (char-component-variants d-chr)))
	(structure
	 (if (characterp s-chr)
	     (char-feature s-chr 'ideographic-structure)
	   (cdr (assq 'ideographic-structure s-chr))))
	component dest ret)
    (catch 'tag
      (while structure
	(setq component (car structure)
	      structure (cdr structure))
	(cond ((memq component dcl) ; (equal component d-chr)
	       (setq ret (nconc (nreverse dest) structure))
	       (throw 'tag
		      (if (cdr (cdr ret))
			  (list (cons 'ideographic-structure ret))
			(car (cdr ret)))))
	      ((setq ret (cbeta-delete-char component d-chr strict-component))
	       (setq ret (nconc (nreverse dest)
				(cons ret structure)))
	       (throw 'tag
		      (if (cdr (cdr ret))
			  (list (cons 'ideographic-structure ret))
			(car (cdr ret)))))
	      (t
	       (setq dest (cons component dest))))))))

(defun cbeta-parse-substitution (s-chr string simplify
				       robust strict-component)
  (let ((ret (cbeta-parse-1 string simplify robust strict-component))
	old-chr new-chr str)
    (when ret
      (setq old-chr (car ret)
	    str (cdr ret))
      (when (and str
		 (eq (aref str 0) ?+)
		 (>= (length str) 2))
	(setq str (substring str 1))
	(setq ret (cbeta-parse-1 str simplify robust strict-component))
	(when ret
	  (setq new-chr (car ret)
		str (cdr ret))
	  (when (setq ret (cbeta-substitute-char s-chr old-chr new-chr))
	    (cons ret str)))))))

(defun cbeta-parse-elimination (s-chr string simplify
				      robust strict-component)
  (let ((ret (cbeta-parse-1 string simplify robust strict-component))
	old-chr str)
    (when ret
      (setq old-chr (car ret)
	    str (cdr ret))
      (cond ((setq ret (cbeta-delete-char
			s-chr old-chr strict-component))
	     (cons ret str))
	    (robust
	     (cons s-chr str))))))

(defun cbeta-parse-1 (string simplify &optional robust strict-component)
  (let ((ret (cbeta-parse-element string simplify robust strict-component))
	c1 str
	op)
    (when ret
      (setq c1 (car ret)
	    str (cdr ret))
      (or (if (and str
		   (setq op (aref str 0))
		   (> (length str) 1)
		   (setq str (substring str 1)))
	      (cond ((eq op ?*)
		     (cbeta-parse-horizontal
		      c1 str simplify robust strict-component))
		    ((eq op ?/)
		     (cbeta-parse-vertical
		      c1 str simplify robust strict-component))
		    ((eq op ?@)
		     (cbeta-parse-other
		      c1 str simplify robust strict-component))
		    ((eq op ?-)
		     (or (cbeta-parse-substitution
			  c1 str simplify robust strict-component)
			 (cbeta-parse-elimination
			  c1 str simplify robust strict-component)))))
	  ret))))


;;; @ End.
;;;

(provide 'cbeta)

;;; cbeta.el ends here
