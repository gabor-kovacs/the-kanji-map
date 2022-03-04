;;; ids-util.el --- Utilities about ideographic-structure -*- coding: utf-8 -*-

;; Copyright (C) 2001,2002,2003,2004 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <tomo@kanji.zinbun.kyoto-u.ac.jp>
;; Keywords: ideographic-structure, CHISE, IDS, IDC, UCS, database

;; This file is a part of Tomoyo Utilities.

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

;;; Commentary:

;;; Code:

;;;###autoload
(defun ideographic-structure-convert-to-domain (structure domain)
  (let (dest cell ret)
    (while structure
      (setq cell (car structure))
      (setq dest
	    (cons
	     (cond ((characterp cell)
		    (char-representative-of-domain cell domain))
		   ((and (consp cell)
			 (symbolp (car cell)))
		    cell)
		   ((setq ret (find-char cell))
		    (char-representative-of-domain cell domain))
		   ((setq ret (assq 'ideographic-structure cell))
		    (put-alist 'ideographic-structure
			       (ideographic-structure-convert-to-domain
				(cdr ret) domain)
			       (copy-alist cell)))
		   (t cell))
	     dest))
      (setq structure (cdr structure)))
    (nreverse dest)))

;;;###autoload
(defun ideographic-structure-convert-to-ucs (structure)
  (let (dest cell ucs ret)
    (while structure
      (setq cell (car structure))
      (setq dest
	    (cons
	     (cond ((characterp cell)
		    (if (or (get-char-attribute cell 'ucs)
			    (null
			     (setq ucs
				   (or (get-char-attribute cell '=>ucs)
				       (get-char-attribute cell '->ucs)))))
			cell
		      (decode-char 'ucs ucs)))
		   ((and (consp cell)
			 (symbolp (car cell)))
		    cell)
		   ((setq ret (find-char cell))
		    (if (or (get-char-attribute ret 'ucs)
			    (null
			     (setq ucs
				   (or (get-char-attribute ret '=>ucs)
				       (get-char-attribute ret '->ucs)))))
			cell
		      (decode-char 'ucs ucs)))
		   ((setq ret (assq 'ideographic-structure cell))
		    (put-alist 'ideographic-structure
			       (ideographic-structure-convert-to-ucs
				(cdr ret))
			       (copy-alist cell)))
		   (t cell))
	     dest))
      (setq structure (cdr structure)))
    (nreverse dest)))

(defun char-cns11643-p (char &optional defined-only)
  (some (lambda (n)
	  (encode-char char
		       (intern (format "=cns11643-%d" n))
		       defined-only))
	'(1 2 3 4 5 6 7)))

(defun char-representative-of-cns11643 (char)
  (if (char-cns11643-p char)
      char
    (let ((ucs (char-ucs char))
	  variants)
      (if (and ucs
	       (setq variants
		     (char-variants (decode-char 'ucs ucs))))
	  (while (and variants
		      (setq char (car variants))
		      (not (char-cns11643-p char)))
	    (setq variants (cdr variants))))
      char)))

(defun ideographic-structure-convert-to-cns11643 (structure)
  (let (dest cell ucs ret)
    (while structure
      (setq cell (car structure))
      (setq dest
	    (cons
	     (cond ((characterp cell)
		    (char-representative-of-cns11643 cell))
		   ((and (consp cell)
			 (symbolp (car cell)))
		    cell)
		   ((setq ret (find-char cell))
		    (char-representative-of-cns11643 ret))
		   ((setq ret (assq 'ideographic-structure cell))
		    (put-alist 'ideographic-structure
			       (ideographic-structure-convert-to-cns11643
				(cdr ret))
			       (copy-alist cell)))
		   (t cell))
	     dest))
      (setq structure (cdr structure)))
    (nreverse dest)))

(defvar morohashi-char-replace-alist
  (list
   (cons (decode-char 'chinese-big5-cdp #x8B42)
	 (decode-char 'chinese-big5-cdp #x8B42))
   (cons (decode-char 'chinese-big5-cdp #x8AFC)
	 (decode-char 'chinese-big5-cdp #x8AFC))
   (cons (decode-char 'ucs #x2EBE)
	 (decode-char 'ucs #x2EBF))
   (cons (decode-char 'ucs #x4EA0)
	 (decode-char 'chinese-big5-cdp #x8B42))
   (cons (decode-char 'ucs #x4EBD)
	 (decode-char 'chinese-big5-cdp #x8AFC))
   (cons (decode-char 'ucs #x517C)
	 (decode-char 'ideograph-gt 01936))
   (cons ?亼 (decode-char 'chinese-big5-cdp #x8AFC))
   (cons (decode-char 'chinese-big5-cdp #x8AFC)
	 (decode-char 'chinese-big5-cdp #x8AFC))
   (cons (decode-char 'chinese-big5-cdp	#x8B69)
	 (decode-char 'chinese-big5-cdp	#x8A60))
   (cons (decode-char 'ucs #x4FDE)
	 (decode-char 'ideograph-daikanwa 01437))
   (cons (decode-char 'ucs #x5151)
	 (decode-char 'ideograph-daikanwa 01356))
   (cons (decode-char 'ucs #x5154)
	 (decode-char 'ideograph-daikanwa 01368))
   (cons (decode-char 'ucs #x5179)
	 (decode-char 'ideograph-daikanwa 30911))
   (cons (decode-char 'ucs #x518D)
	 (decode-char 'ideograph-daikanwa 01524))
   (cons (decode-char 'ucs #x5193)
	 (decode-char 'ideograph-gt 02025))
   (cons (decode-char 'ucs #x53CA)
	 (decode-char 'ideograph-daikanwa 03118))
   (cons (decode-char 'ucs #x544A)
	 (decode-char 'ideograph-daikanwa 03381))
   (cons (decode-char 'ucs #x5468)
	 (decode-char 'ideograph-daikanwa 03441))
   '(?夂 . ?夂)
   (cons (decode-char 'ucs #x5922)
	 (decode-char 'ideograph-daikanwa 05802))
   (cons (decode-char 'ucs #x5C1A)
	 (decode-char 'ucs #x5C19))
   (cons (decode-char 'ucs #x5D29)
	 (decode-char 'ideograph-daikanwa 08212))
   (cons (decode-char 'ucs #x5F66)
	 (decode-char 'ideograph-daikanwa 09980))
   (cons (decode-char 'ucs #x6247)
	 (decode-char 'ideograph-daikanwa 11743))
   (cons (decode-char 'ucs #x656C)
	 (decode-char 'ideograph-daikanwa 13303))
   (cons (decode-char 'ucs #x65E2)
	 (decode-char 'ideograph-daikanwa 13724))
   (cons (decode-char 'ucs #x6B21)
	 (decode-char 'ideograph-daikanwa 15992))
   (cons (decode-char 'ucs #x7235)
	 (decode-char 'ideograph-daikanwa 19711))
   (cons (decode-char 'ucs #x7523)
	 (decode-char 'ideograph-daikanwa 21684))
   (cons (decode-char 'ucs #x76CA)
	 (decode-char 'ideograph-daikanwa 22972))
   (cons (decode-char 'ucs #x771F)
	 (decode-char 'ideograph-daikanwa 23235))
   (cons (decode-char 'ucs #x7FBD)
	 (decode-char 'ideograph-daikanwa 28614))
   (cons (decode-char 'ucs #x7FC1)
	 (decode-char 'ideograph-daikanwa 28635))
   (cons (decode-char 'ucs #x2EA4)
	 (decode-char 'ucs #x722B))
   (cons (decode-char 'ucs #x8005)
	 (decode-char 'ideograph-daikanwa 28853))
   (cons (decode-char 'ucs #x8096)
	 (decode-char 'ideograph-daikanwa 29263))
   (cons (decode-char 'ucs #x82E5)
	 (decode-char 'ideograph-daikanwa 30796))
   (cons (decode-char 'ucs #x82D7)
	 (decode-char 'ideograph-daikanwa 30781))
   (cons (decode-char 'ucs #x82F1)
	 (decode-char 'ideograph-daikanwa 30808))
   (cons (decode-char 'ucs #x8336)
	 (decode-char 'ideograph-daikanwa 30915))
   (cons (decode-char 'ucs #x8449)
	 (decode-char 'ideograph-daikanwa 31387))
   (cons (decode-char 'ucs #x9023)
	 (decode-char 'ideograph-daikanwa 38902))
   (cons (decode-char 'ucs #x9053)
	 (decode-char 'ideograph-daikanwa 39010))
   (cons (decode-char 'ucs #x9054)
	 (decode-char 'ideograph-daikanwa 39011))
   (cons (decode-char 'ucs #x9063)
	 (decode-char 'ideograph-daikanwa 39052))
   (cons (decode-char 'ucs #x9752)
	 (decode-char 'ucs #x9751))
   (cons (decode-char 'ucs #x670B)
	 (decode-char 'ideograph-daikanwa 14340))
   (cons (decode-char 'ucs #x8981)
	 (decode-char 'ideograph-daikanwa 34768))
   (cons (decode-char 'ucs #x8AF8)
	 (decode-char 'ideograph-daikanwa 35743))
   (cons (decode-char 'japanese-jisx0213-2 #x2327)
	 (decode-char 'japanese-jisx0213-2 #x2327))
   (cons (decode-char 'chinese-big5-cdp #x8BFA)
	 (decode-char 'japanese-jisx0213-2 #x2327))
   ))

;;;###autoload
(defun ideographic-structure-convert-to-daikanwa (structure)
  (let (dest cell morohashi ret ret2 ret3)
    (while structure
      (setq cell (car structure))
      (setq dest
	    (cons
	     (cond ((characterp cell)
		    (cond ((setq ret
				 (assq cell morohashi-char-replace-alist))
			   (cdr ret))
			  ((get-char-attribute cell 'ideograph-daikanwa)
			   cell)
			  ((setq morohashi
				 (get-char-attribute
				  cell 'morohashi-daikanwa))
			   (cond ((null (cdr (cdr morohashi)))
				  cell)
				 ((= (nth 1 morohashi) 0)
				  (decode-char 'ideograph-daikanwa
					       (car morohashi)))
				 (t
				  (setq morohashi (list (car morohashi)
							(nth 1 morohashi)))
				  (or (map-char-attribute
				       (lambda (char val)
					 (if (equal morohashi val)
					     char))
				       'morohashi-daikanwa)
				      cell))))
			  (t
			   cell)))
		   ((and (consp cell)
			 (symbolp (car cell)))
		    cell)
		   ((setq ret (find-char cell))
		    (if (or (get-char-attribute ret 'ideograph-daikanwa)
			    (null
			     (setq morohashi
				   (get-char-attribute
				    ret 'morohashi-daikanwa)))
			    (null (cdr (cdr morohashi))))
			cell
		      (if (= (nth 1 morohashi) 0)
			  (decode-char 'ideograph-daikanwa (car morohashi))
			cell)))
		   ((setq ret (assq 'ideographic-structure cell))
		    (setq ret2
			  (ideographic-structure-convert-to-daikanwa
			   (cdr ret)))
		    (if (setq ret3 (ideographic-structure-find-char ret2))
			ret3
		      (put-alist 'ideographic-structure
				 ret2
				 (copy-alist cell))))
		   (t cell))
	     dest))
      (setq structure (cdr structure)))
    (nreverse dest)))


;;; @ End.
;;;

(provide 'ids-util)

;;; ids-util.el ends here
