;;; ids.el --- Parser and utility for Ideographic Description Sequence.

;; Copyright (C) 2001, 2002, 2003, 2005, 2020 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <tomo@kanji.zinbun.kyoto-u.ac.jp>
;; Keywords: IDS, IDC, Ideographs, UCS, Unicode

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

;;; Commentary:

;; Ideographic Description Sequence (IDS) is defined in ISO/IEC
;; 10646-1:2000 Annex F.

;;; Code:

(require 'ideograph-util)
(require 'ids-find)

(defun ideographic-structure-find-char (structure)
  (car (ideographic-structure-find-chars structure))
  ;; (dolist (product (char-feature (nth 1 structure) 'ideographic-products))
  ;;   (if (equal structure
  ;;              (char-feature product 'ideographic-structure))
  ;;       (return product)))
  )

(defun ids-parse-terminal (string)
  (if (>= (length string) 1)
      (let* ((chr (aref string 0))
	     (ucs (encode-char chr '=ucs 'defined-only))
	     big5)
	(unless (or (and ucs (<= #x2FF0 ucs)(<= ucs #x2FFF))
		    (memq (encode-char chr '=ucs-var-001)
			  '(#x2FF0))
		    (memq (encode-char chr '=ucs-itaiji-001)
			  '(#x2FF9 #x2FF6)))
	  (if (and ucs (<= #xE000 ucs)(<= ucs #xF8FF)
		   (setq big5 (encode-char chr 'chinese-big5)))
	      (setq chr (decode-char '=big5-cdp big5)))
	  (cons chr
		(substring string 1))))))

(defun ids-parse-op-2 (string)
  (if (>= (length string) 1)
      (let* ((chr (aref string 0))
	     (ucs (encode-char chr '=ucs 'defined-only)))
	(if (or (and ucs
		     (or (eq ucs #x2FF0)
			 (eq ucs #x2FF1)
			 (and (<= #x2FF4 ucs)(<= ucs #x2FFB))))
		(memq (encode-char chr '=ucs-var-001)
		      '(#x2FF0))
		(memq (encode-char chr '=ucs-itaiji-001)
		      '(#x2FF9 #x2FF6)))
	    (cons chr
		  (substring string 1))))))

(defun ids-parse-op-3 (string)
  (if (>= (length string) 1)
      (let ((chr (aref string 0)))
	(if (memq chr '(?\u2FF2 ?\u2FF3))
	    (cons chr
		  (substring string 1))))))

(defun ids-parse-component (string simplify)
  (let ((ret (ids-parse-element string simplify))
	rret)
    (when ret
      (if (and simplify
	       (listp (car ret))
	       (setq rret (ideographic-structure-find-char
			   (cdr (assq 'ideographic-structure (car ret))))))
	  (cons rret (cdr ret))
	ret))))

(defun ids-parse-element (string simplify)
  (let (ret op arg1 arg2 arg3)
    (cond ((ids-parse-terminal string))
	  ((setq ret (ids-parse-op-2 string))
	   (setq op (car ret))
	   (when (setq ret (ids-parse-component (cdr ret) simplify))
	     (setq arg1 (car ret))
	     (when (setq ret (ids-parse-component (cdr ret) simplify))
	       (setq arg2 (car ret))
	       (cons (list (list 'ideographic-structure op arg1 arg2))
		     (cdr ret)))))
	  ((setq ret (ids-parse-op-3 string))
	   (setq op (car ret))
	   (when (setq ret (ids-parse-component (cdr ret) simplify))
	     (setq arg1 (car ret))
	     (when (setq ret (ids-parse-component (cdr ret) simplify))
	       (setq arg2 (car ret))
	       (when (setq ret (ids-parse-component (cdr ret) simplify))
		 (setq arg3 (car ret))
		 (cons (list (list 'ideographic-structure op arg1 arg2 arg3))
		       (cdr ret)))))))))

;;;###autoload
(defun ids-parse-string (ids-string &optional simplify)
  "Parse IDS-STRING and return the result."
  (let ((ret (ids-parse-element ids-string simplify)))
    (if (= (length (cdr ret)) 0)
	(car ret))))

;; (defun ids-format-unit (ids-char)
;;   (let (ret)
;;     (cond ((characterp ids-char)
;;            (char-to-string ids-char))
;;           ((integerp ids-char)
;;            (char-to-string (decode-char 'ucs ids-char)))
;;           ((setq ret (find-char ids-char))
;;            (char-to-string ret))
;;           ((setq ret (assq 'ideographic-structure ids-char))
;;            (ids-format-list (cdr ret))))))

;; ;;;###autoload
;; (defun ids-format-list (ids-list)
;;   "Format ideographic-structure IDS-LIST as an IDS-string."
;;   (mapconcat (lambda (cell)
;;                (ids-format-unit
;;                 (if (char-ref-p cell)
;;                     (plist-get cell :char)
;;                   cell)))
;;              ids-list ""))
		     
(define-obsolete-function-alias
  'ids-format-list 'ideographic-structure-to-ids)

;;; @ End.
;;;

(provide 'ids)

;;; ids.el ends here
