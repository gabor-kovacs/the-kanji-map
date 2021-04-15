;;; ids-rw.el --- Rewriting utility for ideographic-structure.

;; Copyright (C) 2006, 2020 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <tomo@kanji.zinbun.kyoto-u.ac.jp>
;; Keywords: IDS, TRS, IDC, Ideographs, UCS, Unicode

;; This file is a part of IDS.

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

(require 'ids-find)

(defun ideographic-structure-unify-char (structure pattern &optional env)
  (if (char-ref-p structure)
      (setq structure (plist-get structure :char)))
  (cond
   ((eq structure pattern)
    (or env t)
    )
   ((symbolp pattern)
    (if (eq env t)
	(setq env nil))
    (let ((ret (assq pattern env)))
      (if ret
	  (if (eq structure (cdr ret))
	      env)
	(cons (cons pattern structure)
	      env))))))

(defun ideographic-structure-unify-unit (structure pattern &optional env)
  (or (ideographic-structure-unify-char structure pattern env)
      (let (prest srest ret)
	(if (and
	     (progn
	       (if (setq ret
			 (if (consp structure)
			     (cdr (assq 'ideographic-structure structure))
			   (char-feature structure 'ideographic-structure)))
		   (setq structure ret))
	       (consp structure))
	     (setq srest structure)
	     (progn
	       (if (setq ret
			 (cond ((consp pattern)
				(cdr (assq 'ideographic-structure pattern))
				)
			       ((characterp pattern)
				(char-feature pattern 'ideographic-structure)
				)))
		   (setq pattern ret))
	       (consp pattern))
	     (setq prest pattern)
	     (catch 'tag
	       (while prest
		 (if (not (setq env (ideographic-structure-unify-unit
				     (car srest) (car prest) env)))
		     (throw 'tag nil))
		 (setq prest (cdr prest)
		       srest (cdr srest)))
	       t))
	    (or env t)))))

(defun ideographic-structure-apply-to-term (term env)
  (let (ret dest rest)
    (cond
     ((symbolp term)
      (if (setq ret (assq term env))
	  (cdr ret)
	term))
     ((and (consp term)
	   (cond ((setq ret (assq 'ideographic-structure term))
		  (setq rest (cdr ret))
		  )
		 ((atom (car term))
		  (setq rest term)
		  )))
      (while rest
	(setq dest
	      (cons (ideographic-structure-apply-to-term
		     (car rest) env)
		    dest))
	(setq rest (cdr rest)))
      (list (cons 'ideographic-structure (nreverse dest)))
      )
     (t term))))

;;;###autoload
(defun ideographic-structure-rewrite (structure
				      pattern replacement
				      &optional env)
  (if (setq env (ideographic-structure-unify-unit structure pattern env))
      (ideographic-structure-apply-to-term replacement env)
    (let (srest cell ret dest)
      (if (setq ret
		(if (consp structure)
		    (cdr (assq 'ideographic-structure structure))
		  (char-feature structure 'ideographic-structure)))
	  (setq structure ret))
      (when (consp structure)
	(setq srest (cdr structure))
	(if (catch 'tag
	      (while srest
		(if (setq ret (ideographic-structure-rewrite
			       (car srest)
			       pattern replacement
			       env))
		    (throw 'tag ret))
		(setq dest (cons (car srest) dest)
		      srest (cdr srest))))
	    (cons (car structure)
		  (append (nreverse (cons ret dest))
			  (cdr srest))))))))

;;;###autoload
(defun ideographic-structure-rewrite-by-rules (structure rules)
  (let (rest rule ret)
    (while
	(progn
	  (setq rest rules)
	  (if (catch 'tag
		(while rest
		  (setq rule (car rest))
		  (if (setq ret (ideographic-structure-rewrite structure
							       (car rule)
							       (cdr rule)))
		      (throw 'tag ret))
		  (setq rest (cdr rest))))
	      (if (equal ret structure)
		  nil
		(setq structure ret)))))
    structure))

					       

;;; @ End.
;;;

(provide 'ids-rw)

;;; ids-rw.el ends here
