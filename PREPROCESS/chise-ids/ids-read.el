;;; ids-read.el --- Reader for IDS-* files

;; Copyright (C) 2002, 2003, 2004, 2020, 2021 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <tomo@kanji.zinbun.kyoto-u.ac.jp>
;; Keywords: IDS, IDC, Ideographs, UCS, Unicode

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

(require 'ids)

;;;###autoload
(defun ids-read-buffer (buffer &optional simplify soft)
  (interactive "bBuffer = \nP")
  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (let (line chs ids apparent-ids code char u-char structure)
      (while (not (eobp))
	(unless (looking-at ";")
	  (setq line
		(split-string
		 (buffer-substring (point-at-bol)(point-at-eol))
		 "\t"))
	  (setq chs (car line)
		ids (nth 2 line)
		apparent-ids (nth 3 line)
		u-char nil)
	  (setq apparent-ids
		(if (and apparent-ids
			 (string-match "^@apparent=" apparent-ids))
		    (substring apparent-ids (match-end 0))))
	  (setq char
		(cond
		 ((string-match "U[-+]\\([0-9A-F]+\\)" chs)
		  (setq code (string-to-int (match-string 1 chs) 16))
		  (setq u-char (decode-char '=ucs@unicode code))
		  (decode-char 'ucs code))
		 ((string-match "J90-\\([0-9A-F][0-9A-F][0-9A-F][0-9A-F]\\)"
				chs)
		  (decode-char 'japanese-jisx0208-1990
			       (string-to-int (match-string 1 chs) 16)))
		 ((string-match
		   "C\\([1-7]\\)-\\([0-9A-F][0-9A-F][0-9A-F][0-9A-F]\\)"
		   chs)
		  (decode-char
		   (intern
		    (concat "chinese-cns11643-" (match-string 1 chs)))
		   (string-to-int (match-string 2 chs) 16)))
		 ((string-match "CDP-\\([0-9A-F][0-9A-F][0-9A-F][0-9A-F]\\)"
				chs)
		  (decode-char '=big5-cdp
			       (string-to-int (match-string 1 chs) 16)))
		 ((string-match
		   "HZK\\([0-9][0-9]\\)-\\([0-9A-F][0-9A-F][0-9A-F][0-9A-F]\\)"
		   chs)
		  (decode-char (intern
				(format "=hanziku-%d"
					(string-to-int (match-string 1 chs))))
			       (string-to-int (match-string 2 chs) 16)))
		 ((string-match "M-\\([0-9]+\\)'" chs)
		  (setq code (string-to-int (match-string 1 chs)))
		  (map-char-attribute
		   (lambda (key val)
		     (if (and (eq (car val) code)
			      (eq (nth 1 val) 1)
			      (null (nthcdr 2 val)))
			 key))
		   'morohashi-daikanwa))
		 ((string-match "M-\\([0-9]+\\)\"" chs)
		  (setq code (string-to-int (match-string 1 chs)))
		  (map-char-attribute
		   (lambda (key val)
		     (if (and (eq (car val) code)
			      (eq (nth 1 val) 2)
			      (null (nthcdr 2 val)))
			 key))
		   'morohashi-daikanwa))
		 ((string-match "M-\\([0-9]+\\)" chs)
		  (decode-char 'ideograph-daikanwa
			       (string-to-int (match-string 1 chs))))
		 ((string-match "MH-\\([0-9]+\\)" chs)
		  (setq code (string-to-int (match-string 1 chs)))
		  (map-char-attribute
		   (lambda (key val)
		     (if (and (eq (car val) 'ho)
			      (eq (nth 1 val) code)
			      (null (nthcdr 2 val)))
			 key))
		   'morohashi-daikanwa))
		 ((string-match "CB\\([0-9]+\\)" chs)
		  (decode-char 'ideograph-cbeta
			       (string-to-int (match-string 1 chs))))
		 ((string-match "SW-JIGUGE-\\([0-9]+\\)" chs)
		  (decode-char '=shuowen-jiguge
			       (string-to-int (match-string 1 chs))))
		 ))
	  (when char
	    (when (and (>= (length ids) 3)
		       (not (string-match "\\?" ids))
		       (consp (setq structure (ids-parse-string ids simplify))))
	      (when (or (not soft)
			(null
			 (get-char-attribute char 'ideographic-structure)))
		(put-char-attribute char
				    'ideographic-structure
				    (cdr (car structure))))
	      (when (and u-char
			 (not (eq char u-char))
			 (or (not soft)
			     (null
			      (get-char-attribute
			       u-char 'ideographic-structure))))
		(put-char-attribute
		 u-char 'ideographic-structure
		 (ideographic-structure-convert-to-domain
		  (cdr (car structure)) 'unicode))))
	    (when (and (>= (length apparent-ids) 3)
		       (consp (setq structure
				    (ids-parse-string apparent-ids simplify))))
	      (when (or (not soft)
			(null
			 (get-char-attribute char 'ideographic-structure@apparent)))
		(put-char-attribute char
				    'ideographic-structure@apparent
				    (cdr (car structure)))))
	    )
	  )
	(forward-line)
	))))

;;;###autoload
(defun ids-read-file (file &optional simplify soft)
  (interactive "fIDS file = \nP")
  (with-temp-buffer
    (insert-file-contents file)
    (ids-read-buffer (current-buffer) simplify soft)))


;;; @ End.
;;;

(provide 'ids-read)

;;; ids-read.el ends here
