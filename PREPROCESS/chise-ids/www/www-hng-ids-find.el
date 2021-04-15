(require 'ids-find)
(require 'cwiki-common)

(setq www-format-char-img-style "vertical-align:middle;")

(defvar hng-ccs-list
  (let (dest)
    (dolist (ccs (charset-list))
      (when (string-match "^===hng-" (symbol-name ccs))
	(setq dest (cons ccs dest))))
    dest))

(defun char-hng-p (char)
  (or (get-char-attribute char '->HNG)
      (char-have-hng-p char)))

(defun char-have-hng-p (char)
  (or (some (lambda (ccs)
	      (and (encode-char char ccs)
		   char))
	    hng-ccs-list)
      (some #'char-have-hng-p
	    (get-char-attribute char '->subsumptive))
      (some #'char-have-hng-p
	    (get-char-attribute char '->denotational))))

(defun decode-url-string (string &optional coding-system)
  (if (> (length string) 0)
      (let ((i 0)
	    dest)
	(while (string-match "%\\([0-9A-F][0-9A-F]\\)" string i)
	  (setq dest (concat dest
			     (substring string i (match-beginning 0))
			     (char-to-string
			      (int-char
			       (string-to-int (match-string 1 string) 16))))
		i (match-end 0)))
	(decode-coding-string
	 (concat dest (substring string i))
	 coding-system))))

(defconst www-hng-ids-find-version "0.26")

(defvar www-ids-find-ideographic-products-file-name
  (expand-file-name "ideographic-products"
		    (expand-file-name
		     "feature"
		     (expand-file-name
		      "character"
		      chise-system-db-directory))))

(defvar www-ids-find-char-viewer-url
  "/est/view/character/")

(defvar www-ids-find-chise-link-map-url-prefix
  "http://fonts.jp/chise_linkmap/map.cgi?code=")

(defvar www-ids-find-tang-chars-file-name
  "~tomo/projects/chise/ids/www/tang-chars.udd")

(defun www-ids-find-format-char (c &optional code-desc)
  (princ
   (format "<a href=\"%s%s\">%s</a>"
	   www-ids-find-char-viewer-url
	   (www-uri-encode-object c)
	   (www-format-encode-string (char-to-string c))))
  ;; (let ((str (encode-coding-string (format "%c" c) 'utf-8-er))
  ;;       plane code)
  ;;   (princ
  ;;    (with-temp-buffer
  ;;      (cond
  ;;       ((string-match "&CB\\([0-9]+\\);" str)
  ;;        (setq code (string-to-int (match-string 1 str)))
  ;;        (insert (format "<a href=\"%s"
  ;;                        www-ids-find-char-viewer-url))
  ;;        (insert str)
  ;;        (insert (format "\"><img alt=\"CB%05d\" src=\"/glyphs/cb-gaiji/%02d/CB%05d.gif\">\n"
  ;;                        code (/ code 1000) code))
  ;;        (when code-desc
  ;;          (insert (format "CB%05d</a>" code)))
  ;;        )
  ;;       ((string-match "&JC3-\\([0-9A-F]+\\);" str)
  ;;        (setq code (string-to-int (match-string 1 str) 16))
  ;;        (insert (format "<a href=\"%s"
  ;;                        www-ids-find-char-viewer-url))
  ;;        (insert str)
  ;;        (insert (format "\"><img alt=\"JC3-%04X\" src=\"http://kanji.zinbun.kyoto-u.ac.jp/db/CHINA3/Gaiji/%04x.gif\">\n"
  ;;                        code code))
  ;;        (when code-desc
  ;;          (insert (format "JC3-%04X</a>" code)))
  ;;        )
  ;;       ((string-match "&J\\(78\\|83\\|90\\|SP\\)-\\([0-9A-F]+\\);" str)
  ;;        (setq plane (match-string 1 str)
  ;;              code (string-to-int (match-string 2 str) 16))
  ;;        (insert (format "<a href=\"%s"
  ;;                        www-ids-find-char-viewer-url))
  ;;        (insert str)
  ;;        (insert (format "\"><img alt=\"J%s-%04X\" src=\"/glyphs/JIS-%s/%02d-%02d.gif\">\n"
  ;;                        plane code plane
  ;;                        (- (lsh code -8) 32)
  ;;                        (- (logand code 255) 32)))
  ;;        (when code-desc
  ;;          (insert (format "J%s-%04X</a>" plane code)))
  ;;        )
  ;;       ((string-match "&G\\([01]\\)-\\([0-9A-F]+\\);" str)
  ;;        (setq plane (string-to-int (match-string 1 str))
  ;;              code (string-to-int (match-string 2 str) 16))
  ;;        (insert (format "<a href=\"%s"
  ;;                        www-ids-find-char-viewer-url))
  ;;        (insert str)
  ;;        (insert (format "\"><img alt=\"G%d-%04X\" src=\"/glyphs/GB%d/%02d-%02d.gif\">\n"
  ;;                        plane code plane
  ;;                        (- (lsh code -8) 32)
  ;;                        (- (logand code 255) 32)))
  ;;        (when code-desc
  ;;          (insert (format "G%d-%04X</a>" plane code)))
  ;;        )
  ;;       ((string-match "&C\\([1-7]\\)-\\([0-9A-F]+\\);" str)
  ;;        (setq plane (string-to-int (match-string 1 str))
  ;;              code (string-to-int (match-string 2 str) 16))
  ;;        (insert (format "<a href=\"%s"
  ;;                        www-ids-find-char-viewer-url))
  ;;        (insert str)
  ;;        (insert (format "\"><img alt=\"C%d-%04X\" src=\"/glyphs/CNS%d/%04X.gif\">\n"
  ;;                        plane code plane code))
  ;;        (when code-desc
  ;;          (insert (format "C%d-%04X</a>" plane code)))
  ;;        )
  ;;       ((string-match "&ZOB-\\([0-9]+\\);" str)
  ;;        (setq code (string-to-int (match-string 1 str)))
  ;;        (insert (format "<a href=\"%s"
  ;;                        www-ids-find-char-viewer-url))
  ;;        (insert str)
  ;;        (insert (format "\"><img alt=\"ZOB-%04d\" src=\"/glyphs/ZOB-1968/%04d.png\">\n"
  ;;                        code code))
  ;;        (when code-desc
  ;;          (insert (format "ZOB-%04d</a>" code)))
  ;;        )
  ;;       (t
  ;;        (insert (format "<a href=\"%s"
  ;;                        www-ids-find-char-viewer-url))
  ;;        ;; (insert str)
  ;;        (insert
  ;;         (mapconcat (lambda (c)
  ;;                      (if (<= (char-int c) #x7F)
  ;;                          (char-to-string c)
  ;;                        (format "%%%02X" c)))
  ;;                    str ""))
  ;;        (insert "\">")
  ;;        (insert str)
  ;;        (insert "</a>")
  ;;        ))
  ;;      (goto-char (point-min))
  ;;      (while (search-forward "&" nil t)
  ;;        (replace-match "&amp;" t 'literal))
  ;;      (buffer-string))))
  )
  
(defun www-ids-find-format-line (c is)
  (let (ucs len i ids)
    (princ "<span class=\"entry\">")
    (www-ids-find-format-char c 'code-desc)
    (princ "</span>")
    (princ
     (or (if (setq ucs (or (char-ucs c)
			   (encode-char c 'ucs)))
	     (format
	      " <a href=\"http://www.unicode.org/cgi-bin/GetUnihanData.pl?codepoint=%X\">%s</a>"
	      ucs
	      (cond ((<= ucs #xFFFF)
		     (format "U+%04X" ucs))
		    ((<= ucs #x10FFFF)
		     (format "U-%08X" ucs))))
	   "          ")))
    (when ucs
      (princ
       (format " <a href=\"%s%X\">(link map)</a>"
	       www-ids-find-chise-link-map-url-prefix ucs)))
    (princ " ")
    (when is
      (setq ids (ideographic-structure-to-ids is))
      (setq i 0
	    len (length ids))
      (princ "<span class=\"ids\">")      
      (while (< i len)
	(www-ids-find-format-char (aref ids i))
	(setq i (1+ i)))
      (princ "</span>"))
    (when (and ucs
	       (with-current-buffer
		   (find-file-noselect
		    www-ids-find-tang-chars-file-name)
		 (goto-char (point-min))
		 (re-search-forward (format "^%d$" ucs) nil t)))
      (princ
       (format " <a href=\"http://coe21.zinbun.kyoto-u.ac.jp/djvuchar?query=%s\">"
	       (mapconcat
		(lambda (c)
		  (format "%%%02X" (char-int c)))
		(encode-coding-string (char-to-string c)
				      'utf-8-jp)
		"")))
      (princ (encode-coding-string "⇒[唐代拓本]</a>" 'utf-8-jp-er)))
    (princ "<br>\n")))

(defun www-ids-insert-chars-including-components (components
						  &optional ignored-chars)
  (let ((ret (ideographic-products-find components))
	products
	is as bs len ignore-children)
    (dolist (char ret)
      (if (char-hng-p char)
	  (setq products (cons char products))))
    (setq len (length products))
    (when (>= len 1024)
      (setq ignore-children t)
      (princ
       (encode-coding-string
	"<p>結果が多すぎるため、再帰的検索を省略しました。</p>"
	'utf-8-jp-er)))
    (if (>= len 2048)
	(dolist (c products)
	  (www-ids-find-format-char c))
      (princ "<ul>\n")
      (dolist (c (cond
                  ;; ((>= len 2048)
                  ;;  (setq ignore-children t)
                  ;;  products)
                  ;; ((>= len 1024)
                  ;;  products)
		  ((>= len 1024)
		   (sort (copy-list products)
			 (lambda (a b)
			   (< (char-int a)(char-int b))))
		   )
		  ((>= len 512)
		   (sort (copy-list products)
			 (lambda (a b)
			   (if (setq as (char-total-strokes a))
			       (if (setq bs (char-total-strokes b))
				   (if (= as bs)
				       (< (char-int a)(char-int b))
				     (< as bs))
				 t)
			     (< (char-int a)(char-int b)))))
		   )
		  (t
		   (sort (copy-list products)
			 (lambda (a b)
			   (if (setq as (char-total-strokes a))
			       (if (setq bs (char-total-strokes b))
				   (if (= as bs)
				       (ideograph-char< a b)
				     (< as bs))
				 t)
			     (ideograph-char< a b))))
		   )))
	(unless (memq c ignored-chars)
	  (setq is (char-feature c 'ideographic-structure))
	  (princ "<li>")
	  (www-ids-find-format-line c is)
	  (unless ignore-children
            ;; (princ "<ul>\n")
	    (setq ignored-chars
		  (www-ids-insert-chars-including-components
		   (char-to-string c)
		   (cons c ignored-chars)))
            ;; (princ "</ul>\n")
	    ))
	)
      (princ "</ul>\n")
      ))
  ignored-chars)

(defun www-batch-ids-find ()
  (let ((components (car command-line-args-left))
	(coded-charset-entity-reference-alist
	 (list*
	  '(=cns11643-1		"C1-" 4 X)
	  '(=cns11643-2		"C2-" 4 X)
	  '(=cns11643-3		"C3-" 4 X)
	  '(=cns11643-4		"C4-" 4 X)
	  '(=cns11643-5		"C5-" 4 X)
	  '(=cns11643-6		"C6-" 4 X)
	  '(=cns11643-7		"C7-" 4 X)
	  '(=gb2312		"G0-" 4 X)
	  '(=gb12345		"G1-" 4 X)
	  '(=jis-x0208@1990	"J90-" 4 X)
	  '(=jis-x0212		"JSP-" 4 X)
	  '(=cbeta		"CB" 5 d)
	  '(=jef-china3		"JC3-" 4 X)
	  '(=jis-x0208@1978	"J78-" 4 X)
	  '(=jis-x0208@1983	"J83-" 4 X)
	  '(=daikanwa		"M-" 5 d)
	  coded-charset-entity-reference-alist))
	)
    (setq command-line-args-left (cdr command-line-args-left))
    (cond
     ((stringp components)
      (if (string-match "^components=" components)
	  (setq components (substring components (match-end 0))))
      (setq components
	    (if (> (length components) 0)
		(decode-url-string components 'utf-8-er)
	      nil))
      )
     (t
      (setq components nil)
      ))
    (princ "Content-Type: text/html; charset=UTF-8

<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"
            \"http://www.w3.org/TR/html4/loose.dtd\">
<html lang=\"ja\">
<head>
<title>CHISE IDS Find</title>
<style type=\"text/css\">
<!--
.entry { font-size: 36px; }
.entry a img { height: 36px; }
.ids { font-size: 24px; }
.ids a img { height: 24px; }
img { vertical-align:middle; }
a { text-decoration:none; }
ul { margin: 0 0; }
li { margin: 0 0 -0.2em; }
.tooltip {
    position: relative;
    display: inline-block;
}
.tooltip .tooltiptext {
    display: none;
}
-->
</style>
</head>

<body>

<h1>")
    (princ (encode-coding-string "CHISE-IDS HNG 漢字検索" 'utf-8-jp-er))
    (princ "</h1>")
    (princ "
<p>Version ")
    (princ www-hng-ids-find-version)
    (princ (format-time-string
	    " (Last-modified: %Y-%m-%d %H:%M:%S)"
	    (nth 5
		 (file-attributes
		  www-ids-find-ideographic-products-file-name))))
    (princ "
<hr>
<p>
<form action=\"/hng-ids-find\" method=\"GET\">
")
    (princ (encode-coding-string "部品文字列" 'utf-8-jp-er))
    (princ " <input type=\"text\" name=\"components\" size=\"30\" maxlength=\"30\" value=\"")
    (if (> (length components) 0)
	(princ (encode-coding-string components 'utf-8-er)))
    (princ "\">
<input type=\"submit\" value=\"")
    (princ (encode-coding-string "検索開始" 'utf-8-jp-er))
    (princ "\">
</form>

")
    (unless (file-newer-than-file-p
	     www-ids-find-ideographic-products-file-name
	     (locate-file (car command-line-args) exec-path))
      (princ (encode-coding-string "<hr>
<p>
現在、システムの更新作業中です。しばらくお待ちください。
<hr>
" 'utf-8-jp-er))
      ;; (setq components nil)
      )
    (cond
     (components
      ;; (map-char-attribute
      ;;  (lambda (c v)
      ;;    (when (every (lambda (p)
      ;;                   (ideographic-structure-member p v))
      ;;                 components)
      ;;      (princ (encode-coding-string
      ;;              (ids-find-format-line c v)
      ;;              'utf-8-jp-er))
      ;;      (princ "<br>\n")
      ;;      )
      ;;    nil)
      ;;  'ideographic-structure)
      (when (= (length components) 1)
	(www-ids-find-format-line (aref components 0)
				  (char-feature (aref components 0)
						'ideographic-structure)))
      ;; (dolist (c (ideographic-products-find components))
      ;;   (setq is (char-feature c 'ideographic-structure))
      ;;   ;; to avoid problems caused by wrong indexes
      ;;   (when (every (lambda (c)
      ;;                  (ideographic-structure-member c is))
      ;;                components)
      ;;     (www-ids-find-format-line c is)))
      ;; (princ "<ul>\n")
      (www-ids-insert-chars-including-components components)
      ;; (princ "</ul>\n")
      )
     (t
      (princ (encode-coding-string "<hr>
<p>
指定した部品を全て含む漢字の一覧を表示します。
<p>
CHISE で用いられる実態参照形式（例：&amp;M-00256;）で部品を指定する事もできます。" 'utf-8-jp-er))
      (princ (encode-coding-string "
<p>
\[Links\]
<ul>
<li><a href=\"http://www.shuiren.org/chuden/toyoshi/syoseki/chise_ids.html\"
>「CHISE IDS FINDで漢字を検索」</a> ― 山田崇仁さん（<a
href=\"http://www.shuiren.org/\">睡人亭</a>）による解説
</ul>
<ul>
<li><a href=\"http://www.karitsu.org/tools/firefox_plugin.htm\"
>Firefox 用 plugin</a> by 秋山陽一郎さん（<a href=\"http://www.karitsu.org/\"
>過立齋</a>）
</ul>
<ul>
<li><a href=\"http://cvs.m17n.org/viewcvs/chise/ids/www/www-ids-find.el?view=markup\"
>www-ids-find.el (source file (Emacs Lisp part))
<li><a href=\"http://www.chise.org/ids/\"
>「CHISE 漢字構造情報データベース」</a>
<li><a href=\"http://fonts.jp/chise_linkmap/\"
>「chise_linkmap : CHISE 漢字連環図」</a> by 上地宏一さん
<li><a href=\"http://www.chise.org/\"
>CHISE Project</a>
</ul>
<ul>
<li><a href=\"http://coe21.zinbun.kyoto-u.ac.jp/djvuchar\"
>「拓本文字データベース」</a> by
<a href=\"http://coe21.zinbun.kyoto-u.ac.jp/\"
>京都大学21世紀COE「東アジア世界の人文情報学研究教育拠点」</a>
<li><a href=\"http://www.unicode.org/\"
>Unicode</a>
</ul>"
 'utf-8-jp-er))

      ))
    (princ "<hr>")
    (princ "<p>
Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010, 2015 <a href=\"http://kanji.zinbun.kyoto-u.ac.jp/~tomo/\"
>MORIOKA Tomohiko</a>")
    (princ
     (format
      "<p>Powered by <a
href=\"http://www.chise.org/xemacs/\"
>XEmacs CHISE</a> %s."
      (encode-coding-string xemacs-chise-version 'utf-8-jp-er)))
    (princ "
</body>
</html>
")))
