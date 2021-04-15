(require 'ids-find)
(require 'cwiki-common)

(setq www-format-char-img-style "vertical-align:middle;")

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

(defconst www-ids-find-version "0.99.1")

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

(defun www-ids-insert-chars-including-components* (components
						   &optional ignored-chars products)
  (unless products
    (setq products (ideograph-find-products components ignored-chars)))
  (let (is as bs len)
    (setq len (length products))
    (princ "<ul>\n")
    (dolist (c (cond
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
	(setq ignored-chars
	      (www-ids-insert-chars-including-components*
	       (char-to-string c) (cons c ignored-chars)))
	)
      )
    (princ "</ul>\n")
    )
  ignored-chars)

(defun www-ids-insert-chars-including-components (components
						  &optional ignored-chars)
  (let ((products (ideograph-find-products components ignored-chars))
	is as bs len ignore-children)
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
      (setq ignored-chars
	    (nreverse
	     (www-ids-insert-chars-including-components* components ignored-chars products)))
      (dolist (c ignored-chars)
	(dolist (vc (char-component-variants c))
	  (unless (memq vc ignored-chars)
	    (when (setq is (get-char-attribute vc 'ideographic-structure))
	      (princ "<li>")
	      (www-ids-find-format-line vc is)
	      (setq ignored-chars
		    (www-ids-insert-chars-including-components*
		     (char-to-string vc)
		     (cons vc ignored-chars)))))))
      (setq products (ideograph-find-products-with-variants components ignored-chars))
      (setq len (length products))
      (when (>= len 512)
	(setq ignore-children t)
	(princ
	 (encode-coding-string
	  "<p>結果が多すぎるため、関連字の再帰的検索を省略しました。</p>"
	  'utf-8-jp-er)))
      (if (>= len 1024)
	  (dolist (c products)
	    (www-ids-find-format-char c))
	(dolist (c (sort (copy-tree products)
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
	    (princ "<li>")
	    (www-ids-find-format-line c is)
	    (unless ignore-children
	      (setq ignored-chars
		    (www-ids-insert-chars-including-components*
		     (char-to-string c)
		     (cons c ignored-chars))))
	    ))
	))
    )
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
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
<title>CHISE IDS Find</title>
<link href=\"/css/bootstrap-4.5.0.min.css\" rel=\"stylesheet\">
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

<div class=\"jumbotron jumbotron-fluid mb-0\">
<h1 class=\"display-4 text-center\">")
    (princ (encode-coding-string "CHISE IDS 漢字検索" 'utf-8-jp-er))
    (princ "</h1>")
    (princ "
<p class=\"text-center\">Version ")
    (princ www-ids-find-version)
    (princ (format-time-string
	    " (Last-modified: %Y-%m-%d %H:%M:%S)</p>"
	    (nth 5
		 (file-attributes
		  www-ids-find-ideographic-products-file-name))))
    (princ "
</div>
<div class=\"container mt-0 mw-100 d-inline-block align-top bg-dark\">
<p />
<div class=\"input-group mb-3 h3 my-4\">
<div class=\"input-group-prepend mw-75 ml-3\">
<form action=\"/ids-find\" method=\"GET\">
<span class=\"input-group-text\" id=\"basic-addon1\">
")
    (princ (encode-coding-string "部品文字列" 'utf-8-jp-er))
    (princ "</span>
</div>
<input type=\"text\" class=\"form-control\" aria-describedby=\"basic-addon1\" name=\"components\" size=\"30\" maxlength=\"30\" value=\"")
    (if (> (length components) 0)
	(princ (encode-coding-string components 'utf-8-er)))
    (princ "\">
<input class=\"mr-3\" type=\"submit\" value=\"")
    (princ (encode-coding-string "検索開始" 'utf-8-jp-er))
    (princ "\">
</form>
</div>
</div>

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
      (princ "<div class=\"container\">
")
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
      (princ "</div>\n")
      )
     (t
      (princ (encode-coding-string "<div class=\"container mt-4\">
<div class=\"ml-3\">
<p>
指定した部品を全て含む漢字の一覧を表示します。
</p>
<p>
CHISE で用いられる実態参照形式（例：&amp;M-00256;）で部品を指定する事もできます。
</p>
</div>
" 'utf-8-jp-er))
      (princ (encode-coding-string "
<p  class=\"ml-0\">
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
<li><a href=\"http://git.chise.org/gitweb/?p=chise/ids.git;a=blob;f=www/www-ids-find.el\"
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
</ul>
</p>
</div>
"
 'utf-8-jp-er))

      ))
    (princ "<hr>
<div class=\"container\">
")
    (princ "<div class=\"ml-0\">
Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010, 2015, 2016, 2017, 2020 <a href=\"http://kanji.zinbun.kyoto-u.ac.jp/~tomo/\"
>MORIOKA Tomohiko</a></div>")
    (princ
     (format
      "<div>Powered by <a
href=\"http://www.chise.org/xemacs/\"
>XEmacs CHISE</a> %s.</div>"
      (encode-coding-string xemacs-chise-version 'utf-8-jp-er)))
    (princ "
</div>
</body>
</html>
")))
