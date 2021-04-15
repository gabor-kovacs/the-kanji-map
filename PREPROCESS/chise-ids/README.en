;; -*- coding: utf-8-jis-er -*-
This directory holds the CHISE-IDS package which contains data and
utilities about structures of Han Ideographs (漢字).


* How to install

Please install XEmacs CHISE before install this package.

If XEmacs CHISE is installed in your system, please type

    % make install

in the directory of the CHISE-IDS distribution.


[Note] If you don't have XEmacs CHISE, the CHISE-base package may be
useful.  It is an installer package to install CHISE functionalities
including libchise, XEmacs CHISE, the CHISE-IDS package, some other
Emacs Lisp utilities, some fonts, etc.  It is available at:

	http://www.chise.org/dist/base/

In addition, if you use Mac OS X with Fink,

http://corpus.kanji.zinbun.kyoto-u.ac.jp/cgi-bin/gitweb.cgi?p=corpus/env.git;a=blob_plain;f=install-base-system_mac-fink.sh;hb=HEAD

is available and easier than plain CHISE-base package.


* IDS files

The following files contains the data about structures of Han
Ideographs (漢字).
    
    IDS-UCS-Basic.txt	CJK Unified Ideographs (U+4E00 〜 U+9FA5)
			of ISO/IEC 10646-1:2000
    IDS-UCS-Ext-A.txt	CJK Unified Ideographs Extension A
			(U+3400 〜 U+4DB5, U+FA1F and U+FA23)
			of ISO/IEC 10646-1:2000
    IDS-UCS-Compat.txt	CJK Compatibility Ideographs
			(U+F900 〜 U+FA2D, except U+FA1F and U+FA23)
			of ISO/IEC 10646-1:2000
    IDS-UCS-Ext-B-1.txt	CJK Unified Ideographs Extension B [part 1]
			(U-00020000 〜 U-00021FFF)
			of ISO/IEC 10646-2:2001
    IDS-UCS-Ext-B-2.txt	CJK Unified Ideographs Extension B [part 2]
			(U-00022000 〜 U-00023FFF)
			of ISO/IEC 10646-2:2001
    IDS-UCS-Ext-B-3.txt	CJK Unified Ideographs Extension B [part 3]
			(U-00024000 〜 U-00025FFF)
			of ISO/IEC 10646-2:2001
    IDS-UCS-Ext-B-4.txt	CJK Unified Ideographs Extension B [part 4]
			(U-00026000 〜 U-00027FFF)
			of ISO/IEC 10646-2:2001
    IDS-UCS-Ext-B-5.txt	CJK Unified Ideographs Extension B [part 5]
			(U-00028000 〜 U-00029FFF)
			of ISO/IEC 10646-2:2001
    IDS-UCS-Ext-B-6.txt	CJK Unified Ideographs Extension B [part 6]
			(U-0002A000 〜 U-0002A6D6)
			of ISO/IEC 10646-2:2001
    IDS-UCS-Compat-Supplement.txt
			CJK Compatibility Ideographs Supplement
			(U-0002F800 〜 U-0002FA1D)
			of ISO/IEC 10646-2:2001

These files are encoded by UTF-8.  The format of each line of the
files is:

    <CODEPOINT><TAB><CHARACTER><TAB><IDS>

or

    ;; <COMMENTS>

Each element means

    <TAB>	<HORIZONTAL TABULATION> (U+0009)
    <CODEPOINT>	code point
		U+hhhh		Hex form of UCS code point
				(U+0000 〜 U+FFFF)
		U-hhhhhhhh	Hex form of UCS code point
				(U+00000000 〜 U+7FFFFFFF)
    <CHARACTER>	character corresponding with <CODEPOINT>
    <IDS>	Ideographic Description Sequence
		(based on ISO/IEC 10646-1:2000 F.3.1; however
		Compatibility Ideographs and non-UCS Ideographs are
		also allowed)
    <COMMENTS>	comment


* License

This package is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This package is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this package; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.


* Acknowledgment

The developing of the package was supported by the “Exploratory
Software Project” of Information-technology Promotion Agency, Japan.
Some data in the IDS-UCS* files are derived and expanded from the CDP
database developped by C.C. Hsieh and his team at Academia Sinica in
Taipei, Taiwan.
