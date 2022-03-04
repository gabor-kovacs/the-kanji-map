# -*- coding: utf-8 -*-
import wget
import io
import requests
import urllib.request

from keep import keep

for kanji in keep:
    if (len(kanji.encode('utf-16-le')) == 4):
        glyph = str(kanji.encode('unicode_escape'))
        glyph = glyph.replace(r"\U000", "u")
        glyph = glyph.replace("\\", "")
        glyph = glyph.replace("b", "")
        glyph = glyph.replace("'", "")

        # print(glyph)

        url = 'http://en.glyphwiki.org/glyph/' + glyph + '.svg'
        r = requests.get(url, allow_redirects=True)
        print(url)
        try:
            # wget.download(url, './glyphs/' + kanji + '.svg')
            open('./glyphs/' + glyph + '.svg', 'wb').write(r.content)
            print("success")
            # urllib.request.urlretrieve(url, './glyphs/' + kanji + '.svg')
        except:
            print("failed")
            pass  
        
    
    # if (len(kanji.encode('utf-16-le')) > 4):
    #     kanji = kanji.lower()
    #     # print(str(len(kanji)) + " " + kanji )
    #     url = 'http://en.glyphwiki.org/glyph/' + kanji + '.svg'
    #     r = requests.get(url, allow_redirects=True)
    #     print(url)
    #     try:
    #         # wget.download(url, './glyphs/' + kanji + '.svg')
    #         open('./glyphs/' + kanji + '.svg', 'wb').write(r.content)
    #         print("success")
    #         # urllib.request.urlretrieve(url, './glyphs/' + kanji + '.svg')
    #     except:
    #         print("failed")
    #         pass
