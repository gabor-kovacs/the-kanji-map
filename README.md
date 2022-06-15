## Preprocess

Kanji radical list from:
<https://github.com/sylhare/kanji>

This version of The Kanji Map uses [KanjiVG](https://kanjivg.tagaini.net/) for kanji part decomposition.
To create a static asset we need to process the XML file containing all kanji from their [Github releases](https://github.com/KanjiVG/kanjivg/releases).

```sh
node ./pages/api/create_composition.js
```
