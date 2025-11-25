# The Kanji Map

[![Netlify Status](https://api.netlify.com/api/v1/badges/60c6e34d-30e3-480f-8e53-8848ab7b678d/deploy-status)](https://app.netlify.com/sites/thekanjimap/deploys)

The Kanji Map is a Japanese language learning tool that shows kanji information and decomposition in graph form.

### Try it online at [thekanjimap.com](https://thekanjimap.netlify.app) 

or [thekanjimap.netlify.app](https://thekanjimap.netlify.app) (backup)

## Changelog

- **2025-10-15 – Version 6.0.0**
  - Added Go script updates to keep Kangxi/CJK compatibility radicals consistent (thanks to [mochi-co/equivalent-unified-ideograph](https://github.com/mochi-co/equivalent-unified-ideograph)), avoiding issues like 忄 vs. ⺖ being mixed.
  - Reworked the data composition pipeline; radical alternative forms now link back to the original form.
  - Updated the kanji dataset.
  - Fixed a bug where kanji without stroke animations were not displayed.
  - Fixed radicals and added alternative forms with position metadata.
  - Added a custom font to display radicals (thanks to [KanjiVG](https://github.com/KanjiVG/kanjivg)).


## Kanji information

Displayed kanji information (where available):

- Type: jōyō kanji (taught in school), jinmeiyō kanji (used in names) or neither
- JLPT (Japanese-Language Proficiency) Test level</li>
- Frequency rank out of 2500 most used kanji found in newspapers
- Stroke count
- Meaning
- Kunyomi (Japanese reading of the kanji)
- Onnyomi (Chinese/Sino-Japanese reading of the kanji)
- Examples with audio, kunyomi and onyomi
- Radical with kunyomi and meaning

## Credits

- Kanji and decomposition is based on [KanjiVG](https://github.com/KanjiVG/kanjivg), released under the Creative Commons Attribution-Share Alike 3.0 licence.
- List of radicals are provided by [github.com/sylhare/kanji](https://github.com/sylhare/kanji), licensed under MIT.
- Stroke animations are provided by [animCJK](https://github.com/parsimonhi/animCJK), released under the Arphic Public License.
- Kanji, examples and radical information is provided by [JISHO.org](https://jisho.org), sourcing from multiple open source [dictionaries](https://jisho.org/about) and [Kanji alive](https://kanjialive.com/), released under CC 4.0.
- Graph is created using [react-force-graph](https://github.com/vasturiano/react-force-graph) and [three-spritetext](https://github.com/vasturiano/three-spritetext), released under the MIT license.
- Handwritten kanji recognition uses [handwriting.js](https://github.com/ChenYuHo/handwriting.js), released under the MIT license.


## Donations

[![donate](https://www.paypalobjects.com/en_US/i/btn/btn_donate_SM.gif)](https://www.paypal.com/donate?hosted_button_id=U867B8RRZUN7E)

If this project was useful for you and you would like to contribute, you can always [Donate.](https://www.paypal.com/donate?hosted_button_id=U867B8RRZUN7E)

Donations are used to pay for hosting, maintenance costs and improvements.
