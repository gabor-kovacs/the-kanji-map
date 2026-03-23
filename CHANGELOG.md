# Changelog

## 2026-03-23 - Version 6.3.0

- Improved mobile graph exploration: tapping a node now opens a bottom-sheet preview instead of immediately navigating away.
- Preserved mobile tab state in the URL, so opening kanji from the graph keeps the graph tab active on the next page and browser back navigation returns to the same tab.
- Added a dedicated mobile graph preview sheet with larger touch targets and cleaner tap behavior for switching between nodes.
- Added a local `Sheet` UI primitive for the mobile graph preview flow.

## 2026-03-20 - Version 6.2.0

- Added canonical kanji alias handling so compatibility forms resolve to a single canonical page while still exposing variants.
- Restored compatibility-form alias search and ensured alias-backed pages always use the current generated local data instead of stale committed JSON.
- Added a more robust preprocess pipeline with source normalization, retry handling, stale kanji JSON cleanup, and a single runner script for the full preprocess flow.
- Added fallback handling for unsupported Jisho radical pages so required entries stay in the dataset instead of silently disappearing.
- Improved radical pages and radical variants: alternative forms can link to their own pages when available, and variant radicals inherit the correct radical metadata and animation.
- Polished the graph and page UI with better kanji centering in 2D/3D graphs, clearer graph controls, and improved pointer/hover behavior for interactive elements.

## 2026-03-04 - Version 6.1.0

- Added filterable search with collection, JLPT level, and stroke count filters.
- Added a legend for graph colors and link behavior.
- Updated the kanji dataset and refreshed related UI dependencies.
- Removed the `animCJK` submodule from the repo layout.

## 2025-10-15 - Version 6.0.0

- Added Go script updates to keep Kangxi/CJK compatibility radicals consistent (thanks to [mochi-co/equivalent-unified-ideograph](https://github.com/mochi-co/equivalent-unified-ideograph)), avoiding issues like 忄 vs. 忄 being mixed.
- Reworked the data composition pipeline; radical alternative forms now link back to the original form.
- Updated the kanji dataset.
- Fixed a bug where kanji without stroke animations were not displayed.
- Fixed radicals and added alternative forms with position metadata.
- Added a custom font to display radicals (thanks to [KanjiVG](https://github.com/KanjiVG/kanjivg)).
