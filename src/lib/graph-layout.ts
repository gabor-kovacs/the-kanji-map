// The graph renders in the root layout (GlobalGraphLayer / MobileGraphLayer)
// instead of inside the [id] page, so the WebGL context survives kanji
// navigations. Those layers are fixed-positioned, so these class strings are
// the single source of truth for both a layer's rectangle and the page cell
// it overlays — the two must stay exactly in sync.
//
// Tailwind needs static class strings, so the shared values below are
// literal strings: change them here, not at a usage site.
//
// Desktop: the page is a two-row grid below the 3rem (h-12) header — a
// 330px top row and a bottom row split 2fr (examples) / 3fr (graph).
//   DESKTOP_GRAPH_LAYER  top-[calc(3rem+330px)] = header (3rem) + top row (330px)
//                        left-[40%]            = 2fr of the 2fr+3fr bottom row
// Mobile: the graph tab of the mobile carousel. The carousel is full-width
// below the 3rem header and reserves its tab bar with pb-11; the layer
// mirrors that inset with bottom-11 (the two values must stay equal).
// 44px covers the measured tab-bar height (~43.4px) so the layer never
// overlaps the bar's top edge and swallows taps meant for the tabs.

// The header height (3rem); also the top offset of both graph layers. Used
// by both the desktop and the mobile header, hence the neutral name.
export const HEADER_HEIGHT = "h-12";

// The page grid shared by the [id] page, its server placeholder, and the
// home page.
export const DESKTOP_TOP_ROW = "md:grid-rows-[330px_1fr]";
export const DESKTOP_BOTTOM_ROW = "grid-cols-[2fr_3fr]";

export const DESKTOP_GRAPH_CELL = "border-l";
export const DESKTOP_GRAPH_LAYER =
  "fixed top-[calc(3rem+330px)] right-0 bottom-0 left-[40%] z-10 overflow-hidden";

export const MOBILE_GRAPH_CELL = "size-full";
export const MOBILE_GRAPH_LAYER =
  "fixed inset-x-0 top-[3rem] bottom-11 z-10 overflow-hidden will-change-transform";
export const MOBILE_TAB_BAR_INSET = "pb-11";
