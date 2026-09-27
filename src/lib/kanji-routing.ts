import { resolveKanjiId } from "@/lib/kanji-variants";

export const MOBILE_TAB_KEYS = [
  "kanji",
  "radical",
  "examples",
  "graph",
  "search",
] as const;

export type MobileTabKey = (typeof MOBILE_TAB_KEYS)[number];

// Text labels for the mobile carousel tabs, keyed by tab key. The
// "search" tab renders an icon instead, so it has no entry here; pages
// that build a tab list derive id/label from MOBILE_TAB_KEYS plus this
// map (and special-case the search icon), so the list can't drift from
// the order the graph layer indexes into.
export const MOBILE_TAB_LABELS: Record<Exclude<MobileTabKey, "search">, string> =
  {
    kanji: "漢字",
    radical: "部首",
    examples: "例",
    graph: "図",
  };

export const MOBILE_TAB_PARAM = "tab";

export const isMobileTabKey = (value: string): value is MobileTabKey =>
  MOBILE_TAB_KEYS.includes(value as MobileTabKey);

export const getMobileTabKey = (index: number): MobileTabKey =>
  MOBILE_TAB_KEYS[index] ?? MOBILE_TAB_KEYS[0];

export const getMobileTabIndex = (value: string | null | undefined) => {
  if (!value || !isMobileTabKey(value)) {
    return 0;
  }

  return MOBILE_TAB_KEYS.indexOf(value);
};

export const buildKanjiHref = (
  id: string,
  options?: {
    tab?: MobileTabKey | null;
  },
) => {
  const pathname = `/${encodeURIComponent(resolveKanjiId(id))}`;

  if (!options?.tab) {
    return pathname;
  }

  const params = new URLSearchParams({
    [MOBILE_TAB_PARAM]: options.tab,
  });

  return `${pathname}?${params.toString()}`;
};

// Single-segment routes that are not kanji pages. Unknown single-segment
// ids ARE kanji-shaped (they just end in a 404), so non-kanji routes must
// be listed explicitly rather than inverted. When you add a new
// single-segment route, add it here — otherwise useActiveGraphData will
// float the last kanji's graph over it.
const NON_KANJI_ROUTES = new Set(["about"]);

export const isKanjiRoute = (pathname: string): boolean => {
  const segments = pathname.split("/").filter(Boolean);
  return segments.length === 1 && !NON_KANJI_ROUTES.has(segments[0]);
};
