import { resolveKanjiId } from "@/lib/kanji-variants";

export const MOBILE_TAB_KEYS = [
  "kanji",
  "radical",
  "examples",
  "graph",
  "search",
] as const;

export type MobileTabKey = (typeof MOBILE_TAB_KEYS)[number];

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
