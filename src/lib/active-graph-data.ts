"use client";

import { useAtomValue } from "jotai";
import { usePathname } from "next/navigation";
import { isKanjiRoute } from "@/lib/kanji-routing";
import { activeKanjiGraphAtom, graphClearedAtom } from "@/lib/store";

type ActiveGraphData = {
  kanjiInfo: KanjiInfo;
  graphData: BothGraphData;
};

// On a navigation that is not prefetched, the [id] route segment suspends
// while its RSC payload is fetched. During that window the new page's
// bridge has not mounted yet, so activeKanjiGraphAtom still holds the
// previous page's data (KanjiGraphBridge never nulls it on unmount). If a
// graph layer unmounted on that transient state, the canvas would be
// destroyed and a fresh WebGL context created on every such navigation.
// While the current route is a kanji route, return the atom's data as-is so
// the graph stays mounted and simply swaps its data in place when the new
// page resolves. Off kanji routes (home, about) the retained data is
// ignored and the layers unmount; on a 404 the cleared flag wins over
// everything, since an unknown id keeps the kanji-shaped route.
export function useActiveGraphData(): ActiveGraphData | null {
  const pathname = usePathname();
  const data = useAtomValue(activeKanjiGraphAtom);
  const cleared = useAtomValue(graphClearedAtom);

  if (cleared || !isKanjiRoute(pathname)) {
    return null;
  }

  return data;
}
