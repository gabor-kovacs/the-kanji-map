"use client";

import { useSetAtom } from "jotai";
import * as React from "react";
import { Header } from "@/components/header";
import { graphClearedAtom } from "@/lib/store";

export default function NotFound() {
  const setGraphCleared = useSetAtom(graphClearedAtom);

  // Tell the hoisted graph layers to forget the previous kanji's data,
  // which useActiveGraphData would otherwise retain on a kanji-shaped
  // route (unknown ids keep the single-segment [id] shape). A layout
  // effect, so the clear lands before this commit paints — a passive
  // effect would let the previous kanji's graph flash for a frame on
  // top of the 404 page. No cleanup: the flag stays set until the
  // destination kanji page's bridge clears it. Resetting it on unmount
  // would re-expose the retained graph over the loading page during
  // the next navigation's suspense window.
  React.useLayoutEffect(() => {
    setGraphCleared(true);
  }, [setGraphCleared]);

  return (
    <>
      <Header className="w-full" />
      <div className="size-full grid place-items-center">
        <h1 className="font-semibold text-xl">404 - Not Found</h1>
      </div>
    </>
  );
}
