"use client";

import { useMediaQuery } from "react-responsive";
import { MOBILE_TAB_KEYS } from "@/lib/kanji-routing";
import { onMobileCarouselProgress } from "@/lib/mobile-carousel-progress";
import { useActiveGraphData } from "@/lib/active-graph-data";
import { MOBILE_GRAPH_LAYER } from "@/lib/graph-layout";
import { Graphs } from "./graphs";
import * as React from "react";

// The [id] page subtree remounts on every kanji navigation (Next.js keys
// segment subtrees by param value), which destroyed the mobile graph's WebGL
// context on each page change. Like the desktop GlobalGraphLayer, the graph
// therefore lives in the root layout and only swaps its data in place. It
// overlays the mobile carousel's graph tab and *moves with it*: each frame
// the carousel publishes its scroll progress, and this layer translates by
// the graph slide's offset. Slides are equal-width and the track has no
// gutters, so the transform exactly mirrors the slide's own motion — the
// graph slides in from the edge together with its tab instead of fading in
// on top of the still-animating content. When fully off-screen the layer
// collapses to display:none, which also pauses the engine via the graph's
// IntersectionObserver.
export function MobileGraphLayer() {
  const isMobile = useMediaQuery({ query: "(max-width: 767px)" });
  const data = useActiveGraphData();
  const ref = React.useRef<HTMLDivElement>(null);

  React.useLayoutEffect(() => {
    if (!isMobile || !data) {
      return;
    }

    const count = MOBILE_TAB_KEYS.length;
    const graphIndex = MOBILE_TAB_KEYS.indexOf("graph");

    return onMobileCarouselProgress((progress) => {
      const el = ref.current;
      if (!el) {
        return;
      }

      // Slides are equal-width, so scroll progress maps linearly to a
      // continuous slide position (0 = first tab ... count-1 = last tab).
      // The graph slide sits (graphIndex - position) slide-widths away from
      // the left edge; in percent of this element's own width (one slide)
      // that is the exact transform the real slide receives each frame.
      const position = progress * (count - 1);
      const distance = Math.abs(position - graphIndex);
      const visible = distance < 1;

      // Fully written each frame; no CSS transition, since the carousel
      // already interpolates the values smoothly. Pointer events only once
      // settled, so a sliding-in graph never swallows carousel touches.
      el.style.display = visible ? "block" : "none";
      el.style.pointerEvents = distance < 0.01 ? "auto" : "none";
      el.style.transform = `translate3d(${(graphIndex - position) * 100}%, 0, 0)`;
    });
  }, [isMobile, data]);

  if (!isMobile || !data) {
    return null;
  }

  // Starts hidden off-screen at the first-tab position; the effect above
  // applies the real state before the first paint. The style object must
  // stay constant across renders so React never overwrites the values
  // written to the DOM above.
  return (
    <div
      ref={ref}
      className={MOBILE_GRAPH_LAYER}
      style={{ display: "none", transform: "translate3d(300%, 0, 0)" }}
    >
      <Graphs
        kanjiInfo={data.kanjiInfo}
        graphData={data.graphData}
        enableNodePreview
        navigationTab="graph"
      />
    </div>
  );
}
