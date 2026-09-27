"use client";

import { useMediaQuery } from "react-responsive";
import { useActiveGraphData } from "@/lib/active-graph-data";
import { DESKTOP_GRAPH_LAYER } from "@/lib/graph-layout";
import { Graphs } from "./graphs";

// Rendered in the root layout. It overlays the graph cell of the [id] page
// (bottom row, right 60%, below the 330px top row) and stays mounted across
// kanji navigations, so the canvas survives and the graph morphs in place
// instead of being rebuilt for every new page. Mobile keeps its graph in the
// page's tab, so this layer is desktop-only.
export function GlobalGraphLayer() {
  const isMobile = useMediaQuery({ query: "(max-width: 767px)" });
  const data = useActiveGraphData();

  if (isMobile || !data) {
    return null;
  }

  return (
    <div className={DESKTOP_GRAPH_LAYER}>
      <Graphs kanjiInfo={data.kanjiInfo} graphData={data.graphData} />
    </div>
  );
}
