"use client";

import * as React from "react";

import ForceGraph2D, {
  ForceGraphMethods,
  GraphData,
  LinkObject,
  NodeObject,
} from "react-force-graph-2d";
import kanjilist from "@/../data/kanjilist.json";
import { buildKanjiHref, type MobileTabKey } from "@/lib/kanji-routing";
import { escapeHtml } from "@/lib/utils";
import {
  NODE_SELECTED,
  NODE_JOYO,
  NODE_JINMEIYO,
  NODE_OTHER,
} from "@/lib/graph-colors";
import type { RectReadOnly } from "react-use-measure";
import { useRouter } from "next/navigation";
import { useTheme } from "next-themes";

interface Props {
  kanjiInfo: KanjiInfo;
  graphData: BothGraphData | null;
  showOutLinks: boolean;
  showParticles: boolean;
  triggerFocus: number;
  bounds: RectReadOnly;
  navigationTab?: MobileTabKey;
  enableNodePreview?: boolean;
  onPreviewNode?: (node: GraphNode) => void;
  onClosePreview?: () => void;
}

type NodeObjectWithData = NodeObject & { data: GraphNodeData | null };

const KANJI_TEXT_OFFSET_Y = 0.5;

const KANJI_GROUPS = kanjilist.reduce(
  (groups, entry) => {
    if (entry.g === 1) {
      groups.joyo.add(entry.k);
    } else if (entry.g === 2) {
      groups.jinmeiyo.add(entry.k);
    }

    return groups;
  },
  {
    joyo: new Set<string>(),
    jinmeiyo: new Set<string>(),
  },
);

const Graph2D: React.FC<Props> = ({
  kanjiInfo,
  graphData,
  showOutLinks,
  showParticles,
  triggerFocus,
  bounds,
  navigationTab,
  enableNodePreview = false,
  onPreviewNode,
  onClosePreview,
}) => {
  const { resolvedTheme } = useTheme();
  const { push, prefetch } = useRouter();

  const fgRef: React.MutableRefObject<ForceGraphMethods | undefined> =
    React.useRef(undefined);

  const data = React.useMemo<GraphData | undefined>(
    () =>
      showOutLinks
        ? graphData?.withOutLinks
        : (graphData?.noOutLinks as unknown as GraphData),
    [graphData?.noOutLinks, graphData?.withOutLinks, showOutLinks],
  );

  const buildNodeHref = React.useCallback(
    (id: string) =>
      buildKanjiHref(id, {
        tab: navigationTab ?? null,
      }),
    [navigationTab],
  );

  const handleClick = (node: NodeObject) => {
    const nodeId = String(node.id);

    if (enableNodePreview && onPreviewNode) {
      onPreviewNode({
        id: nodeId,
        data: (node as NodeObjectWithData).data ?? null,
      });
      return;
    }

    void push(buildNodeHref(nodeId));
  };

  // store the hovered node in a state
  const [hoverNode, setHoverNode] = React.useState<NodeObject | null>(null);

  const handleNodeHover = (node: NodeObject | null) => {
    setHoverNode(node || null);
    if (node) {
      void prefetch(buildNodeHref(String(node.id)));
    }
  };

  const paintNode = (
    node: NodeObject,
    ctx: CanvasRenderingContext2D,
    // globalScale: number
  ) => {
    const label = String(node.id);
    const fontSize = 6;
    ctx.font = `${fontSize}px Sans-Serif`;
    const textWidth = ctx.measureText(label).width;
    const bckgDimensions = [textWidth, fontSize].map((n) => n + fontSize * 0.2); // some padding

    let color;
    // if it is he main node
    if (node.id === kanjiInfo.id) {
      color = NODE_SELECTED;
    } else if (KANJI_GROUPS.joyo.has(String(node.id))) {
      color = NODE_JOYO;
    } else if (KANJI_GROUPS.jinmeiyo.has(String(node.id))) {
      color = NODE_JINMEIYO;
    } else {
      color = NODE_OTHER;
    }

    if (node.id === hoverNode?.id) {
      color = NODE_SELECTED;
    }

    const radius = (bckgDimensions[1] / 2) * 1.5;

    ctx.beginPath();
    node.x &&
      node.y &&
      ctx.arc(node.x, node.y, radius * 1.1, 0, 2 * Math.PI, false);
    ctx.fillStyle = "#000000";
    ctx.fill();

    ctx.beginPath();
    node.x && node.y && ctx.arc(node.x, node.y, radius, 0, 2 * Math.PI, false);
    ctx.fillStyle = color;
    ctx.fill();

    ctx.textAlign = "center";
    ctx.textBaseline = "middle";
    ctx.fillStyle = "black";
    node.x &&
      node.y &&
      ctx.fillText(label, node.x, node.y + KANJI_TEXT_OFFSET_Y);

    // node.__bckgDimensions = bckgDimensions; // to re-use in nodePointerAreaPaint
  };

  // Precompute the shared onyomi label for every link once per data change,
  // so the per-frame canvas paint doesn't scan the node list
  const linkLabelByLink = React.useMemo(() => {
    const onyomiById = new Map<string, string[]>();
    data?.nodes?.forEach((node) => {
      const onyomi = (node as NodeObjectWithData).data?.onyomi;
      if (onyomi?.length) {
        onyomiById.set(String(node.id), onyomi);
      }
    });

    const labels = new Map<LinkObject, string>();
    data?.links?.forEach((link) => {
      const source =
        typeof link.source === "object" ? link.source.id : link.source;
      const target =
        typeof link.target === "object" ? link.target.id : link.target;
      const on1 = onyomiById.get(String(source));
      const on2 = onyomiById.get(String(target));
      const shared = on1 && on2 ? on1.filter((value) => on2.includes(value)) : [];
      labels.set(link, shared.join(","));
    });

    return labels;
  }, [data]);

  // Hex, not a computed CSS variable: next-themes applies the new theme to
  // the DOM in a post-state effect, so a render-phase read is one theme
  // stale after a manual switch. Matches the arrow and particle accessors.
  const foregroundColor = resolvedTheme === "dark" ? "#ffffff" : "#000000";

  // The container reports 0x0 before the first layout (and while the graph
  // layer is hidden on mobile), so the engine is only mounted once it has
  // real dimensions to be born into.
  const boundsReady = bounds.width > 0 && bounds.height > 0;

  // The user has taken over the framing since the last fit request; a
  // delayed re-fit must not override their view.
  const userZoomedRef = React.useRef(false);
  const lastTriggerFocusRef = React.useRef(triggerFocus);

  // FOCUS ON GRAPH — re-fit only when the graph content (or an explicit
  // focus request) changes, never when just the container size changes: on
  // mobile the layer is display:none until the graph tab comes on screen, so
  // the container grows 0 -> full size on reveal and must not re-zoom an
  // unchanged graph (the 3D view already behaves this way). If the content
  // changes while hidden, the fit is deferred until the first reveal.
  const fitRef = React.useRef<() => void>(() => {});
  fitRef.current = () => {
    // Measure live: the reveal can outrun the useMeasure re-render, and the
    // bounds prop would then still be the stale 0x0 size
    const rect = containerRef.current?.getBoundingClientRect();
    const width = rect?.width ?? 0;
    const height = rect?.height ?? 0;
    if (kanjiInfo.id && data?.nodes?.length && width > 0) {
      const fg = fgRef.current;
      // A layout that has no coordinates yet would yield a NaN bbox and
      // wreck the view, so skip the fit in that case.
      const bbox = fg?.getGraphBbox();
      if (
        !fg ||
        !bbox ||
        ![bbox.x[0], bbox.x[1], bbox.y[0], bbox.y[1]].every(Number.isFinite)
      ) {
        return;
      }
      userZoomedRef.current = false;
      // Fit the (mostly) settled layout with modest padding.
      fg.zoomToFit(500, Math.min(width, height) * 0.05);
    }
  };

  const pendingFitRef = React.useRef(false);
  const isOnScreenRef = React.useRef(false);
  // The engine instance once it is mounted, so the unmount cleanup below can
  // pause it; React nulls fgRef before running effect cleanups, so the
  // cleanup must not read that ref.
  const mountedEngineRef = React.useRef<ForceGraphMethods | undefined>(
    undefined,
  );

  React.useEffect(() => {
    // An explicit focus request (the fit button) always wins over a view
    // the user has taken over; any other trigger (a data swap) re-arms the
    // fit so the new graph is framed unless the user grabs the view within
    // the delay window.
    const explicit = triggerFocus !== lastTriggerFocusRef.current;
    lastTriggerFocusRef.current = triggerFocus;
    pendingFitRef.current = true;
    userZoomedRef.current = false;
    if (!isOnScreenRef.current) {
      return;
    }
    const focusMain = setTimeout(() => {
      if (!pendingFitRef.current) {
        // A deferred fit (reveal or engine mount) already consumed it.
        return;
      }
      pendingFitRef.current = false;
      if (!explicit && userZoomedRef.current) {
        return;
      }
      fitRef.current();
    }, 100);
    return () => clearTimeout(focusMain);
  }, [data, kanjiInfo.id, triggerFocus]);

  // Stop the render loop when the graph is not on screen (e.g. its tab is
  // off-screen on mobile) and resume it when it comes back
  const containerRef = React.useRef<HTMLDivElement>(null);
  React.useEffect(() => {
    const container = containerRef.current;
    if (!container || typeof IntersectionObserver === "undefined") {
      return;
    }

    const observer = new IntersectionObserver(
      ([entry]) => {
        if (!entry) {
          return;
        }
        const engine = fgRef.current;
        if (entry.isIntersecting) {
          isOnScreenRef.current = true;
          engine?.resumeAnimation();
          // The engine can mount after this reveal (it only exists once the
          // container has real dimensions); don't consume the deferred fit
          // for an engine that isn't there yet — the fit-on-mount effect
          // below runs it instead.
          if (engine && pendingFitRef.current) {
            pendingFitRef.current = false;
            fitRef.current();
          }
        } else {
          isOnScreenRef.current = false;
          engine?.pauseAnimation();
        }
      },
      { threshold: 0 },
    );

    observer.observe(container);
    return () => observer.disconnect();
  }, []);

  // The engine mounts only once the container has real dimensions, which on
  // mobile lands right around the IntersectionObserver reveal, so run the
  // deferred fit once the engine appears (the 100ms settle mirrors the
  // focus effect above). Keyed on boundsReady, not fgRef.current: the ref
  // is attached during commit, after this render's dependency array was
  // computed, so it is still undefined on the render that mounts the engine
  // and the effect would never re-run on it.
  React.useEffect(() => {
    mountedEngineRef.current = fgRef.current;
    if (!fgRef.current || !pendingFitRef.current || !isOnScreenRef.current) {
      return;
    }
    const runFit = setTimeout(() => {
      pendingFitRef.current = false;
      fitRef.current();
    }, 100);
    return () => clearTimeout(runFit);
  }, [boundsReady]);

  // onZoom only reports the transform (d3-zoom's sourceEvent is stripped
  // before it is handed out), so detect user gestures on the canvas
  // directly: pointer-down, wheel and touch all mean the user is taking
  // over the framing. Keyed on boundsReady, not fgRef.current: the canvas
  // exists once the engine is mounted, and the ref is only attached after
  // this render's dependency array was computed.
  React.useEffect(() => {
    const canvas = containerRef.current?.querySelector("canvas");
    if (!canvas) {
      return;
    }
    const onUserGesture = () => {
      userZoomedRef.current = true;
    };
    canvas.addEventListener("pointerdown", onUserGesture);
    canvas.addEventListener("wheel", onUserGesture, { passive: true });
    canvas.addEventListener("touchstart", onUserGesture, { passive: true });
    return () => {
      canvas.removeEventListener("pointerdown", onUserGesture);
      canvas.removeEventListener("wheel", onUserGesture);
      canvas.removeEventListener("touchstart", onUserGesture);
    };
  }, [boundsReady]);

  // The kapsule engine outlives the React component, so stop its animation
  // loop when the component unmounts (e.g. leaving a kanji route or a
  // breakpoint change). The engine instance is read from mountedEngineRef
  // (set in the fit-on-mount effect), because React nulls fgRef before
  // running effect cleanups.
  React.useEffect(() => {
    return () => {
      mountedEngineRef.current?.pauseAnimation();
    };
  }, []);

  if (!graphData || !kanjiInfo || !data) return <></>;

  return (
    <div ref={containerRef} className="size-full">
      {boundsReady && (
      <ForceGraph2D
      ref={fgRef}
      width={bounds.width}
      height={bounds.height}
      backgroundColor={"#00000000"}
      graphData={data}
      nodeLabel={(n) => {
        if (enableNodePreview) {
          return "";
        }

        const node = n as NodeObjectWithData;
        if (!node.data) {
          return "";
        }
        const kunyomi = node.data.kunyomi.join(", ");
        const meaning = node.data.meaning;
        // Don't show tooltip if both kunyomi and meaning are empty
        if (!kunyomi && !meaning) {
          return "";
        }
        return `${escapeHtml(kunyomi)}<br/>${escapeHtml(meaning)}`;
      }}
      warmupTicks={60}
      onNodeClick={handleClick}
      onBackgroundClick={() => {
        if (enableNodePreview) {
          onClosePreview?.();
        }
      }}
      nodeCanvasObject={paintNode}
      nodePointerAreaPaint={(node, color, ctx) => {
        const label = String(node.id);
        // const fontSize = 24 / globalScale;
        const fontSize = 6;
        ctx.font = `${fontSize}px Sans-Serif`;
        const textWidth = ctx.measureText(label).width;
        const bckgDimensions = [textWidth, fontSize].map(
          (n) => n + fontSize * 0.2,
        ); // some padding
        // const bckgDimensions = node.__bckgDimensions;
        const radius = (bckgDimensions[1] / 2) * 1.5;

        ctx.beginPath();
        node.x &&
          node.y &&
          ctx.arc(node.x, node.y, radius, 0, 2 * Math.PI, false);
        ctx.fillStyle = color;
        ctx.fill();
      }}
      onNodeHover={(node) => handleNodeHover(node)}
      linkCanvasObject={(link: LinkObject, ctx: CanvasRenderingContext2D) => {
        if (
          typeof link.source === "object" &&
          typeof link.target === "object" &&
          link.source.x &&
          link.target.x &&
          link.source.y &&
          link.target.y
        ) {
          const x = (link.source.x + link.target.x) / 2;
          const y = (link.source.y + link.target.y) / 2;

          const label = linkLabelByLink.get(link) ?? "";

          ctx.beginPath();
          ctx.moveTo(link.source.x, link.source.y);
          ctx.lineTo(link.target.x, link.target.y);
          ctx.lineWidth = 0.25;
          ctx.strokeStyle = foregroundColor;
          ctx.stroke();

          const fontSize = 4;
          ctx.font = `${fontSize}px Sans-Serif`;

          ctx.save();
          x && y && ctx.translate(x, y);
          ctx.textAlign = "center";
          ctx.textBaseline = "middle";
          ctx.fillStyle = foregroundColor;
          ctx.fillText(label, 0, 0);
          ctx.restore();
        }
      }}
      linkDirectionalArrowLength={4}
      linkDirectionalArrowColor={() =>
        resolvedTheme === "dark" ? "#ffffff" : "#000000"
      }
      linkDirectionalArrowRelPos={({ source, target }) => {
        if (
          typeof source === "object" &&
          typeof target === "object" &&
          source.x &&
          target.x &&
          source.y &&
          target.y
        ) {
          const linkLength = Math.hypot(
            target.x - source.x,
            target.y - source.y,
          );
          if (!linkLength) {
            return 0.8;
          }

          // Clamp: while the layout is still settling, short links would
          // otherwise place the arrow head outside the line
          return Math.max(0, Math.min(1, (linkLength - 3) / linkLength));
        } else {
          return 0.8;
        }
      }}
      // 0 photons when particles are disabled so the canvas can auto-pause
      // once the layout settles, instead of repainting every frame
      linkDirectionalParticles={showParticles ? 3 : 0}
      linkDirectionalParticleSpeed={0.004}
      linkDirectionalParticleWidth={() => (showParticles ? 2 : 0)}
      linkDirectionalParticleColor={() =>
        resolvedTheme === "dark" ? "#ffffff" : "#000000"
      }
      />
      )}
    </div>
  );
};

export default Graph2D;
