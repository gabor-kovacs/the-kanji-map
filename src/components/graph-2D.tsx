"use client";
import * as React from "react";

import ForceGraph2D, {
  ForceGraphMethods,
  GraphData,
  LinkObject,
  NodeObject,
} from "react-force-graph-2d";
import kanjilist from "@/../data/kanjilist.json";
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
}

type NodeObjectWithData = NodeObject & { data: KanjiInfo };

const Graph2D: React.FC<Props> = ({
  kanjiInfo,
  graphData,
  showOutLinks,
  showParticles,
  triggerFocus,
  bounds,
}) => {
  // group: el.g === 1 ? "joyo" : el.g === 2 ? "jinmeiyo" : "other",
  const joyoList = kanjilist.filter((el) => el.g === 1).map((el) => el.k);
  const jinmeiyoList = kanjilist.filter((el) => el.g === 2).map((el) => el.k);

  const { resolvedTheme } = useTheme();

  const fgRef: React.MutableRefObject<ForceGraphMethods | undefined> =
    React.useRef();

  const router = useRouter();

  const [data, setData] = React.useState<GraphData | undefined>({
    nodes: [],
    links: [],
  });

  React.useEffect(() => {
    setData(
      showOutLinks
        ? graphData?.withOutLinks
        : (graphData?.noOutLinks as unknown as GraphData)
    );
  }, [graphData?.noOutLinks, graphData?.withOutLinks, showOutLinks]);

  const handleClick = (node: NodeObject) => void router.push(`/${node.id}`);

  // prefetch routes for nodes visible in the graph
  React.useEffect(() => {
    data?.nodes?.forEach((node) => {
      void router.prefetch(`/${node.id}`);
    });
  }, [data, router]);
  // store the hovered node in a state
  const [hoverNode, setHoverNode] = React.useState<NodeObject | null>(null);

  const handleNodeHover = (node: NodeObject | null) => {
    setHoverNode(node || null);
    // paintNode(node);
  };

  const paintNode = (
    node: NodeObject,
    ctx: CanvasRenderingContext2D
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
      color = "#2B99CF";
    } else if (joyoList?.includes(String(node.id))) {
      color = "#80c2e2";
    } else if (jinmeiyoList?.includes(String(node.id))) {
      color = "#d5ebf5";
    } else {
      color = "#fff";
    }

    if (node.id === hoverNode?.id) {
      color = "#2B99CF";
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
    node.x && node.y && ctx.fillText(label, node.x, node.y);

    // node.__bckgDimensions = bckgDimensions; // to re-use in nodePointerAreaPaint
  };

  // find same onyomi
  const sameOn = (kanji1: string, kanji2: string) => {
    const k1 = data?.nodes?.find((o) => o.id === kanji1) as NodeObjectWithData;
    const k2 = data?.nodes?.find((o) => o.id === kanji2) as NodeObjectWithData;
    const on1: string[] | undefined = k1?.data?.jishoData?.onyomi;
    const on2: string[] | undefined = k2?.data?.jishoData?.onyomi;
    return on1?.filter((value) => on2?.includes(value)) ?? "";
  };

  // FOCUS  ON MAIN NODE AT START
  React.useEffect(() => {
    const focusMain = setTimeout(() => {
      if (kanjiInfo.id && data?.nodes?.length && data?.nodes?.length > 0) {
        fgRef?.current?.zoomToFit(1000, bounds.width * 0.1);
      }
    }, 100);
    return () => clearTimeout(focusMain);
  }, [data, kanjiInfo.id, triggerFocus, bounds]);

  return (
    <ForceGraph2D
      ref={fgRef}
      width={bounds.width}
      height={bounds.height}
      backgroundColor={"var(--color-background)"}
      graphData={data}
      nodeLabel={(n) => {
        const node = n as NodeObjectWithData;
        return `${node.data.jishoData?.kunyomi}<br/>${node.data.jishoData?.meaning}`;
      }}
      warmupTicks={10}
      onNodeClick={handleClick}
      nodeCanvasObject={paintNode}
      nodePointerAreaPaint={(node, color, ctx) => {
        const label = String(node.id);
        // const fontSize = 24 / globalScale;
        const fontSize = 6;
        ctx.font = `${fontSize}px Sans-Serif`;
        const textWidth = ctx.measureText(label).width;
        const bckgDimensions = [textWidth, fontSize].map(
          (n) => n + fontSize * 0.2
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
      linkColor={() =>
        getComputedStyle(document?.body)?.getPropertyValue("--color-foreground")
      }
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

          const linkText = sameOn(
            String(link.source.id),
            String(link.target.id)
          );

          ctx.beginPath();
          ctx.moveTo(link.source.x, link.source.y);
          ctx.lineTo(link.target.x, link.target.y);
          ctx.lineWidth = 0.25;
          ctx.strokeStyle = resolvedTheme === "dark" ? "#ffffff" : "#000000";
          ctx.stroke();

          const label = String(linkText);
          const fontSize = 4;
          ctx.font = `${fontSize}px Sans-Serif`;

          ctx.save();
          x && y && ctx.translate(x, y);
          ctx.textAlign = "center";
          ctx.textBaseline = "middle";
          ctx.fillStyle = resolvedTheme === "dark" ? "#ffffff" : "#000000";
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
            target.y - source.y
          );

          return (linkLength - 3) / linkLength;
        } else {
          return 0.8;
        }
      }}
      linkDirectionalParticles={3}
      linkDirectionalParticleSpeed={0.004}
      linkDirectionalParticleWidth={() => (showParticles ? 2 : 0)}
      linkDirectionalParticleColor={() =>
        resolvedTheme === "dark" ? "#ffffff" : "#000000"
      }
    />
  );
};

export default Graph2D;
