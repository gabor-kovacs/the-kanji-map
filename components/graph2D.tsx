import React, { useState, useEffect, useRef } from "react";

import ForceGraph2D, { LinkObject, NodeObject } from "react-force-graph-2d";
import type { ForceGraphMethods, GraphData } from "react-force-graph-2d";

import { joyoList } from "../data/joyo";
import { jinmeiyoList } from "../data/jinmeiyo";

import SpriteText from "three-spritetext";
import { useRouter } from "next/router";
import { useTheme } from "next-themes";
import type { KanjiParseResult } from "unofficial-jisho-api";

import type { RectReadOnly } from "react-use-measure";

type KanjiInfo = {
  id: string;
  kanjialiveData?: any;
  jishoData?: KanjiParseResult | null;
};

interface Props {
  kanjiInfo: KanjiInfo;
  graphData: any;
  showOutLinks: boolean;
  triggerFocus: number;
  bounds: RectReadOnly;
}

type NodeObjectWithData = NodeObject & { data: KanjiInfo };

const Graph2D: React.FC<Props> = ({
  kanjiInfo,
  graphData,
  showOutLinks,
  triggerFocus,
  bounds,
}) => {
  const { theme } = useTheme();
  const fg2DRef: React.MutableRefObject<ForceGraphMethods | undefined> =
    useRef();

  const router = useRouter();

  const [data, setData] = useState<GraphData>({
    nodes: [],
    links: [],
  });

  useEffect(() => {
    setData(
      showOutLinks
        ? graphData.withOutLinks
        : (graphData.noOutLinks as unknown as GraphData)
    );
  }, [graphData.noOutLinks, graphData.withOutLinks, showOutLinks]);

  const handleClick = (node: NodeObject) => {
    router.push(`/kanji/${node.id}`);
  };

  // prefetch routes for nodes visible in the graph
  useEffect(() => {
    data?.nodes?.forEach((node) => {
      router.prefetch(`/kanji/${node.id}`);
    });
  }, [data, router]);

  // store the hovered node in a state
  const [hoverNode, setHoverNode] = useState<NodeObject | null>(null);

  const handleNodeHover = (node: NodeObject | null) => {
    setHoverNode(node || null);
    // paintNode(node);
  };

  const paintNode = (
    node: NodeObject,
    ctx: CanvasRenderingContext2D,
    globalScale: number
  ) => {
    // console.log(`hovernode: ${hoverNode?.id}`);
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
    const onyomiOverlap = on1?.filter((value) => on2?.includes(value));
    return onyomiOverlap;
  };

  // FOCUS  ON MAIN NODE AT START
  useEffect(() => {
    const focusMain = setTimeout(() => {
      if (kanjiInfo.id && data?.nodes?.length > 0) {
        fg2DRef?.current?.zoomToFit(1000, bounds.width * 0.1);
      }
    }, 100);
    return () => clearTimeout(focusMain);
  }, [data, kanjiInfo.id, triggerFocus, bounds]);

  return (
    <ForceGraph2D
      ref={fg2DRef}
      width={bounds.width}
      height={bounds.height - 50}
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
          ctx.strokeStyle = theme === "dark" ? "#ffffff" : "#000000";
          ctx.stroke();

          const label = String(linkText);
          const fontSize = 4;
          ctx.font = `${fontSize}px Sans-Serif`;

          ctx.save();
          x && y && ctx.translate(x, y);
          ctx.textAlign = "center";
          ctx.textBaseline = "middle";
          ctx.fillStyle = theme === "dark" ? "#ffffff" : "#000000";
          ctx.fillText(label, 0, 0);
          ctx.restore();
        }
      }}
      linkDirectionalArrowLength={4}
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

          const relPos = (linkLength - 3) / linkLength;
          return relPos;
        } else {
          return 0.8;
        }
      }}
      linkDirectionalParticles={3}
      linkDirectionalParticleSpeed={0.008}
      linkDirectionalParticleWidth={2}
    />
  );
};

export default Graph2D;
