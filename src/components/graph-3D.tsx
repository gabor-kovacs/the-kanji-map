"use client";

import kanjilist from "@/../data/kanjilist.json";
import { buildKanjiHref, type MobileTabKey } from "@/lib/kanji-routing";
import { escapeHtml } from "@/lib/utils";
import {
  NODE_SELECTED,
  NODE_JOYO,
  NODE_JINMEIYO,
  NODE_OTHER,
} from "@/lib/graph-colors";
import { useTheme } from "next-themes";
import { useRouter } from "next/navigation";
import * as React from "react";
import type { ForceGraphMethods, GraphData } from "react-force-graph-3d";
import ForceGraph3D, { LinkObject, NodeObject } from "react-force-graph-3d";
import type { RectReadOnly } from "react-use-measure";
import * as THREE from "three";
import SpriteText from "three-spritetext";

type NodeObjectWithData = NodeObject & { data: GraphNodeData | null };

interface Props {
  kanjiInfo: KanjiInfo;
  graphData: BothGraphData | null;
  showOutLinks: boolean;
  triggerFocus: number;
  bounds: RectReadOnly;
  autoRotate: boolean;
  showParticles: boolean;
  navigationTab?: MobileTabKey;
  enableNodePreview?: boolean;
  onPreviewNode?: (node: GraphNode) => void;
  onClosePreview?: () => void;
}

const KANJI_SPRITE_OFFSET_Y = 2.0;

const NODE_GEOMETRY = new THREE.SphereGeometry(8, 32, 32);

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

const getNodeDefaultColor = (nodeId: string, selectedId: string) => {
  if (nodeId === selectedId) {
    return NODE_SELECTED;
  }
  if (KANJI_GROUPS.joyo.has(String(nodeId))) {
    return NODE_JOYO;
  }
  if (KANJI_GROUPS.jinmeiyo.has(String(nodeId))) {
    return NODE_JINMEIYO;
  }

  return NODE_OTHER;
};

const resetNodeColor = (node: any, selectedId: string) => {
  const defaultColor = getNodeDefaultColor(node.id, selectedId);
  if (node?.__threeObj?.children[1]?.material?.color) {
    node.__threeObj.children[1].material.color.set(defaultColor);
  }
};

const highlightNode = (node: any) => {
  if (node?.__threeObj?.children[1]?.material?.color) {
    const color = node.__threeObj.children[1].material.color;
    node.__threeObj.children[1].material.color.setRGB(
      color.r * 0.8,
      color.g * 0.8,
      color.b * 0.8,
    );
  }
};

const getSharedOnyomi = (data: GraphData | null, kanji1: string, kanji2: string) => {
  const k1 = data?.nodes?.find((o) => o?.id === kanji1) as NodeObjectWithData;
  const k2 = data?.nodes?.find((o) => o?.id === kanji2) as NodeObjectWithData;
  const on1 = k1?.data?.onyomi;
  const on2 = k2?.data?.onyomi;
  return on1?.filter((value) => on2?.includes(value)) ?? "";
};

const getLinkDirectionalArrowRelPos = ({ source, target }: LinkObject) => {
  if (
    typeof source === "object" &&
    typeof target === "object" &&
    source.x &&
    target.x &&
    source.y &&
    target.y &&
    source.z &&
    target.z
  ) {
    const linkLength = Math.hypot(
      target.x - source.x,
      target.y - source.y,
      target.z - source.z,
    );
    return (linkLength - 8) / linkLength;
  }

  return 0.8;
};

const getNodeLabel = (n: NodeObject, enableNodePreview: boolean) => {
  if (enableNodePreview) {
    return "";
  }

  const node = n as NodeObjectWithData;
  if (!node.data) {
    return "";
  }

  const kunyomi = node.data.kunyomi.join(", ");
  const meaning = node.data.meaning;
  if (!kunyomi && !meaning) {
    return "";
  }

  return `<div style="color: #ffffff; background: #000000a6; padding: 4px; border-radius: 4px;">
            <span>${escapeHtml(kunyomi)}</span>
            <br/>
            <span>${escapeHtml(meaning)}</span>
          </div>
         `;
};

const createNodeThreeObject = (node: NodeObject, selectedId: string) => {
  const ball = new THREE.Mesh(
    NODE_GEOMETRY,
    new THREE.MeshLambertMaterial({
      color: getNodeDefaultColor(String(node.id), selectedId),
      transparent: true,
      depthWrite: false,
      opacity: 0.8,
    }),
  );

  const sprite = new SpriteText(String(node.id));
  sprite.fontFace =
    "Iowan Old Style, Apple Garamond, Baskerville, Times New Roman, Droid Serif, Times, Source Serif Pro, serif";
  sprite.color = "#000";
  sprite.textHeight = 10;
  sprite.fontSize = 120;
  sprite.padding = 3;
  sprite.offsetY = KANJI_SPRITE_OFFSET_Y;

  const group = new THREE.Group();
  group.add(sprite);
  group.add(ball);
  return group;
};

const createLinkThreeObject = (
  link: LinkObject,
  data: GraphData | null,
  resolvedTheme: string | undefined,
) => {
  const source = typeof link.source === "object" ? link.source.id : link.source;
  const target = typeof link.target === "object" ? link.target.id : link.target;
  const linkText = getSharedOnyomi(data, String(source), String(target));

  if (!linkText || linkText.length === 0) {
    return null;
  }

  const sprite = new SpriteText(linkText.join(", "));
  sprite.color = resolvedTheme === "dark" ? "#ffffff" : "#000000";
  sprite.textHeight = 6;
  return sprite;
};

const updateLinkPosition = (
  sprite: THREE.Object3D | SpriteText | null | undefined,
  { start, end }: { start: { x: number; y: number; z: number }; end: { x: number; y: number; z: number } },
) => {
  const middlePos = {
    x: start.x + (end.x - start.x) / 2,
    y: start.y + (end.y - start.y) / 2,
    z: start.z + (end.z - start.z) / 2,
  };

  sprite?.position && Object.assign(sprite.position, middlePos);
  return null;
};

const Graph3D = ({
  kanjiInfo,
  graphData,
  showOutLinks,
  triggerFocus,
  bounds,
  autoRotate,
  showParticles,
  navigationTab,
  enableNodePreview = false,
  onPreviewNode,
  onClosePreview,
}: Props) => {
  const { resolvedTheme } = useTheme();
  const { push, prefetch } = useRouter();

  const fg3DRef: React.MutableRefObject<ForceGraphMethods | undefined> =
    React.useRef(undefined);

  const data = React.useMemo<GraphData | null>(
    () => (showOutLinks ? graphData?.withOutLinks : graphData?.noOutLinks) ?? null,
    [graphData?.noOutLinks, graphData?.withOutLinks, showOutLinks],
  );

  const buildNodeHref = React.useCallback(
    (id: string) =>
      buildKanjiHref(id, {
        tab: navigationTab ?? null,
      }),
    [navigationTab],
  );

  const resumeRotateTimeout = React.useRef<ReturnType<typeof setTimeout>>(
    undefined,
  );
  const hasGraph = Boolean(data);

  // Refs are detached before effect cleanup, so capture the instance up front.
  React.useEffect(() => {
    const fg = fg3DRef.current;
    if (!fg) return;
    return () => {
      clearTimeout(resumeRotateTimeout.current);
      fg.pauseAnimation();
      const renderer = fg.renderer();
      renderer.dispose();
      renderer.forceContextLoss();
    };
  }, [hasGraph]);

  const handleClick = (node: NodeObject) => {
    const nodeId = String(node?.id);

    if (enableNodePreview && onPreviewNode) {
      onPreviewNode({
        id: nodeId,
        data: (node as NodeObjectWithData).data ?? null,
      });
      return;
    }

    void push(buildNodeHref(nodeId));
  };

  React.useEffect(() => {
    const controls = fg3DRef?.current?.controls();
    if (controls) {
      //@ts-ignore
      controls.autoRotate = autoRotate;
    }
  }, [autoRotate, fg3DRef?.current]);

  // FOCUS  ON MAIN NODE AT START
  React.useEffect(() => {
    const focusMain = setTimeout(() => {
      if (kanjiInfo.id && data && data?.nodes?.length > 0) {
        const node = data?.nodes?.find((o) => o.id === kanjiInfo.id);
        const distance = 160;
        if (
          node &&
          node?.x &&
          node?.y &&
          node?.z &&
          fg3DRef &&
          fg3DRef?.current
        ) {
          const distRatio = 1 + distance / Math.hypot(node.x, node.y, node.z);
          fg3DRef.current.cameraPosition(
            {
              x: node.x * distRatio,
              y: node.y * distRatio,
              z: node.z * distRatio,
            }, // new position
            { x: node.x, y: node.y, z: node.z }, // lookAt ({ x, y, z })
            1000, // ms transition duration
          );
        }
      }
    }, 100);

    return () => {
      clearTimeout(focusMain);
    };
  }, [data, kanjiInfo.id, triggerFocus]);

  const handleHover = (node: NodeObject | null, prevNode: NodeObject | null) => {
    if (node) {
      void prefetch(buildNodeHref(String(node.id)));
    }

    // Pause autoRotate while hovering, resume shortly after leaving a node
    const controls = fg3DRef.current?.controls() as
      | { autoRotate: boolean }
      | undefined;
    if (autoRotate && controls) {
      clearTimeout(resumeRotateTimeout.current);
      if (node) {
        controls.autoRotate = false;
      } else {
        resumeRotateTimeout.current = setTimeout(() => {
          controls.autoRotate = true;
        }, 500);
      }
    }

    // Reset the previous node's color to its default
    if (prevNode) resetNodeColor(prevNode, kanjiInfo.id);

    // Apply hover effect to the currently hovered node
    if (node) highlightNode(node);
  };

  const linkColor = React.useCallback(
    () => (resolvedTheme === "dark" ? "#ffffff" : "#000000"),
    [resolvedTheme],
  );
  const nodeLabel = React.useCallback(
    (node: NodeObject) => getNodeLabel(node, enableNodePreview),
    [enableNodePreview],
  );
  const nodeThreeObject = React.useCallback(
    (node: NodeObject) => createNodeThreeObject(node, kanjiInfo.id),
    [kanjiInfo.id],
  );
  const linkThreeObject = React.useCallback(
    (link: LinkObject) => createLinkThreeObject(link, data, resolvedTheme),
    [data, resolvedTheme],
  );
  const handleBackgroundClick = React.useCallback(() => {
    if (enableNodePreview) {
      onClosePreview?.();
    }
  }, [enableNodePreview, onClosePreview]);

  if (!graphData || !kanjiInfo || !data) return <></>;

  return (
    <ForceGraph3D
      controlType={"orbit"}
      width={bounds.width}
      height={bounds.height}
      backgroundColor={"#00000000"}
      graphData={data}
      linkColor={linkColor}
      linkDirectionalArrowLength={5}
      linkDirectionalArrowRelPos={getLinkDirectionalArrowRelPos}
      linkDirectionalArrowResolution={8}
      linkDirectionalParticles={3}
      linkDirectionalParticleSpeed={0.004}
      linkDirectionalParticleWidth={showParticles ? 1 : 0.001}
      linkDirectionalParticleResolution={8}
      enableNavigationControls={true}
      showNavInfo={false}
      ref={fg3DRef}
      // warmupTicks={120}
      // cooldownTime={1500}
      onNodeClick={handleClick}
      onBackgroundClick={handleBackgroundClick}
      onNodeHover={handleHover}
      nodeLabel={nodeLabel}
      nodeThreeObject={nodeThreeObject}
      // ADD ONYOMI TO LINKS
      linkThreeObjectExtend={true}
      // @ts-ignore
      linkThreeObject={linkThreeObject}
      linkPositionUpdate={updateLinkPosition}
    />
  );
};

export default Graph3D;
