"us client";
import * as React from "react";
import { useRouter } from "next/navigation";
import * as THREE from "three";
import type { ForceGraphMethods, GraphData } from "react-force-graph-3d";
import ForceGraph3D, { LinkObject, NodeObject } from "react-force-graph-3d";
import SpriteText from "three-spritetext";
import kanjilist from "@/../data/kanjilist.json";
import type { RectReadOnly } from "react-use-measure";
import { useTheme } from "next-themes";

type NodeObjectWithData = NodeObject & { data: KanjiInfo };

interface Props {
  kanjiInfo: KanjiInfo;
  graphData: BothGraphData | null;
  showOutLinks: boolean;
  triggerFocus: number;
  bounds: RectReadOnly;
  autoRotate: boolean;
}

const Graph3D = ({
  kanjiInfo,
  graphData,
  showOutLinks,
  triggerFocus,
  bounds,
  autoRotate,
}: Props) => {
  const joyoList = kanjilist.filter((el) => el.g === 1).map((el) => el.k);
  const jinmeiyoList = kanjilist.filter((el) => el.g === 2).map((el) => el.k);

  const { resolvedTheme } = useTheme();

  const fg3DRef: React.MutableRefObject<ForceGraphMethods | undefined> =
    React.useRef();

  const router = useRouter();

  const [data, setData] = React.useState<GraphData>({
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

  const handleClick = (node: NodeObject) => {
    void router.push(`/${node.id}`);
  };

  // prefetch routes for nodes visible in the graph
  React.useEffect(() => {
    data?.nodes?.forEach((node) => {
      void router.prefetch(`/${node.id}`);
    });
  }, [data, router]);

  React.useEffect(() => {
    const controls = fg3DRef?.current?.controls();
    // @ts-ignore
    if (controls) controls.autoRotate = autoRotate;
  }, [autoRotate]);

  // FOCUS  ON MAIN NODE AT START
  React.useEffect(() => {
    const focusMain = setTimeout(() => {
      if (kanjiInfo.id && data?.nodes?.length > 0) {
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
            1000 // ms transition duration
          );
        }
      }
    }, 100);

    return () => {
      clearTimeout(focusMain);
    };
  }, [data, kanjiInfo.id, triggerFocus]);

  const debounce = (func: (...args: any[]) => void, wait: number) => {
    let timeout: NodeJS.Timeout;
    return (...args: any[]) => {
      clearTimeout(timeout);
      timeout = setTimeout(() => func(...args), wait);
    };
  };
  const handleAutoRotate = debounce((node) => {
    if (autoRotate && fg3DRef?.current) {
      // @ts-ignore
      !node && (fg3DRef.current.controls().autoRotate = true);
    }
  }, 500);

  const handleHover = (node: any, prevNode: any) => {
    if (autoRotate && fg3DRef?.current) {
      // @ts-ignore
      node && (fg3DRef.current.controls().autoRotate = false);
      handleAutoRotate(node);
    }
    // TODO: make this more compact
    // RESTORE COLOR OF PREVIOUS HOVERED NODE
    if (prevNode?.id) {
      if (prevNode?.__threeObj?.children[1]?.material?.color?.b) {
        prevNode.__threeObj.children[1].material.color.b = 1;
      }
      if (prevNode?.__threeObj?.children[1]?.material?.color?.g) {
        prevNode.__threeObj.children[1].material.color.g = 1;
      }
      if (prevNode?.__threeObj?.children[1]?.material?.color?.r) {
        prevNode.__threeObj.children[1].material.color.r = 1;
      }
    }
    if (prevNode?.id && joyoList?.includes(prevNode.id)) {
      if (prevNode?.__threeObj?.children[1]?.material?.color?.b) {
        prevNode.__threeObj.children[1].material.color.b = 0.8862745098;
      }
      if (prevNode?.__threeObj?.children[1]?.material?.color?.g) {
        prevNode.__threeObj.children[1].material.color.g = 0.76078431372;
      }
      if (prevNode?.__threeObj?.children[1]?.material?.color?.r) {
        prevNode.__threeObj.children[1].material.color.r = 0.50196078431;
      }
    }

    if (prevNode?.id && jinmeiyoList?.includes(prevNode.id)) {
      if (prevNode?.__threeObj?.children[1]?.material?.color?.b) {
        prevNode.__threeObj.children[1].material.color.b = 0.96078431372;
      }
      if (prevNode?.__threeObj?.children[1]?.material?.color?.g) {
        prevNode.__threeObj.children[1].material.color.g = 0.92156862745;
      }
      if (prevNode?.__threeObj?.children[1]?.material?.color?.r) {
        prevNode.__threeObj.children[1].material.color.r = 0.83529411764;
      }
    }

    if (prevNode?.id && kanjiInfo.id && prevNode.id === kanjiInfo.id) {
      if (prevNode?.__threeObj?.children[1]?.material?.color?.b) {
        prevNode.__threeObj.children[1].material.color.b = 0.8117647058823529;
      }
      if (prevNode?.__threeObj?.children[1]?.material?.color?.g) {
        prevNode.__threeObj.children[1].material.color.g = 0.6;
      }
      if (prevNode?.__threeObj?.children[1]?.material?.color?.r) {
        prevNode.__threeObj.children[1].material.color.r = 0.16862745098039217;
      }
    }
    // CHANGE COLOR OF HOVERED NODE
    if (node?.__threeObj?.children[1]?.material?.color?.b) {
      node.__threeObj.children[1].material.color.b = 0.8117647058823529;
    }
    if (node?.__threeObj?.children[1]?.material?.color?.g) {
      node.__threeObj.children[1].material.color.g = 0.6;
    }
    if (node?.__threeObj?.children[1]?.material?.color?.r) {
      node.__threeObj.children[1].material.color.r = 0.16862745098039217;
    }
  };

  // find same onyomi
  const sameOn = (kanji1: string, kanji2: string) => {
    const k1 = data?.nodes?.find((o) => o.id === kanji1) as NodeObjectWithData;
    const k2 = data?.nodes?.find((o) => o.id === kanji2) as NodeObjectWithData;
    const on1: string[] = k1?.data?.jishoData?.onyomi;
    const on2: string[] = k2?.data?.jishoData?.onyomi;
    return on1?.filter((value) => on2?.includes(value)) ?? "";
  };

  return (
    <ForceGraph3D
      controlType={"orbit"}
      width={bounds.width}
      height={bounds.height}
      backgroundColor={"#00000000"}
      graphData={data}
      linkColor={() => {
        return resolvedTheme === "dark" ? "#ffffff" : "#000000";
      }}
      linkDirectionalArrowLength={5}
      linkDirectionalArrowRelPos={({ source, target }) => {
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
            target.z - source.z
          );
          return (linkLength - 8) / linkLength;
        } else {
          return 0.8;
        }
      }}
      linkDirectionalArrowResolution={8}
      linkDirectionalParticles={3}
      linkDirectionalParticleSpeed={0.004}
      linkDirectionalParticleWidth={1}
      linkDirectionalParticleResolution={8}
      enableNavigationControls={true}
      showNavInfo={false}
      ref={fg3DRef}
      warmupTicks={60}
      onNodeClick={handleClick}
      onNodeHover={handleHover}
      nodeLabel={(n) => {
        const node = n as NodeObjectWithData;
        return `<div style="color: #ffffff; background: #000000a6; padding: 4px; border-radius: 4px;">
                  <span>${node.data.jishoData?.kunyomi}</span>
                  <br/>
                  <span>${node.data.jishoData?.meaning}</span>
                </div>
               `;
      }}
      nodeThreeObject={(node: NodeObject) => {
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

        const ball = new THREE.Mesh(
          new THREE.SphereGeometry(8, 32, 32),
          new THREE.MeshLambertMaterial({
            color: color,
            transparent: true,
            depthWrite: false,
            opacity: 0.8,
          })
        );

        // If it's a single character
        const sprite = new SpriteText(String(node.id));
        sprite.fontFace =
          "Iowan Old Style, Apple Garamond, Baskerville, Times New Roman, Droid Serif, Times, Source Serif Pro, serif";
        // sprite.fontFace =
        //   "Iowan Old Style" ||
        //   "Apple Garamond" ||
        //   "Baskerville" ||
        //   "Times New Roman" ||
        //   "Droid Serif" ||
        //   "Times" ||
        //   "Source Serif Pro" ||
        //   "serif";
        sprite.color = "#000";
        sprite.textHeight = 10;
        sprite.fontSize = 120;
        sprite.padding = 3;

        const group = new THREE.Group();
        group.add(sprite);
        group.add(ball);
        return group;
      }}
      // ADD ONYOMI TO LINKS
      linkThreeObjectExtend={true}
      // @ts-ignore
      linkThreeObject={(link: LinkObject) => {
        const source =
          typeof link.source === "object" ? link.source.id : link.source;
        const target =
          typeof link.target === "object" ? link.target.id : link.target;

        const linkText = sameOn(String(source), String(target));

        let sprite: SpriteText;
        if (linkText && linkText.length > 0) {
          sprite = new SpriteText(linkText.join(", "));
        } else {
          return null;
        }
        sprite.color = resolvedTheme === "dark" ? "#ffffff" : "#000000";
        sprite.textHeight = 6;
        return sprite;
      }}
      linkPositionUpdate={(sprite, { start, end }) => {
        const middlePos: { x: number; y: number; z: number } = {
          x: start.x + (end.x - start.x) / 2,
          y: start.y + (end.y - start.y) / 2,
          z: start.z + (end.z - start.z) / 2,
        };
        // if there is a same onyomi link
        sprite?.position && Object.assign(sprite.position, middlePos);
        return null;
      }}
    />
  );
};

export default Graph3D;
