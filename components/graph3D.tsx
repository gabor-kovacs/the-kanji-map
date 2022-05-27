import React, { useState, useEffect, useRef, SetStateAction } from "react";

import ForceGraph3D, { LinkObject, NodeObject } from "react-force-graph-3d";
import type { ForceGraphMethods, GraphData } from "react-force-graph-3d";
import { getGraphData } from "../lib/lib";

import { joyoList } from "../data/joyo";
import { jinmeiyoList } from "../data/jinmeiyo";

import * as THREE from "three";

import SpriteText from "three-spritetext";
import { useRouter } from "next/router";
import { useTheme } from "next-themes";

type KanjiInfo = {
  id: string;
  kanjialiveData?: any;
  jishoData?: any;
};

interface Props {
  kanjiInfo: KanjiInfo;
  graphData: any;
}

const Graph3D: React.FC<Props> = ({ kanjiInfo, graphData }) => {
  const { theme } = useTheme();

  const fgRef: React.MutableRefObject<ForceGraphMethods | undefined> = useRef();

  const router = useRouter();

  const [data, setData] = useState<GraphData>({
    nodes: [],
    links: [],
  });

  useEffect(() => {
    setData(graphData.withOutLinks as unknown as GraphData);
    console.dir(graphData.withOutLinks, { depth: null });
  }, []);

  const handleClick = (node: NodeObject) => {
    router.push(`/kanji/${node.id}`);
  };

  // prefetch routes for nodes visible in the graph
  useEffect(() => {
    data?.nodes?.forEach((node) => {
      router.prefetch(`/kanji/${node.id}`);
    });
  }, [data, router]);

  // FOCUS  ON MAIN NODE AT START
  // TODO
  useEffect(() => {
    const focusMain = setTimeout(() => {
      if (kanjiInfo.id && data?.nodes?.length > 0) {
        const node = data?.nodes?.find((o) => o.id === kanjiInfo.id);
        const distance = 160;
        if (node && node?.x && node?.y && node?.z && fgRef && fgRef?.current) {
          const distRatio = 1 + distance / Math.hypot(node.x, node.y, node.z);
          fgRef.current.cameraPosition(
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
    return () => clearTimeout(focusMain);
  }, [data, kanjiInfo.id]);

  const handleHover = (node: any, prevNode: any) => {
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
    const on1 = data?.nodes?.find((o) => o.id === kanji1)?.data?.jishoData
      ?.onyomi;
    const on2 = data?.nodes?.find((o) => o.id === kanji2)?.data?.jishoData
      ?.onyomi;

    console.log(on1);
    console.log(on2);

    const onyomiOverlap = on1?.filter((value) => on2?.includes(value));

    return onyomiOverlap;

    // return data[kanji1]?.jishoData?.onyomi?.filter((value) =>
    //   data[kanji2]?.jishoData?.onyomi?.includes(value)
    // );
  };

  useEffect(() => {
    sameOn("語", "吾");
    console.log(sameOn("語", "吾"));
  }, [data]);

  return (
    <ForceGraph3D
      width={500}
      height={500}
      // using css variables here can cause unexpected behavior
      backgroundColor={theme === "dark" ? "#1f1f1f" : "#ffffff"}
      graphData={data}
      // ARROWS
      linkColor={() => {
        return theme === "dark" ? "#ffffff" : "#000000";
      }}
      linkDirectionalArrowLength={5}
      // linkDirectionalArrowRelPos={0.9}
      linkDirectionalArrowResolution={32}
      //PARTICLES
      linkDirectionalParticles={3}
      linkDirectionalParticleSpeed={0.008}
      linkDirectionalParticleWidth={1}
      linkDirectionalParticleResolution={8}
      enableNodeDrag={false}
      enableNavigationControls={true}
      showNavInfo={false}
      ref={fgRef}
      warmupTicks={10}
      onNodeClick={handleClick}
      onNodeHover={handleHover}
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
          "Iowan Old Style" ||
          "Apple Garamond" ||
          "Baskerville" ||
          "Times New Roman" ||
          "Droid Serif" ||
          "Times" ||
          "Source Serif Pro" ||
          "serif";
        sprite.color = "#000";
        sprite.textHeight = 10;
        sprite.fontSize = 180;
        sprite.padding = 3;

        const group = new THREE.Group();
        group.add(sprite);
        group.add(ball);
        return group;
      }}
      // ADD ONYOMI TO LINKS
      // linkThreeObjectExtend={true}
      // linkThreeObject={(link: LinkObject) => {
      //   const linkText = sameOn(String(link.source), String(link.target));
      //   let sprite: SpriteText;
      //   if (linkText && linkText.length > 0) {
      //     sprite = new SpriteText(linkText.join(", "));
      //   } else {
      //     return null;
      //   }
      //   sprite.color = "#000";
      //   sprite.textHeight = 5;
      //   return sprite;
      // }}
      // linkPositionUpdate={(sprite, { start, end }) => {
      //   const middlePos: { x: number; y: number; z: number } = {
      //     x: start.x + (end.x - start.x) / 2,
      //     y: start.y + (end.y - start.y) / 2,
      //     z: start.z + (end.z - start.z) / 2,
      //   };
      //   // if there is a same onyomi link
      //   sprite?.position && Object.assign(sprite.position, middlePos);
      //   return null;
      // }}
    />
  );
};

export default Graph3D;
