import React, { useState, useEffect, useRef, SetStateAction } from "react";

import ForceGraph3D, {
  ForceGraphMethods,
  GraphData,
  LinkObject,
  NodeObject,
} from "react-force-graph-3d";

import { joyoList } from "../data/joyo";
import { jinmeiyoList } from "../data/jinmeiyo";

import * as THREE from "three";

import SpriteText from "three-spritetext";

import styled from "@emotion/styled";

interface Props {
  data: GraphData;
  id: string;
}

export const Graph: React.FC<Props> = ({ data, id }) => {
  const fgRef: React.MutableRefObject<ForceGraphMethods | undefined> = useRef();

  // find same onyomi
  // const sameOn = (kanji1: string, kanji2: string) => {
  //   return data[kanji1]?.jishoData?.onyomi?.filter((value) =>
  //     data[kanji2]?.jishoData?.onyomi?.includes(value)
  //   );
  // };

  useEffect(() => {
    console.log(data);
  }, []);

  // FOCUS  ON MAIN NODE AT START
  useEffect(() => {
    const focusMain = setTimeout(() => {
      if (id && data?.nodes?.length > 0) {
        console.log("fgRef.current");
        console.log(fgRef.current);

        data.nodes.forEach((node) => {
          console.log(node.id);
        });

        fgRef.current?.zoomToFit();

        const node = data?.nodes?.find((o) => o.id === id);
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
    }, 1);
    return () => clearTimeout(focusMain);
  }, [data, id]);

  // const handleClick = (node: NodeObject) => {
  //   if (node?.id && typeof node.id === "string" && node.id.length <= 4) {
  //     const kanji = node.id;
  //     chise && kanji && chise[kanji] && setCurrent(chise[kanji]);
  //   }
  // };

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

    if (prevNode?.id && id && prevNode.id === id) {
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

  function genRandomTree(N = 300, reverse = false) {
    return {
      nodes: [...Array(N).keys()].map((i) => ({ id: i })),
      links: [...Array(N).keys()]
        .filter((id) => id)
        .map((id) => ({
          [reverse ? "target" : "source"]: id,
          [reverse ? "source" : "target"]: Math.round(Math.random() * (id - 1)),
        })),
    };
  }

  return (
    <>
      <ForceGraph3D
        width={500}
        height={500}
        backgroundColor={"#ffc"}
        graphData={genRandomTree()}
        // nodeLabel="id"
        // nodeColor="#ff0000"
        // ref={fgRef}
      />
      {/* <ForceGraph3D graphData={genRandomTree()} /> */}
    </>
  );
};

export default Graph;

// * STYLES **************************************************************************************************

const GraphWrapper = styled.div`
  grid-area: graphArea;
  width: calc(100% + 32px);
  height: 100%;
  background: #fff;
  padding: 0;
  margin-left: -16px;
  margin-right: -16px;
  box-shadow: 0 10px 20px rgba(0, 0, 0, 0.19), 0 6px 6px rgba(0, 0, 0, 0.23);
  overflow: hidden;

  border-top-left-radius: 40px;
  border-top-right-radius: 40px;

  //fix for ios top border radius
  backface-visibility: hidden;
  transform: translate3d(0, 0, 0);

  @media (min-width: 1000px) {
    width: 100%;
    margin: 0;
    border-radius: 20px;
  }
`;
