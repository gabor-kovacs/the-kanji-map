import React, { useState, useEffect, useRef, SetStateAction } from "react";

import dynamic from "next/dynamic";
const NoSSRForceGraph = dynamic(() => import("../lib/noSSRforceGraph"), {
  ssr: false,
});

import type { ForceGraphMethods, GraphData } from "react-force-graph-3d";

interface Props {
  data: GraphData;
  id: string;
}

const Graph: React.FC<Props> = ({ data, id }) => {
  const fgRef: React.MutableRefObject<ForceGraphMethods | undefined> = useRef();

  useEffect(() => {
    console.log(fgRef?.current);

    // fgRef?.current?.zoomToFit();
  }, [data, fgRef]);

  return (
    <NoSSRForceGraph
      width={500}
      height={500}
      backgroundColor={"#6af5e9"}
      graphData={data}
      ref={fgRef}
    />
  );
};

export default Graph;
