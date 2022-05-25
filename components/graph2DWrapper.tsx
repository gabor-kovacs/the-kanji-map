import dynamic from "next/dynamic";

const Graph2DNoSSR = dynamic(() => import("./graph2D"), {
  ssr: false,
});

export default Graph2DNoSSR;
