import dynamic from "next/dynamic";

const GraphNoSSR = dynamic(() => import("./graph"), {
  ssr: false,
});

export default Graph;
