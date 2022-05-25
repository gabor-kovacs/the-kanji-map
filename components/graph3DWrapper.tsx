import dynamic from "next/dynamic";

const Graph3DNoSSR = dynamic(() => import("./graph3D"), {
  ssr: false,
});

export default Graph3DNoSSR;
