import dynamic from "next/dynamic";

const FullGraphNoSSR = dynamic(() => import("./graphFull"), {
  ssr: false,
});

export default FullGraphNoSSR;
