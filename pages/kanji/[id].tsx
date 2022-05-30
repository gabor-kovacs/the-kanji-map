import * as React from "react";
import Layout from "../../components/layout";
import { getAllKanji, getGraphData, getKanjiData } from "../../lib/lib";
import Head from "next/head";
import type { GetStaticPaths, GetStaticProps } from "next";

import type { ForceGraphMethods, GraphData } from "react-force-graph-3d";

import Graph3DNoSSR from "../../components/graph3DWrapper";
import Graph2DNoSSR from "../../components/graph2DWrapper";

type KanjiInfo = {
  id: string;
  kanjialiveData?: any;
  jishoData?: any;
};

interface Props {
  kanjiInfo: KanjiInfo;
  graphData: any;
}

const Page: React.FC<Props> = ({ kanjiInfo, graphData }) => {
  return (
    <Layout>
      <Head>
        <title>{kanjiInfo.id}</title>
      </Head>
      <Graph2DNoSSR kanjiInfo={kanjiInfo} graphData={graphData} />
      <Graph3DNoSSR kanjiInfo={kanjiInfo} graphData={graphData} />
    </Layout>
  );
};

export default Page;

export const getStaticPaths: GetStaticPaths = async () => {
  const paths = getAllKanji();
  return {
    paths,
    fallback: false,
  };
};

// const removeUndefinedForNextJsSerializing = <T,>(props: T): T =>
//   Object.fromEntries(
//     Object.entries(props).filter(([, value]) => value !== undefined)
//   ) as T;

// const removeUndefinedForNextJsSerializing = (obj: obj) => {
//   if (!obj) return;
//   Object?.keys(obj)?.forEach(function (key) {
//     // Get this value and its type
//     var value = obj[key];
//     var type = typeof value;
//     if (type === "object") {
//       // Recurse...
//       removeUndefinedForNextJsSerializing(value);
//       // ...and remove if now "empty" (NOTE: insert your definition of "empty" here)
//       if (!Object?.keys(value)?.length) {
//         delete obj[key];
//       }
//     } else if (type === "undefined") {
//       // Undefined, remove it
//       delete obj[key];
//     }
//   });
// };

export const getStaticProps: GetStaticProps = async ({ params }) => {
  const kanjiInfoRaw = await getKanjiData(params?.id as string);
  const graphDataRaw = await getGraphData(params?.id as string);
  // workaround to avoid "cannot serialize undefined" error
  const kanjiInfo = JSON.parse(JSON.stringify(kanjiInfoRaw));
  const graphData = JSON.parse(JSON.stringify(graphDataRaw));

  return {
    props: {
      kanjiInfo,
      graphData,
    },
  };
};
