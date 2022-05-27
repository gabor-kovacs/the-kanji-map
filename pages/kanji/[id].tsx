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

export const getStaticProps: GetStaticProps = async ({ params }) => {
  const kanjiInfo = await getKanjiData(params?.id as string);
  const graphDataRaw = await getGraphData(params?.id as string);
  // workaround to avoid "cannot serialize undefined" error
  const graphData = JSON.parse(JSON.stringify(graphDataRaw));
  // console.dir(graphData, { depth: null });

  return {
    props: {
      kanjiInfo,
      graphData,
    },
  };
};
