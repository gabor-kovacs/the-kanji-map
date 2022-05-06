import * as React from "react";
import Layout from "../../components/layout";
import { getAllKanji, getGraphData, getKanjiData } from "../../lib/lib";
import Head from "next/head";
import type { GetStaticPaths, GetStaticProps } from "next";

import type { ForceGraphMethods, GraphData } from "react-force-graph-3d";

import GraphNoSSR from "../../components/graphWrapper";

type KanjiInfo = {
  id: string;
  kanjialiveData?: any;
  jishoData?: any;
};

interface Props {
  kanjiInfo: KanjiInfo;
}

const Page: React.FC<Props> = ({ kanjiInfo }) => {
  return (
    <Layout>
      <Head>
        <title>{kanjiInfo.id}</title>
      </Head>
      <GraphNoSSR kanjiInfo={kanjiInfo} />
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
  return {
    props: {
      kanjiInfo,
    },
  };
};
