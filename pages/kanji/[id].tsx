import * as React from "react";
import Layout from "../../components/layout";
import { getAllKanji, getGraphData, getKanjiData } from "../../lib/lib";
import Head from "next/head";
import type { GetStaticPaths, GetStaticProps } from "next";

import ForceGraph3D, {
  ForceGraphMethods,
  GraphData,
  LinkObject,
  NodeObject,
} from "react-force-graph-3d";
import Graph from "../../components/graphWrapper";

interface Props {
  kanjiInfo: { id: string; kanjialiveData: any };
}

const Page: React.FC<Props> = ({ kanjiInfo }) => {
  const [graphData, setGraphData] = React.useState<GraphData>({
    nodes: [],
    links: [],
  });

  React.useEffect(() => {
    console.log(kanjiInfo?.kanjialiveData);

    const { graphDataNoOutLinks, graphDataWithOutLinks } = getGraphData(
      kanjiInfo.id
    );
    setGraphData(graphDataWithOutLinks as unknown as GraphData); // TODO
    console.log(graphDataNoOutLinks);
    console.log(graphDataWithOutLinks);
  }, []);

  return (
    <Layout>
      <Head>
        <title>{kanjiInfo.id}</title>
      </Head>
      <Graph data={graphData} id={kanjiInfo.id} />

      <h1>{kanjiInfo.id}</h1>
      <p>{`${process.env.KANJIALIVE_API_KEY}`}</p>
      <p>{`${kanjiInfo.kanjialiveData}`}</p>

      {kanjiInfo?.kanjialiveData?.radical?.name?.hiragana && (
        <p>
          Reading:{" "}
          <strong>{kanjiInfo?.kanjialiveData?.radical?.name?.hiragana}</strong>
        </p>
      )}
      {kanjiInfo?.kanjialiveData?.radical?.strokes && (
        <p>
          Strokes:{" "}
          <strong>{kanjiInfo?.kanjialiveData?.radical?.strokes}</strong>
        </p>
      )}
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
