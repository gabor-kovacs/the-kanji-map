import * as React from "react";
import Layout from "../../components/layout";
import { getAllKanji, getKanjiData } from "../../lib/kanji";
import Head from "next/head";
import type { GetStaticPaths, GetStaticProps } from "next";

interface Props {
  kanjiInfo: { id: string; kanjialiveData: any };
}

const Page: React.FC<Props> = ({ kanjiInfo }) => {
  React.useEffect(() => {
    console.log(kanjiInfo?.kanjialiveData);
  }, []);

  return (
    <Layout>
      <Head>
        <title>{kanjiInfo.id}</title>
      </Head>
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
