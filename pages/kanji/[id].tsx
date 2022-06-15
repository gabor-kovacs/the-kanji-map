import * as React from "react";
import Layout from "../../components/layout";
import { getAllKanji, getGraphData, getKanjiData } from "../../lib/lib";
import Head from "next/head";
import type { GetStaticPaths, GetStaticProps } from "next";

import Graphs from "../../components/graphs";

import Search from "../../components/search";
import DrawInput from "../../components/drawInput";
import Header from "../../components/header";
import styled from "@emotion/styled";

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
    <>
      <Head>
        <title>{kanjiInfo.id}</title>
      </Head>
      <Header />
      <Main>
        <Top>
          <div>
            <Search />
            <DrawInput />
          </div>
          <div style={{ background: "pink" }}>kanji</div>
          <div style={{ background: "yellow" }}>radical</div>
        </Top>
        <Bottom>
          <div style={{ background: "yellow" }}>examples</div>
          <Graphs kanjiInfo={kanjiInfo} graphData={graphData} />
        </Bottom>
      </Main>
    </>
  );
};

export default Page;

// *  Next.js

export const getStaticPaths: GetStaticPaths = async () => {
  const paths = getAllKanji();
  return {
    paths,
    fallback: false,
  };
};

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

// *  Styles

const Main = styled.main`
  width: 100%;
  height: calc(100% - 50px);
  display: grid;
  grid-template-columns: 1fr;
  grid-template-rows: 1fr 1fr;
`;

const Top = styled.div`
  display: grid;
  grid-template-columns: 200px 1fr 1fr;
`;

const Bottom = styled.div`
  display: grid;
  grid-template-columns: 2fr 3fr;
`;
