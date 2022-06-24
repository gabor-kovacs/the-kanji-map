import * as React from "react";

import {
  getAllKanji,
  getGraphData,
  getKanjiData,
  getKanjiDataLocal,
  getStrokeAnimation,
} from "../lib/lib";
import Head from "next/head";
import type { GetStaticPaths, GetStaticProps } from "next";

import Graphs from "../components/graphs";

import Search from "../components/search";
import DrawInput from "../components/drawInput";
import Header from "../components/header";
import styled from "@emotion/styled";
import Examples from "../components/examples";
import Radical from "../components/radical";
import Kanji from "../components/kanji";

type KanjiInfo = {
  id: string;
  kanjialiveData?: any;
  jishoData?: any;
};

interface Props {
  kanjiInfo: KanjiInfo;
  graphData: any;
  strokeAnimation: string;
}

const Page: React.FC<Props> = ({ kanjiInfo, graphData, strokeAnimation }) => {
  return (
    <>
      <Head>
        <title>{kanjiInfo.id}</title>
      </Head>
      <Header />
      <Main>
        <Top>
          <SearchWrapper>
            <Search />
            <DrawInput />
          </SearchWrapper>
          {/* <Test>
            <div dangerouslySetInnerHTML={{ __html: strokeAnimation }} />
          </Test> */}
          <Kanji
            {...{ kanjiInfo, graphData, strokeAnimation }}
            // kanjiInfo={kanjiInfo}
            // graphData={graphData}
            // strokeAnimation={strokeAnimation}
          />
          <Radical kanjiInfo={kanjiInfo} />
        </Top>
        <Bottom>
          <Examples kanjiInfo={kanjiInfo} />
          <Graphs {...{ kanjiInfo, graphData }} />
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
  getKanjiDataLocal(params?.id as string);

  // const kanjiInfoRaw = await getKanjiData(params?.id as string);
  const kanjiInfoRaw = await getKanjiDataLocal(params?.id as string);
  const graphDataRaw = await getGraphData(params?.id as string);
  const strokeAnimation = await getStrokeAnimation(params?.id as string);
  // workaround to avoid "cannot serialize undefined" error
  const kanjiInfo = JSON.parse(JSON.stringify(kanjiInfoRaw));
  const graphData = JSON.parse(JSON.stringify(graphDataRaw));

  return {
    props: {
      kanjiInfo,
      graphData,
      strokeAnimation,
    },
  };
};

// *  Styles

const Main = styled.main`
  width: 100%;
  height: calc(100% - 50px);
  display: grid;
  grid-template-columns: 1fr;
  grid-template-rows: 330px 1fr;
  /* grid-template-rows: 1fr 1fr; */
`;

const Top = styled.div`
  display: grid;
  grid-template-columns: 252px 1fr 1fr;
  overflow: hidden;
  border-top: 1px solid var(--color-lighter);
  border-bottom: 1px solid var(--color-lighter);
`;

const Bottom = styled.div`
  display: grid;
  grid-template-columns: 2fr 3fr;
  overflow: hidden;
`;

const SearchWrapper = styled.div`
  position: relative;
  padding: 16px;

  & > div:first-of-type {
    margin-bottom: 10px;
  }
`;
