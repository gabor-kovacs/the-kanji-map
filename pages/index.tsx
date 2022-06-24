import * as React from "react";

import Head from "next/head";

import Graphs from "../components/graphs";
import Search from "../components/search";
import DrawInput from "../components/drawInput";
import Header from "../components/header";
import styled from "@emotion/styled";
import Examples from "../components/examples";
import Radical from "../components/radical";
import Kanji from "../components/kanji";

const Home: React.FC = () => {
  return (
    <>
      <Head>
        <title>Home</title>
      </Head>
      <Header />
      <Main>
        <Top>
          <SearchWrapper>
            <Search />
            <DrawInput />
          </SearchWrapper>
          <Kanji kanjiInfo={null} graphData={null} strokeAnimation={null} />
          <Radical kanjiInfo={null} />
        </Top>
        <Bottom>
          <Examples kanjiInfo={null} />
          <Graphs kanjiInfo={null} graphData={null} />
        </Bottom>
      </Main>
    </>
  );
};

export default Home;

// *  Styles

const Main = styled.main`
  width: 100%;
  height: calc(100% - 50px);
  display: grid;
  grid-template-columns: 1fr;
  grid-template-rows: 330px 1fr;
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
