import * as React from "react";

import { NextSeo } from "next-seo";

import Graphs from "../components/graphs";
import Search from "../components/search";
import DrawInput from "../components/drawInput";
import Header from "../components/header";
import styled from "@emotion/styled";
import Examples from "../components/examples";
import Radical from "../components/radical";
import Kanji from "../components/kanji";
import useMediaQuery from "@mui/material/useMediaQuery";
import Tabs from "@mui/material/Tabs";
import Tab from "@mui/material/Tab";
import SearchIcon from "@mui/icons-material/Search";
const Home: React.FC = () => {
  const mobile = useMediaQuery("(max-width: 767px)");
  const desktop = useMediaQuery("(min-width: 768px)");

  // fix to search when no kanji is selected
  const tabValue = 4;

  return (
    <>
      <NextSeo
        title="Home | The Kanji Map"
        description="The Kanji Map is a Japanese language learning tool that shows kanji information and decomposition in graph form."
      />
      <Header />
      <Main>
        {desktop && (
          <>
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
          </>
        )}
        {mobile && (
          <>
            <div>
              <TabPanel value={tabValue} index={0}>
                <div />
              </TabPanel>
              <TabPanel value={tabValue} index={1}>
                <div />
              </TabPanel>
              <TabPanel value={tabValue} index={2}>
                <div />
              </TabPanel>
              <TabPanel value={tabValue} index={3}>
                <div />
              </TabPanel>
              <TabPanel value={tabValue} index={4}>
                <SearchWrapper>
                  <Search />
                  <DrawInput />
                </SearchWrapper>
              </TabPanel>
            </div>
            <Controls>
              <Tabs
                value={tabValue}
                textColor="inherit"
                variant="fullWidth"
                aria-label="Tabs"
              >
                <Tab label="漢字" />
                <Tab label="例" />
                <Tab label="部首" />
                <Tab label="図" />
                <Tab icon={<SearchIcon />} />
              </Tabs>
            </Controls>
            <Graphs kanjiInfo={null} graphData={null} />
          </>
        )}
      </Main>
    </>
  );
};

export default Home;

interface TabPanelProps {
  children?: React.ReactNode;
  dir?: string;
  index: number;
  value: number;
}

const TabPanel = (props: TabPanelProps) => {
  const { children, value, index, ...other } = props;
  return (
    <TabPanelWrapper
      role="tabpanel"
      hidden={value !== index}
      id={`full-width-tabpanel-${index}`}
      aria-labelledby={`full-width-tab-${index}`}
      {...other}
    >
      {value === index && <>{children}</>}
    </TabPanelWrapper>
  );
};
const TabPanelWrapper = styled.div`
  position: relative;
  width: 100%;
  height: 100%;
  overflow: hidden;
`;

// *  Styles

const Main = styled.main`
  width: 100%;
  height: calc(100% - 50px);
  display: grid;
  grid-template-columns: 1fr;
  grid-template-rows: 330px 1fr;

  @media (max-width: 767px) {
    grid-template-rows: 1fr 50px;
    & > div {
      height: 100%;
      & > div {
        height: 100%;
      }
    }
  }
`;

const Top = styled.div`
  display: grid;
  grid-template-columns: 252px 1fr 1fr;
  overflow: hidden;
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

// * Mobile
const Controls = styled.div`
  border-top: 1px solid var(--color-lighter);
  border-bottom: 1px solid var(--color-lighter);
  height: 50px;

  & .Mui-selected {
    color: var(--color-primary) !important;
  }
  & .MuiTabs-indicator {
    background-color: var(--color-primary) !important;
  }

  button {
    min-width: 80px;
  }
`;
