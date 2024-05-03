import * as React from "react";
import { NextSeo } from "next-seo";
import Graphs from "../components/graphs";
import Search from "../components/search";
import DrawInput from "../components/drawInput";
import Header from "../components/header";
import Examples from "../components/examples";
import Radical from "../components/radical";
import Kanji from "../components/kanji";
import useMediaQuery from "@mui/material/useMediaQuery";
import Tabs from "@mui/material/Tabs";
import Tab from "@mui/material/Tab";
import TabPanel from "../components/tabpanel";
import SearchIcon from "@mui/icons-material/Search";
import {
  Wrapper,
  Main,
  Top,
  Bottom,
  SearchWrapper,
  Controls,
} from "../styles/global";
import { Alert } from "../components/alert";

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
      <Wrapper>
        <Alert />
        <Header />
        <Main>
          {desktop && (
            <>
              <Top>
                <SearchWrapper>
                  <Search />
                  <DrawInput />
                </SearchWrapper>
                <Kanji
                  kanjiInfo={null}
                  graphData={null}
                  strokeAnimation={null}
                />
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
            </>
          )}
        </Main>
        {mobile && (
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
        )}
      </Wrapper>
    </>
  );
};

export default Home;
