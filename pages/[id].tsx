import * as React from "react";
import { useRouter } from "next/router";
import { NextSeo } from "next-seo";
import Graphs from "../components/graphs";
import Search from "../components/search";
import DrawInput from "../components/drawInput";
import Header from "../components/header";
import Examples from "../components/examples";
import Radical from "../components/radical";
import Kanji from "../components/kanji";
import TabPanel from "../components/tabpanel";
import useMediaQuery from "@mui/material/useMediaQuery";
import {
  getAllKanji,
  getGraphData,
  getKanjiDataLocal,
  getStrokeAnimation,
} from "../lib/lib";
import Tabs from "@mui/material/Tabs";
import Tab from "@mui/material/Tab";
import SearchIcon from "@mui/icons-material/Search";
import SwipeableViews from "react-swipeable-views";
import {
  Wrapper,
  Main,
  Top,
  Bottom,
  SearchWrapper,
  Controls,
} from "../styles/global";
import type { GetStaticPaths, GetStaticProps } from "next";

interface Props {
  kanjiInfo: KanjiInfo;
  graphData: BothGraphData;
  strokeAnimation: string;
}

const Page: React.FC<Props> = ({ kanjiInfo, graphData, strokeAnimation }) => {
  const mobile = useMediaQuery("(max-width: 767px)");
  const desktop = useMediaQuery("(min-width: 768px)");

  const [tabValue, setTabValue] = React.useState(0);

  const handleTabChange = (event: React.SyntheticEvent, newValue: number) => {
    setTabValue(newValue);
  };

  const handleChangeIndex = (index: number) => {
    setTabValue(index);
  };

  const router = useRouter();
  React.useEffect(() => {
    const resetTab = () => setTabValue(0);
    router.events.on("routeChangeComplete", resetTab);
    return () => {
      router.events.off("routeChangeComplete", resetTab);
    };
  }, [router.events]);

  return (
    <>
      <NextSeo
        title={`${kanjiInfo?.id} | The Kanji Map`}
        description={`Kanji information for ${kanjiInfo?.id}`}
      />
      <Wrapper>
        <Header />
        <Main>
          {desktop && (
            <>
              <Top>
                <SearchWrapper>
                  <Search />
                  <DrawInput />
                </SearchWrapper>
                <Kanji {...{ kanjiInfo, graphData, strokeAnimation }} />
                <Radical kanjiInfo={kanjiInfo} />
              </Top>
              <Bottom>
                <Examples kanjiInfo={kanjiInfo} />
                <Graphs {...{ kanjiInfo, graphData }} />
              </Bottom>
            </>
          )}
          {mobile && (
            <>
              <SwipeableViews
                axis={"x"}
                index={tabValue}
                onChangeIndex={handleChangeIndex}
              >
                <TabPanel value={tabValue} index={0}>
                  <Kanji {...{ kanjiInfo, graphData, strokeAnimation }} />
                </TabPanel>
                <TabPanel value={tabValue} index={1}>
                  <Examples kanjiInfo={kanjiInfo} />
                </TabPanel>
                <TabPanel value={tabValue} index={2}>
                  <Radical kanjiInfo={kanjiInfo} />
                </TabPanel>
                <TabPanel value={tabValue} index={3}>
                  <Graphs {...{ kanjiInfo, graphData }} />
                </TabPanel>
                <TabPanel value={tabValue} index={4}>
                  <SearchWrapper>
                    <Search />
                    <DrawInput />
                  </SearchWrapper>
                </TabPanel>
              </SwipeableViews>
            </>
          )}
        </Main>
        {mobile && (
          <Controls>
            <Tabs
              value={tabValue}
              onChange={handleTabChange}
              indicatorColor="secondary"
              textColor="inherit"
              variant="fullWidth"
              aria-label="full width tabs example"
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

export default Page;

// *  Next.js

export const getStaticPaths: GetStaticPaths = async () => {
  const paths = getAllKanji();

  return {
    paths,
    fallback: false,
  };
};

// export const getStaticProps: GetStaticProps = async ({ params }) => {
//   const kanjiInfoRaw = await getKanjiDataLocal(params?.id as string);
//   const graphDataRaw = await getGraphData(params?.id as string);
//   const strokeAnimation = await getStrokeAnimation(params?.id as string);
//   // workaround to avoid "cannot serialize undefined" error
//   const kanjiInfo = JSON.parse(JSON.stringify(kanjiInfoRaw));
//   const graphData = JSON.parse(JSON.stringify(graphDataRaw));

//   return {
//     props: {
//       kanjiInfo,
//       graphData,
//       strokeAnimation,
//     },
//   };
// };

export const getStaticProps: GetStaticProps = async ({ params }) => {
  try {
    if (!params?.id) {
      return { props: { error: "Missing or invalid Kanji ID" } };
    }
    const id = params.id as string;
    // const kanjiInfo = await getKanjiDataLocal(id);
    const kanjiInfo = null;
    const graphData = await getGraphData(id); // Assume this function is similarly safe
    const strokeAnimation = await getStrokeAnimation(id); // Ditto

    return {
      props: {
        kanjiInfo,
        graphData,
        strokeAnimation,
      },
    };
  } catch (error) {
    console.error("Error in getStaticProps:", error);
    return {
      props: {
        error: "An error occurred",
      },
    };
  }
};
