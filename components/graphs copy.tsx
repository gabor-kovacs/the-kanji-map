import * as React from "react";
import Tabs from "@mui/material/Tabs";
import Tab from "@mui/material/Tab";
import Typography from "@mui/material/Typography";
import Box from "@mui/material/Box";

import Checkbox from "@mui/material/Checkbox";

import { ResizeObserver } from "@juggle/resize-observer";
import useMeasure from "react-use-measure";

// import Graph3DNoSSR from "../components/graph3DWrapper";
// import Graph2DNoSSR from "../components/graph2DWrapper";

import dynamic from "next/dynamic";
const Graph2DNoSSR = dynamic(() => import("./graph2D"), {
  ssr: false,
});
const Graph3DNoSSR = dynamic(() => import("./graph3D"), {
  ssr: false,
});

import styled from "@emotion/styled";
import { css } from "@emotion/react";

interface TabPanelProps {
  children?: React.ReactNode;
  index: number;
  value: number;
}

type KanjiInfo = {
  id: string;
  kanjialiveData?: any;
  jishoData?: any;
};

interface Props {
  kanjiInfo: KanjiInfo;
  graphData: any;
}

function TabPanel(props: TabPanelProps) {
  const { children, value, index, ...other } = props;

  return (
    <div
      style={{ display: value === index ? "block" : "none" }}
      // style={{ pointerEvents: "none" }}
      role="tabpanel"
      // hidden={value !== index}
      id={`tabpanel-${index}`}
      aria-labelledby={`tab-${index}`}
      {...other}
    >
      {children}
    </div>
  );
}

function a11yProps(index: number) {
  return {
    id: `tab-${index}`,
    "aria-controls": `tabpanel-${index}`,
  };
}

const Graphs: React.FC<Props> = ({ kanjiInfo, graphData }) => {
  const [ref, bounds] = useMeasure({ polyfill: ResizeObserver, debounce: 50 });

  const [tabValue, setTabValue] = React.useState(0);
  const [showOutLinks, setShowOutLinks] = React.useState<boolean>(true);

  const handleTabChange = (event: React.SyntheticEvent, newValue: number) => {
    setTabValue(newValue);
  };

  const handleShowOutlinks = (event: React.ChangeEvent<HTMLInputElement>) => {
    setShowOutLinks(event.target.checked);
  };

  React.useEffect(() => {
    console.log(bounds);
  }, [bounds]);

  return (
    <Wrapper ref={ref}>
      <Box sx={{ borderBottom: 1, borderColor: "divider" }}>
        <Tabs value={tabValue} onChange={handleTabChange} aria-label="tabs">
          <Tab label="3D" {...a11yProps(0)} />
          <Tab label="2D" {...a11yProps(1)} />
        </Tabs>
      </Box>
      <TabPanel value={tabValue} index={0}>
        <Graph3DNoSSR
          kanjiInfo={kanjiInfo}
          graphData={graphData}
          showOutLinks={showOutLinks}
          triggerFocus={tabValue}
          bounds={bounds}
        />
      </TabPanel>
      <TabPanel value={tabValue} index={1}>
        <Graph2DNoSSR
          kanjiInfo={kanjiInfo}
          graphData={graphData}
          showOutLinks={showOutLinks}
          triggerFocus={tabValue}
          bounds={bounds}
        />
      </TabPanel>
      <div
        css={css`
          position: absolute;
          bottom: 0;
          right: 0;
        `}
      >
        <Checkbox
          onChange={handleShowOutlinks}
          inputProps={{ "aria-label": "Show out links" }}
          defaultChecked
        />
      </div>
    </Wrapper>
  );
};

export default Graphs;

const Wrapper = styled.div`
  position: relative;
  width: 100%;
  height: 800px;
  & .MuiTab-textColorPrimary {
    color: var(--color-light);
  }

  & .Mui-selected {
    color: var(--color-primary);
  }
  & .MuiTabs-indicator {
    background-color: var(--color-primary);
  }
`;
