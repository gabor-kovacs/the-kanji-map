import * as React from "react";
import Tabs from "@mui/material/Tabs";
import Tab from "@mui/material/Tab";
import Typography from "@mui/material/Typography";
import Box from "@mui/material/Box";

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

const GraphTabs: React.FC<Props> = ({ kanjiInfo, graphData }) => {
  const [value, setValue] = React.useState(0);

  const handleChange = (event: React.SyntheticEvent, newValue: number) => {
    setValue(newValue);
  };

  return (
    <Wrapper>
      <Box sx={{ borderBottom: 1, borderColor: "divider" }}>
        <Tabs value={value} onChange={handleChange} aria-label="tabs">
          <Tab label="3D" {...a11yProps(0)} />
          <Tab label="2D" {...a11yProps(1)} />
        </Tabs>
      </Box>
      <TabPanel value={value} index={0}>
        <Graph3DNoSSR
          kanjiInfo={kanjiInfo}
          graphData={graphData}
          triggerFocus={value}
        />
      </TabPanel>
      <TabPanel value={value} index={1}>
        <Graph2DNoSSR
          kanjiInfo={kanjiInfo}
          graphData={graphData}
          triggerFocus={value}
        />
      </TabPanel>
    </Wrapper>
  );
};

export default GraphTabs;

const Wrapper = styled.div`
  width: 100%;
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
