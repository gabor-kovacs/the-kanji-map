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

type KanjiInfo = {
  id: string;
  kanjialiveData?: any;
  jishoData?: any;
};

interface Props {
  kanjiInfo: KanjiInfo;
  graphData: any;
}

const Graphs: React.FC<Props> = ({ kanjiInfo, graphData }) => {
  const [measureRef, bounds] = useMeasure({
    polyfill: ResizeObserver,
    debounce: 50,
  });

  const [tabValue, setTabValue] = React.useState(0);
  const [showOutLinks, setShowOutLinks] = React.useState<boolean>(true);

  const handleTabChange = (event: React.SyntheticEvent, newValue: number) => {
    setTabValue(newValue);
  };

  const handleShowOutlinks = (event: React.ChangeEvent<HTMLInputElement>) => {
    setShowOutLinks(event.target.checked);
  };

  // React.useEffect(() => {
  //   console.log(bounds);
  // }, [bounds]);

  return (
    <Wrapper ref={measureRef}>
      <div style={{ width: "100%", height: "50px" }}>
        <Tabs value={tabValue} onChange={handleTabChange} aria-label="tabs">
          <Tab label="3D" id={"tab-0"} aria-controls={"tabpanel-0"} />
          <Tab label="2D" id={"tab-1"} aria-controls={"tabpanel-1"} />
        </Tabs>
      </div>

      <GraphWrapper>
        {tabValue === 0 && (
          <Graph3DNoSSR
            kanjiInfo={kanjiInfo}
            graphData={graphData}
            showOutLinks={showOutLinks}
            triggerFocus={tabValue}
            bounds={bounds}
          />
        )}
        {tabValue === 1 && (
          <Graph2DNoSSR
            kanjiInfo={kanjiInfo}
            graphData={graphData}
            showOutLinks={showOutLinks}
            triggerFocus={tabValue}
            bounds={bounds}
          />
        )}
      </GraphWrapper>
      <div
        css={css`
          position: absolute;
          top: 0;
          right: 0;
        `}
      >
        Out Links:
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
  height: 100%;

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

const GraphWrapper = styled.div`
  position: absolute;
  inset: 0;
  margin-top: 50px;
`;
