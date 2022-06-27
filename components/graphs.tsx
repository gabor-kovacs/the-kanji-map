import * as React from "react";
import Tabs from "@mui/material/Tabs";
import Tab from "@mui/material/Tab";

import Checkbox from "@mui/material/Checkbox";

import { ResizeObserver } from "@juggle/resize-observer";
import useMeasure from "react-use-measure";

import dynamic from "next/dynamic";
const Graph2DNoSSR = dynamic(() => import("./graph2D"), {
  ssr: false,
});
const Graph3DNoSSR = dynamic(() => import("./graph3D"), {
  ssr: false,
});

import styled from "@emotion/styled";
import { css } from "@emotion/react";
import ThreeSixtyIcon from "@mui/icons-material/ThreeSixty";
import OutboundIcon from "@mui/icons-material/Outbound";
import CropFreeIcon from "@mui/icons-material/CropFree";
type KanjiInfo = {
  id: string;
  kanjialiveData?: any;
  jishoData?: any;
};

interface Props {
  kanjiInfo: KanjiInfo | null;
  graphData: any;
}

const Graphs: React.FC<Props> = ({ kanjiInfo, graphData }) => {
  const [measureRef, bounds] = useMeasure({
    polyfill: ResizeObserver,
    // debounce: 50,
  });

  const [tabValue, setTabValue] = React.useState(0);
  const [showOutLinks, setShowOutLinks] = React.useState<boolean>(true);
  const [autoRotate, setAutoRotate] = React.useState<boolean>(true);
  const [random, setRandom] = React.useState<number>(Date.now());

  const handleTabChange = (event: React.SyntheticEvent, newValue: number) => {
    setTabValue(newValue);
  };

  const handleShowOutlinks = (event: React.ChangeEvent<HTMLInputElement>) => {
    setShowOutLinks(event.target.checked);
  };

  const handleAutoRotate = (event: React.ChangeEvent<HTMLInputElement>) => {
    setAutoRotate(event.target.checked);
  };
  const handleZoomToFit = (event: React.ChangeEvent<HTMLInputElement>) => {
    setRandom(Date.now());
  };

  React.useEffect(() => {
    console.log(bounds);
  }, [bounds]);

  return (
    <Wrapper ref={measureRef}>
      <div
        style={{
          width: "100%",
          height: "48px",
          borderBottom: "1px solid var(--color-lighter)",
        }}
      >
        <Tabs value={tabValue} onChange={handleTabChange} aria-label="tabs">
          <Tab
            style={{ color: "var(--color-light)" }}
            label="3D"
            id={"tab-0"}
            aria-controls={"tabpanel-0"}
          />
          <Tab
            style={{ color: "var(--color-light)" }}
            label="2D"
            id={"tab-1"}
            aria-controls={"tabpanel-1"}
          />
        </Tabs>
      </div>

      <GraphWrapper>
        {kanjiInfo && tabValue === 0 && (
          <Graph3DNoSSR
            kanjiInfo={kanjiInfo}
            graphData={graphData}
            showOutLinks={showOutLinks}
            triggerFocus={tabValue + random}
            bounds={bounds}
            autoRotate={autoRotate}
          />
        )}
        {kanjiInfo && tabValue === 1 && (
          <Graph2DNoSSR
            kanjiInfo={kanjiInfo}
            graphData={graphData}
            showOutLinks={showOutLinks}
            triggerFocus={tabValue + random}
            bounds={bounds}
          />
        )}
      </GraphWrapper>
      <Controls>
        <div style={{ display: tabValue === 0 ? "block" : "none" }}>
          <Checkbox
            sx={checkBoxStyle}
            onChange={handleAutoRotate}
            inputProps={{ "aria-label": "Auto Rotate" }}
            defaultChecked
            icon={<ThreeSixtyIcon />}
            checkedIcon={<ThreeSixtyIcon />}
          />
        </div>
        <div style={{ gridArea: "outCheck" }}>
          <Checkbox
            sx={checkBoxStyle}
            onChange={handleShowOutlinks}
            inputProps={{ "aria-label": "Show out links" }}
            defaultChecked
            icon={<OutboundIcon />}
            checkedIcon={<OutboundIcon />}
          />
        </div>
        <div style={{ gridArea: "zoomCheck" }}>
          <Checkbox
            sx={[
              {
                color: "var(--color-light)",
              },
              {
                "&.Mui-checked": {
                  color: "var(--color-light)",
                },
              },
            ]}
            onChange={handleZoomToFit}
            inputProps={{ "aria-label": "Zoom to fit" }}
            defaultChecked
            icon={<CropFreeIcon />}
            checkedIcon={<CropFreeIcon />}
          />
        </div>
      </Controls>
    </Wrapper>
  );
};

export default Graphs;

const Wrapper = styled.div`
  position: relative;
  width: 100%;
  height: 100%;
  & .Mui-selected {
    color: var(--color-primary) !important;
  }
  & .MuiTabs-indicator {
    background-color: var(--color-primary) !important;
  }
`;

const GraphWrapper = styled.div`
  position: absolute;
  inset: 0;
  margin-top: 48px;
`;

const Controls = styled.div`
  position: absolute;
  top: 0;
  right: 0;
  height: 48px;
  place-items: center;
  display: grid;
  grid-template-areas: "rotateCheck outCheck zoomCheck";

  p {
    padding-top: 2px;
    color: var(--color-light);
  }
`;

const checkBoxStyle = [
  {
    color: "var(--color-light)",
  },
  {
    "&.Mui-checked": {
      color: "var(--color-primary)",
    },
  },
];
