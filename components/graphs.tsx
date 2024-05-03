import * as React from "react";
import Tabs from "@mui/material/Tabs";
import Tab from "@mui/material/Tab";
import { ResizeObserver } from "@juggle/resize-observer";
import styled from "@emotion/styled";
import useMeasure from "react-use-measure";
import Checkbox from "@mui/material/Checkbox";
import ThreeSixtyIcon from "@mui/icons-material/ThreeSixty";
import OutboundIcon from "@mui/icons-material/Outbound";
import CropFreeIcon from "@mui/icons-material/CropFree";
import DeadSpace from "./deadspace";
import dynamic from "next/dynamic";
import { useGraphPreferenceStore } from "../lib/store";

const Graph2DNoSSR = dynamic(() => import("./graph2D"), {
  ssr: false,
});
const Graph3DNoSSR = dynamic(() => import("./graph3D"), {
  ssr: false,
});

interface Props {
  kanjiInfo: KanjiInfo | null;
  graphData: BothGraphData | null;
}

const Graphs: React.FC<Props> = ({ kanjiInfo, graphData }) => {
  const [measureRef, bounds] = useMeasure({
    polyfill: ResizeObserver,
    // debounce: 50,
  });

  const { style, rotate, outLinks, setStyle, setRotate, setOutLinks } =
    useGraphPreferenceStore();

  const [tabValue, _] = React.useState(0);

  const [random, setRandom] = React.useState<number>(Date.now());

  const handleTabChange = (event: React.SyntheticEvent, newValue: number) => {
    newValue === 0 && setStyle("3D");
    newValue === 1 && setStyle("2D");
  };

  const handleOutlinks = () => {
    setOutLinks(!outLinks);
  };

  const handleRotate = () => {
    setRotate(!rotate);
  };

  const handleZoomToFit = () => {
    setRandom(Date.now());
  };

  return (
    <Wrapper ref={measureRef}>
      <div
        style={{
          width: "100%",
          height: "48px",
          borderBottom: "1px solid var(--color-lighter)",
        }}
      >
        <Tabs
          value={style === "3D" ? 0 : 1}
          onChange={handleTabChange}
          aria-label="tabs"
        >
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
      <DeadSpace>
        <GraphWrapper>
          {kanjiInfo && style === "3D" && (
            <Graph3DNoSSR
              kanjiInfo={kanjiInfo}
              graphData={graphData}
              showOutLinks={outLinks}
              triggerFocus={tabValue + random}
              bounds={bounds}
              autoRotate={rotate}
            />
          )}
          {kanjiInfo && style === "2D" && (
            <Graph2DNoSSR
              kanjiInfo={kanjiInfo}
              graphData={graphData}
              showOutLinks={outLinks}
              triggerFocus={tabValue + random}
              bounds={bounds}
            />
          )}
        </GraphWrapper>
      </DeadSpace>
      <Controls>
        <div style={{ display: style === "3D" ? "block" : "none" }}>
          <Checkbox
            sx={checkBoxStyle}
            onChange={handleRotate}
            checked={rotate}
            inputProps={{ "aria-label": "Auto Rotate" }}
            // defaultChecked
            icon={<ThreeSixtyIcon />}
            checkedIcon={<ThreeSixtyIcon />}
          />
        </div>
        <div style={{ gridArea: "outCheck" }}>
          <Checkbox
            sx={checkBoxStyle}
            onChange={handleOutlinks}
            checked={outLinks}
            inputProps={{ "aria-label": "Show out links" }}
            // defaultChecked
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
            // defaultChecked
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
