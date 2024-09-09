"use client";
import { Tabs, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { Toggle } from "@/components/ui/toggle";
import { ResizeObserver } from "@juggle/resize-observer";
import {
  CircleArrowOutUpRightIcon,
  MaximizeIcon,
  RefreshCcwIcon,
} from "lucide-react";
import dynamic from "next/dynamic";
import * as React from "react";
import useMeasure from "react-use-measure";
import { useGraphPreferenceStore } from "../lib/store";
import { Button } from "./ui/button";
import { Tooltip, TooltipContent, TooltipTrigger } from "./ui/tooltip";

const Graph2DNoSSR = dynamic(() => import("./graph-2D"), {
  ssr: false,
});
const Graph3DNoSSR = dynamic(() => import("./graph-3D"), {
  ssr: false,
});

interface Props {
  kanjiInfo: KanjiInfo | null;
  graphData: BothGraphData | null;
}

export const Graphs: React.FC<Props> = ({ kanjiInfo, graphData }) => {
  const [measureRef, bounds] = useMeasure({
    polyfill: ResizeObserver,
    // debounce: 50,
  });

  const { style, rotate, outLinks, setStyle, setRotate, setOutLinks } =
    useGraphPreferenceStore();

  const [tabValue, _] = React.useState(0);

  const [random, setRandom] = React.useState<number>(Date.now());

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
    <div ref={measureRef} className="relative size-full">
      <div className="absolute top-4 left-4 z-50">
        <Tabs
          defaultValue={style}
          value={style}
          onValueChange={(value) => setStyle(value as "3D" | "2D")}
        >
          <TabsList className="px-1">
            <TabsTrigger value="2D">2D</TabsTrigger>
            <TabsTrigger value="3D">3D</TabsTrigger>
          </TabsList>
        </Tabs>
      </div>
      <div className="absolute inset-0">
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
      </div>
      <div className="absolute top-0 right-0 p-4 flex gap-1">
        <div style={{ display: style === "3D" ? "block" : "none" }}>
          <Tooltip>
            <TooltipTrigger asChild>
              <Toggle
                className="size-10"
                variant="outline"
                aria-label="Autorotate"
                pressed={rotate}
                onPressedChange={handleRotate}
              >
                <RefreshCcwIcon className="size-4" />
              </Toggle>
            </TooltipTrigger>
            <TooltipContent>
              <p>Autorotate</p>
            </TooltipContent>
          </Tooltip>
        </div>
        <div>
          <Tooltip>
            <TooltipTrigger asChild>
              <Toggle
                className="size-10"
                variant="outline"
                aria-label="Show out links"
                pressed={outLinks}
                onPressedChange={handleOutlinks}
              >
                <CircleArrowOutUpRightIcon className="size-4" />
              </Toggle>
            </TooltipTrigger>
            <TooltipContent>
              <p>Show outgoing links</p>
            </TooltipContent>
          </Tooltip>
        </div>
        <div>
          <Tooltip>
            <TooltipTrigger asChild>
              <Button
                variant="outline"
                size="icon"
                aria-label="Fit to screen"
                onClick={handleZoomToFit}
              >
                <MaximizeIcon className="size-4" />
              </Button>
            </TooltipTrigger>
            <TooltipContent>
              <p>Zoom to fit</p>
            </TooltipContent>
          </Tooltip>
        </div>
      </div>
    </div>
  );
};
