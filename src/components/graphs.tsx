"use client";
import { Tabs, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { Toggle } from "@/components/ui/toggle";
import { cn } from "@/lib/utils";
import { ResizeObserver } from "@juggle/resize-observer";
import {
  ArrowUpFromDotIcon,
  CircleArrowOutUpRightIcon,
  MaximizeIcon,
  RefreshCcwIcon,
} from "lucide-react";
import dynamic from "next/dynamic";
import * as React from "react";
import useMeasure from "react-use-measure";
import { Button } from "./ui/button";
import { Tooltip, TooltipContent, TooltipTrigger } from "./ui/tooltip";
import { usePathname } from "next/navigation";
import { useAtom } from "jotai";
import {
  outLinksAtom,
  particlesAtom,
  rotateAtom,
  styleAtom,
} from "@/lib/store";

const Graph2DNoSSR = dynamic(() => import("./graph-2D"), {
  ssr: false,
  loading: () => <div />,
});
const Graph3DNoSSR = dynamic(() => import("./graph-3D"), {
  ssr: false,
  loading: () => <div />,
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

  const [style, setStyle] = useAtom(styleAtom);
  const [rotate, setRotate] = useAtom(rotateAtom);
  const [outLinks, setOutLinks] = useAtom(outLinksAtom);
  const [particles, setParticles] = useAtom(particlesAtom);

  const handleRotateChange = (value: boolean) => {
    setRotate(value);
  };
  const handleStyleChange = (value: string) => {
    setStyle(value as "3D" | "2D");
  };
  const handleOutLinksChange = (value: boolean) => {
    setOutLinks(value);
  };
  const handleParticlesChange = (value: boolean) => {
    setParticles(value);
  };

  const [tabValue] = React.useState(0);
  const [random, setRandom] = React.useState<number>(Date.now());

  const handleZoomToFit = () => {
    setRandom(Date.now());
  };

  const pathname = usePathname();

  if (!kanjiInfo) return <></>;

  return (
    <div ref={measureRef} className="relative size-full graphs">
      <div className="absolute top-4 left-4 z-50">
        <Tabs
          defaultValue={style}
          value={style}
          onValueChange={handleStyleChange}
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
            key={tabValue + random + pathname}
            kanjiInfo={kanjiInfo}
            graphData={graphData}
            showOutLinks={outLinks}
            showParticles={particles}
            autoRotate={rotate}
            triggerFocus={tabValue + random}
            bounds={bounds}
          />
        )}
        {kanjiInfo && style === "2D" && (
          <Graph2DNoSSR
            kanjiInfo={kanjiInfo}
            graphData={graphData}
            showOutLinks={outLinks}
            showParticles={particles}
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
                className={cn("size-10", rotate ? "bg-accent" : "")}
                variant="outline"
                aria-label="Autorotate"
                pressed={rotate}
                onPressedChange={handleRotateChange}
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
                className={cn("size-10", particles ? "bg-accent" : "")}
                variant="outline"
                aria-label="Show arrow particles"
                pressed={particles}
                onPressedChange={handleParticlesChange}
              >
                <ArrowUpFromDotIcon className="size-4" />
              </Toggle>
            </TooltipTrigger>
            <TooltipContent>
              <p>Show arrow particles</p>
            </TooltipContent>
          </Tooltip>
        </div>
        <div>
          <Tooltip>
            <TooltipTrigger asChild>
              <Toggle
                className={cn("size-10", outLinks ? "bg-accent" : "")}
                variant="outline"
                aria-label="Show out links"
                pressed={outLinks}
                onPressedChange={handleOutLinksChange}
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
