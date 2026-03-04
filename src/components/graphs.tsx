"use client";
import { ToggleGroup, ToggleGroupItem } from "@/components/ui/toggle-group";
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
import { GraphLegend } from "./graph-legend";

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

  const handleStyleChange = (values: string[]) => {
    const nextStyle = values[0];
    if (nextStyle === "2D" || nextStyle === "3D") {
      setStyle(nextStyle);
    }
  };
  const activeControls = React.useMemo(() => {
    const values: string[] = [];
    if (rotate) values.push("rotate");
    if (particles) values.push("particles");
    if (outLinks) values.push("outLinks");
    return values;
  }, [outLinks, particles, rotate]);
  const handleControlsChange = (values: string[]) => {
    const nextValues = new Set(values);
    setRotate(nextValues.has("rotate"));
    setParticles(nextValues.has("particles"));
    setOutLinks(nextValues.has("outLinks"));
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
        <ToggleGroup
          value={[style]}
          onValueChange={handleStyleChange}
          className="overflow-hidden rounded-md [&_[data-slot=toggle-group-item]]:rounded-none"
        >
          <ToggleGroupItem
            value="2D"
            size="sm"
            variant="outline"
            className="h-8  data-[pressed]:bg-accent"
          >
            2D
          </ToggleGroupItem>
          <ToggleGroupItem
            value="3D"
            size="sm"
            variant="outline"
            className="h-8 data-[pressed]:bg-accent"
          >
            3D
          </ToggleGroupItem>
        </ToggleGroup>
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
      <GraphLegend showOutLinks={outLinks} showParticles={particles} />
      <div className="absolute top-0 right-0 p-4 flex gap-1">
        <ToggleGroup
          multiple
          spacing={1}
          value={activeControls}
          onValueChange={handleControlsChange}
        >
          {style === "3D" && (
            <Tooltip>
              <TooltipTrigger
                render={
                  <ToggleGroupItem
                    value="rotate"
                    variant="outline"
                    className="size-8 p-0"
                    aria-label="Autorotate"
                  />
                }
              >
                <RefreshCcwIcon className="size-4" />
              </TooltipTrigger>
              <TooltipContent>
                <p>Autorotate</p>
              </TooltipContent>
            </Tooltip>
          )}

          <Tooltip>
            <TooltipTrigger
              render={
                <ToggleGroupItem
                  value="particles"
                  variant="outline"
                  className="size-8 p-0"
                  aria-label="Show arrow particles"
                />
              }
            >
              <ArrowUpFromDotIcon className="size-4" />
            </TooltipTrigger>
            <TooltipContent>
              <p>Show arrow particles</p>
            </TooltipContent>
          </Tooltip>

          <Tooltip>
            <TooltipTrigger
              render={
                <ToggleGroupItem
                  value="outLinks"
                  variant="outline"
                  className="size-8 p-0"
                  size="sm"
                  aria-label="Show out links"
                />
              }
            >
              <CircleArrowOutUpRightIcon className="size-4" />
            </TooltipTrigger>
            <TooltipContent>
              <p>Show outgoing links</p>
            </TooltipContent>
          </Tooltip>
        </ToggleGroup>
        <div>
          <Tooltip>
            <TooltipTrigger
              render={
                <Button
                  variant="outline"
                  size="icon"
                  aria-label="Fit to screen"
                  onClick={handleZoomToFit}
                />
              }
            >
              <MaximizeIcon className="size-4" />
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
