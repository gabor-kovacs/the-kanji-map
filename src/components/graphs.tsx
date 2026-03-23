"use client";
import { ToggleGroup, ToggleGroupItem } from "@/components/ui/toggle-group";
import {
  Sheet,
  SheetContent,
  SheetDescription,
  SheetFooter,
  SheetHeader,
  SheetTitle,
} from "@/components/ui/sheet";
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
import { usePathname, useRouter } from "next/navigation";
import { useAtom } from "jotai";
import {
  outLinksAtom,
  particlesAtom,
  rotateAtom,
  styleAtom,
} from "@/lib/store";
import { GraphLegend } from "./graph-legend";
import { buildKanjiHref, type MobileTabKey } from "@/lib/kanji-routing";
import {
  resolveKanjiId,
} from "@/lib/kanji-variants";

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
  navigationTab?: MobileTabKey;
  enableNodePreview?: boolean;
}

type GraphPreviewNode = {
  id: string;
  data: KanjiInfo | null;
};

export const Graphs: React.FC<Props> = ({
  kanjiInfo,
  graphData,
  navigationTab,
  enableNodePreview = false,
}) => {
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
  const [previewNode, setPreviewNode] = React.useState<GraphPreviewNode | null>(
    null,
  );

  const handleZoomToFit = () => {
    setRandom(Date.now());
  };

  const pathname = usePathname();
  const router = useRouter();

  const previewKunyomi = previewNode?.data?.jishoData?.kunyomi
    ?.filter(Boolean)
    ?.join("、");
  const previewOnyomi = previewNode?.data?.jishoData?.onyomi
    ?.filter(Boolean)
    ?.join("、");
  const previewMeaning = previewNode?.data?.jishoData?.meaning;
  const previewIsCurrentKanji = previewNode
    ? resolveKanjiId(previewNode.id) === kanjiInfo?.id
    : false;

  React.useEffect(() => {
    setPreviewNode(null);
  }, [kanjiInfo?.id, pathname, style]);

  const handlePreviewOpenChange = (open: boolean) => {
    if (!open) {
      setPreviewNode(null);
    }
  };

  const handleOpenPreviewPage = () => {
    if (!previewNode) {
      return;
    }

    void router.push(
      buildKanjiHref(previewNode.id, {
        tab: previewIsCurrentKanji ? "kanji" : navigationTab ?? null,
      }),
    );
    setPreviewNode(null);
  };

  if (!kanjiInfo) return <></>;

  return (
    <div ref={measureRef} className="relative size-full graphs">
      <div className="absolute top-4 left-4 z-50">
        <ToggleGroup
          value={[style]}
          onValueChange={handleStyleChange}
          className="overflow-hidden rounded-lg border border-input bg-background divide-x divide-border"
        >
          <ToggleGroupItem
            value="2D"
            size="sm"
            className="h-8 rounded-none bg-background data-[pressed]:bg-accent"
          >
            2D
          </ToggleGroupItem>
          <ToggleGroupItem
            value="3D"
            size="sm"
            className="h-8 rounded-none bg-background data-[pressed]:bg-accent"
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
            navigationTab={navigationTab}
            enableNodePreview={enableNodePreview}
            onPreviewNode={setPreviewNode}
            onClosePreview={() => setPreviewNode(null)}
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
            navigationTab={navigationTab}
            enableNodePreview={enableNodePreview}
            onPreviewNode={setPreviewNode}
            onClosePreview={() => setPreviewNode(null)}
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
                    className="size-8 bg-background p-0 data-[pressed]:bg-accent"
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
                  className="size-8 bg-background p-0 data-[pressed]:bg-accent"
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
                  className="size-8 bg-background p-0 data-[pressed]:bg-accent"
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
                  className="!bg-background hover:!bg-muted dark:!bg-background dark:hover:!bg-muted"
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
      <Sheet
        modal={false}
        open={Boolean(previewNode)}
        onOpenChange={handlePreviewOpenChange}
      >
        <SheetContent
          side="bottom"
          overlayClassName="pointer-events-none bg-transparent"
          className="rounded-t-3xl border-t bg-muted px-0 pb-0 pt-3 shadow-2xl"
          onInteractOutside={(event) => event.preventDefault()}
        >
          <div className="mx-auto mb-3 h-1.5 w-12 rounded-full bg-muted-foreground/20" />
          <SheetHeader className="gap-3 px-4 pb-4 pr-12 text-left">
            <SheetTitle className="flex items-center gap-3 text-left">
              <span className="text-4xl leading-none">
                {previewNode?.id ?? ""}
              </span>
              <span className="text-sm font-normal leading-5 text-muted-foreground">
                {previewMeaning || "Open this kanji page from the graph."}
              </span>
            </SheetTitle>
            {(previewKunyomi || previewOnyomi) && (
              <SheetDescription className="space-y-1 text-left">
                {previewKunyomi && (
                  <span className="block">Kunyomi: {previewKunyomi}</span>
                )}
                {previewOnyomi && (
                  <span className="block">Onyomi: {previewOnyomi}</span>
                )}
              </SheetDescription>
            )}
          </SheetHeader>
          <SheetFooter className="border-t bg-muted px-4 py-4">
            <Button className="h-12 w-full text-base" onClick={handleOpenPreviewPage}>
              {previewIsCurrentKanji ? "Show kanji" : "Open page"}
            </Button>
          </SheetFooter>
        </SheetContent>
      </Sheet>
    </div>
  );
};
