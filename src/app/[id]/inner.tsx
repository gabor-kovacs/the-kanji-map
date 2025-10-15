"use client";

import { useMediaQuery } from "react-responsive";
import { Kanji } from "@/components/kanji";
import { MobileLayout } from "@/components/mobile-layout";
import { Radical } from "@/components/radical";
import { Examples } from "@/components/examples";
import { Graphs } from "@/components/graphs";
import { SearchInput } from "@/components/search-input";
import { DrawInput } from "@/components/draw-input";
import { ScrollArea } from "@/components/ui/scroll-area";
import { SearchIcon } from "lucide-react";
import React from "react";

interface KanjiPageContentProps {
  kanjiInfo: KanjiInfo; // Replace 'any' with the actual type
  graphData: BothGraphData; // Replace 'any' with the actual type
  strokeAnimation: string | null; // Replace 'any' with the actual type
}

export function KanjiPageContent({
  kanjiInfo,
  graphData,
  strokeAnimation,
}: KanjiPageContentProps) {
  const isMobile = useMediaQuery({ query: "(max-width: 768px)" });

  const [isMounted, setIsMounted] = React.useState(false);
  React.useEffect(() => {
    setIsMounted(true);
  }, []);
  if (!isMounted) {
    // Render a placeholder or nothing on the server and initial client render
    return null; // or a loading indicator if appropriate
  }

  if (isMobile) {
    return (
      <div className="w-full grow md:hidden overflow-hidden">
        <MobileLayout
          tabs={[
            {
              id: 0,
              label: "漢字",
              content: (
                <div className="p-4">
                  <Kanji
                    kanjiInfo={kanjiInfo}
                    graphData={graphData}
                    strokeAnimation={strokeAnimation}
                    screen="mobile"
                  />
                </div>
              ),
            },
            {
              id: 1,
              label: "例",
              content: (
                <div className="p-4">
                  <Radical kanjiInfo={kanjiInfo} />
                </div>
              ),
            },
            {
              id: 2,
              label: "部首",
              content: (
                <ScrollArea className="size-full">
                  <Examples kanjiInfo={kanjiInfo} />
                </ScrollArea>
              ),
            },
            {
              id: 3,
              label: "図",
              content: <Graphs kanjiInfo={kanjiInfo} graphData={graphData} />,
            },
            {
              id: 4,
              label: (
                <SearchIcon className="w-4 h-4 inline-block -translate-y-0.5" />
              ),
              content: (
                <div className="relative mt-8 p-4 flex flex-col items-center gap-12">
                  <SearchInput searchPlaceholder="Search kanji..." />
                  <DrawInput />
                </div>
              ),
            },
          ]}
          initialActiveTab={0}
        />
      </div>
    );
  } else {
    return (
      <div className="size-full grow hidden md:grid grid-cols-1 md:grid-rows-[330px_1fr] overflow-hidden">
        <div className="top grid grid-cols-[252px_1.5fr_1fr] overflow-hidden border-b border-lighter">
          <div className="flex flex-col items-center gap-2 mt-3">
            <SearchInput searchPlaceholder="Search..." />
            <DrawInput />
          </div>
          <ScrollArea className="w-full h-full">
            <div className="p-4 border-l">
              <Kanji
                screen="desktop"
                kanjiInfo={kanjiInfo}
                graphData={graphData}
                strokeAnimation={strokeAnimation}
              />
            </div>
          </ScrollArea>
          <div className="p-4 border-l">
            <Radical kanjiInfo={kanjiInfo} />
          </div>
        </div>
        <div className="bottom grid grid-cols-[2fr_3fr] overflow-hidden">
          <ScrollArea className="w-full h-full">
            <Examples kanjiInfo={kanjiInfo} />
          </ScrollArea>
          <div className="border-l">
            <Graphs kanjiInfo={kanjiInfo} graphData={graphData} />
          </div>
        </div>
      </div>
    );
  }
}
