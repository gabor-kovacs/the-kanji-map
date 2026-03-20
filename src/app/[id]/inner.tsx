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
import { buildKanjiHref } from "@/lib/kanji-variants";
import { useRouter } from "next/navigation";
import React from "react";

interface KanjiPageContentProps {
  requestedId: string;
  canonicalId: string;
  variantInfo: {
    aliases: string[];
  };
  kanjiInfo: KanjiInfo; // Replace 'any' with the actual type
  graphData: BothGraphData; // Replace 'any' with the actual type
  strokeAnimation: string | null; // Replace 'any' with the actual type
  navigableRadicalIds: string[];
}

export function KanjiPageContent({
  requestedId,
  canonicalId,
  variantInfo,
  kanjiInfo,
  graphData,
  strokeAnimation,
  navigableRadicalIds,
}: KanjiPageContentProps) {
  const isMobile = useMediaQuery({ query: "(max-width: 768px)" });
  const router = useRouter();

  const [isMounted, setIsMounted] = React.useState(false);
  React.useEffect(() => {
    setIsMounted(true);
  }, []);

  React.useEffect(() => {
    if (isMounted && requestedId !== canonicalId) {
      void router.replace(buildKanjiHref(canonicalId));
    }
  }, [canonicalId, isMounted, requestedId, router]);

  // Render placeholder with same structure to prevent layout shift
  if (!isMounted) {
    return (
      <>
        {/* Mobile placeholder */}
        <div className="w-full grow md:hidden overflow-hidden" />
        {/* Desktop placeholder */}
        <div className="w-full grow hidden md:grid grid-cols-1 md:grid-rows-[330px_1fr] overflow-hidden">
          <div className="top grid grid-cols-[252px_1.5fr_1fr] overflow-hidden border-b border-lighter">
            <div className="flex flex-col items-center gap-2 mt-3" />
            <div className="p-4 border-l" />
            <div className="p-4 border-l" />
          </div>
          <div className="bottom grid grid-cols-[2fr_3fr] overflow-hidden">
            <div />
            <div className="border-l" />
          </div>
        </div>
      </>
    );
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
                    variantInfo={variantInfo}
                    graphData={graphData}
                    strokeAnimation={strokeAnimation}
                    screen="mobile"
                  />
                </div>
              ),
            },
            {
              id: 1,
              label: "部首",
              content: (
                <div className="p-4">
                  <Radical
                    kanjiInfo={kanjiInfo}
                    navigableRadicalIds={navigableRadicalIds}
                  />
                </div>
              ),
            },
            {
              id: 2,
              label: "例",
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
      <div className="w-full grow hidden md:grid grid-cols-1 md:grid-rows-[330px_1fr] overflow-hidden">
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
                variantInfo={variantInfo}
                graphData={graphData}
                strokeAnimation={strokeAnimation}
              />
            </div>
          </ScrollArea>
          <div className="p-4 border-l">
            <Radical
              kanjiInfo={kanjiInfo}
              navigableRadicalIds={navigableRadicalIds}
            />
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
