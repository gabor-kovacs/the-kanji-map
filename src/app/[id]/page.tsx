import composition from "@/../data/composition.json";
import { DrawInput } from "@/components/draw-input";
import { Examples } from "@/components/examples";
import { Graphs } from "@/components/graphs";
import { Header } from "@/components/header";
import { Kanji } from "@/components/kanji";
import { MobileLayout } from "@/components/mobile-layout";
import { Radical } from "@/components/radical";
import { SearchInput } from "@/components/search-input";
import { ScrollArea } from "@/components/ui/scroll-area";
import { getGraphData, getKanjiDataLocal, getStrokeAnimation } from "@/lib";
import { SearchIcon } from "lucide-react";
import { Metadata } from "next";

export async function generateMetadata({
  params,
}: {
  params: { id: string };
}): Promise<Metadata> {
  const { id: urlEncodedId } = params;
  const id = decodeURIComponent(urlEncodedId);
  return {
    title: id,
  };
}

export async function generateStaticParams() {
  const kanjis = Object.keys(composition);
  return kanjis.map((kanji) => ({
    id: kanji,
  }));
}

export default async function KanjiPage({
  params,
}: {
  params: { id: string };
}) {
  const { id: urlEncodedId } = params;
  const id = decodeURIComponent(urlEncodedId);
  const kanjiInfo = await getKanjiDataLocal(id);
  const graphData = await getGraphData(id);
  const strokeAnimation = await getStrokeAnimation(id);

  return (
    <div className="size-full flex flex-col overflow-hidden">
      <Header className="relative w-full shrink-0" />
      {/* MOBILE */}
      <div className="w-full flex-grow md:hidden overflow-hidden">
        <MobileLayout
          tabs={[
            {
              id: 0,

              label: "漢字",
              content: (
                <div className="p-4">
                  <Kanji
                    {...{ kanjiInfo, graphData, strokeAnimation }}
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
              content: <Graphs {...{ kanjiInfo, graphData }} />,
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
      {/* DESKTOP */}
      <div className="size-full flex-grow hidden md:grid grid-cols-1 md:grid-rows-[330px,1fr] overflow-hidden">
        <div className="top grid grid-cols-[252px_1fr_1fr] overflow-hidden border-b border-lighter">
          <div className="flex flex-col items-center gap-2 mt-3">
            <SearchInput searchPlaceholder="Search..." />
            <DrawInput />
          </div>
          <div className="p-4 border-l">
            <Kanji
              screen="desktop"
              {...{ kanjiInfo, graphData, strokeAnimation }}
            />
          </div>
          <div className="p-4 border-l">
            <Radical kanjiInfo={kanjiInfo} />
          </div>
        </div>
        <div className="bottom grid grid-cols-[2fr_3fr] overflow-hidden">
          <ScrollArea className="w-full h-full">
            <Examples kanjiInfo={kanjiInfo} />
          </ScrollArea>
          <div className="border-l">
            <Graphs {...{ kanjiInfo, graphData }} />
          </div>
        </div>
      </div>
    </div>
  );
}
