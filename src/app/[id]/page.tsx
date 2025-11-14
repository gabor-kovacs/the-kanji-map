import composition from "@/../data/composition.json";
import { getGraphData, getKanjiDataLocal, getStrokeAnimation } from "@/lib";
import { Metadata } from "next";
import { KanjiPageContent } from "./inner";
import { Header } from "@/components/header";

export const dynamic = "force-static";

export async function generateMetadata({
  params,
}: {
  params: Promise<{ id: string }>;
}): Promise<Metadata> {
  const { id: urlEncodedId } = await params;
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
  params: Promise<{ id: string }>;
}) {
  const { id: urlEncodedId } = await params;
  const id = decodeURIComponent(urlEncodedId);
  const kanjiInfo = await getKanjiDataLocal(id);
  const graphData = await getGraphData(id);
  const strokeAnimation = await getStrokeAnimation(id);

  return (
    <div className="size-full ">
      <Header className="w-full" />
      {kanjiInfo && (
        <KanjiPageContent
          kanjiInfo={kanjiInfo}
          graphData={graphData}
          strokeAnimation={strokeAnimation}
        />
      )}
    </div>
  );
}
