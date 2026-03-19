import { getAllKanji, getGraphData, getKanjiDataLocal, getStrokeAnimation } from "@/lib";
import { aliasIds, getKanjiVariants, resolveKanjiId } from "@/lib/kanji-variants";
import { Metadata } from "next";
import { KanjiPageContent } from "./inner";
import { Header } from "@/components/header";
import { notFound } from "next/navigation";

export const dynamic = "force-static";

export async function generateMetadata({
  params,
}: {
  params: Promise<{ id: string }>;
}): Promise<Metadata> {
  const { id: urlEncodedId } = await params;
  const id = resolveKanjiId(decodeURIComponent(urlEncodedId));
  return {
    title: id,
    alternates: {
      canonical: `/${encodeURIComponent(id)}`,
    },
  };
}

export async function generateStaticParams() {
  const canonicalIds = getAllKanji().map(({ params: { id } }) => id);

  return [...canonicalIds, ...aliasIds].map((id) => ({
    id,
  }));
}

export default async function KanjiPage({
  params,
}: {
  params: Promise<{ id: string }>;
}) {
  const { id: urlEncodedId } = await params;
  const requestedId = decodeURIComponent(urlEncodedId);
  const canonicalId = resolveKanjiId(requestedId);
  const kanjiInfo = await getKanjiDataLocal(canonicalId);
  const graphData = await getGraphData(canonicalId);
  const strokeAnimation = await getStrokeAnimation(canonicalId);

  if (!kanjiInfo) {
    notFound();
  }

  return (
    <div className="size-full flex flex-col">
      <Header className="w-full" />
      <KanjiPageContent
        requestedId={requestedId}
        canonicalId={canonicalId}
        variantInfo={getKanjiVariants(canonicalId)}
        kanjiInfo={kanjiInfo}
        graphData={graphData}
        strokeAnimation={strokeAnimation}
      />
    </div>
  );
}
