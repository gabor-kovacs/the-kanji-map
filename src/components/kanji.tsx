"use client";

import * as React from "react";
import { joyoList } from "@/../data/joyo";
import { jinmeiyoList } from "@/../data/jinmeiyo";

interface Props {
  kanjiInfo: KanjiInfo | null;
  graphData: BothGraphData | null;
  strokeAnimation: string | null;
  screen?: "mobile" | "desktop";
}

// Helper function to make the clip-path id unique
const makeSvgUnique = (svgContent: string, screen: string): string => {
  // Replace all occurrences of id="something" and xlink:href="#something" to make them unique
  return svgContent
    .replace(/id="(\w+)"/g, `id="$1-${screen}"`) // Update ids
    .replace(/xlink:href="#(\w+)"/g, `xlink:href="#$1-${screen}"`) // Update references
    .replace(/clip-path="url\(#(\w+)\)"/g, `clip-path="url(#$1-${screen})"`); // Update clip-path references
};

export const Kanji = ({
  kanjiInfo,
  graphData,
  strokeAnimation,
  screen,
}: Props) => {
  // restarting stroke animation
  const [hash, setHash] = React.useState(Date.now());

  const restartAnimation = () => {
    setHash(Date.now);
  };

  return (
    <div className="relative size-full overflow-hidden grid gap-4 md:gap-2 grid-rows-[36px_1fr_1fr] grid-cols-[100px_1fr]">
      <div>
        <h3 className="text-lg font-extrabold">Kanji</h3>
      </div>
      <div className="pt-2 w-full h-full overflow-auto text-sm leading-6 row-span-3">
        {kanjiInfo && joyoList?.includes(kanjiInfo.id) && (
          <p>
            <strong>Jōyō kanji</strong>
            {kanjiInfo?.jishoData?.taughtIn && (
              <span>
                , Taught in <strong>{kanjiInfo?.jishoData?.taughtIn}</strong>
              </span>
            )}
          </p>
        )}
        {kanjiInfo && jinmeiyoList?.includes(kanjiInfo.id) && (
          <p>Jinmeiyō kanji, used in names</p>
        )}

        {kanjiInfo?.jishoData?.jlptLevel && (
          <p>
            JLPT level: <strong>{kanjiInfo?.jishoData?.jlptLevel}</strong>
          </p>
        )}
        {kanjiInfo?.jishoData?.newspaperFrequencyRank && (
          <p>
            <strong>{kanjiInfo?.jishoData?.newspaperFrequencyRank}</strong> of
            2500 most used kanji in newspapers
          </p>
        )}
        {kanjiInfo?.jishoData?.strokeCount && (
          <p>
            Stroke count: <strong>{kanjiInfo?.jishoData?.strokeCount}</strong>
          </p>
        )}
        {kanjiInfo?.jishoData?.meaning && (
          <>
            <p>
              Meaning: <strong>{kanjiInfo?.jishoData?.meaning}</strong>
            </p>
          </>
        )}
        {kanjiInfo?.jishoData?.kunyomi && (
          <>
            <p>
              Kunyomi: <strong>{kanjiInfo.jishoData.kunyomi.join(", ")}</strong>
            </p>
          </>
        )}
        {kanjiInfo?.jishoData?.onyomi && (
          <>
            <p>
              Onyomi: <strong>{kanjiInfo.jishoData.onyomi.join(", ")}</strong>
            </p>
          </>
        )}

        {graphData?.noOutLinks?.links && (
          <>
            <p>
              {graphData.noOutLinks.links.filter(
                (link: any) => link.target === kanjiInfo?.id
              ).length > 0 && "Composition: "}
              {graphData.noOutLinks.links
                .filter((link: any) => link.target === kanjiInfo?.id)
                .map((link: any) => link.source)
                .map((comp: any, index: number) => (
                  <span key={index}>{comp} </span>
                ))}
            </p>
          </>
        )}
      </div>

      <div className="flex flex-col items-center justify-center w-full h-full overflow-hidden [grid-area:'main']">
        <h1 className="text-6xl leading-tight sm:text-5xl">{kanjiInfo?.id}</h1>
      </div>
      <div className="flex flex-col items-center justify-center w-full h-full overflow-hidden ">
        {strokeAnimation && (
          <div
            className="cursor-pointer w-20 h-20 kanji-svg-container md:something"
            onClick={restartAnimation}
            key={hash}
            dangerouslySetInnerHTML={{
              __html: makeSvgUnique(strokeAnimation, screen ?? "unknown"),
            }}
          />
        )}
      </div>
    </div>
  );
};
