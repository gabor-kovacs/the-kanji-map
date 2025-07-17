"use client";
import * as React from "react";
import RadicalImages from "./radical-images";

interface Props {
  kanjiInfo: KanjiInfo | null;
}

export const Radical: React.FC<Props> = ({ kanjiInfo }) => {
  return (
    <div className="min-h-[330px] relative w-full h-full overflow-hidden grid grid-rows-[36px_100px_1fr] grid-cols-[120px_1fr]">
      <div>
        <h3 className="text-lg font-extrabold">Radical</h3>
      </div>
      <div className="pt-2 w-full h-full overflow-hidden text-sm leading-6 row-span-3">
        {kanjiInfo?.jishoData?.radical?.symbol && (
          <p>
            Radical: <strong>{kanjiInfo?.jishoData?.radical?.symbol}</strong>
          </p>
        )}

        {kanjiInfo?.kanjialiveData?.radical?.name?.hiragana && (
          <p>
            Reading:{" "}
            <strong>
              {kanjiInfo?.kanjialiveData?.radical?.name?.hiragana}
            </strong>
          </p>
        )}
        {kanjiInfo?.jishoData?.radical?.meaning && (
          <p>
            Meaning: <strong>{kanjiInfo?.jishoData?.radical?.meaning}</strong>
          </p>
        )}
        {kanjiInfo?.kanjialiveData?.radical?.strokes && (
          <p>
            Strokes:{" "}
            <strong>{kanjiInfo?.kanjialiveData?.radical?.strokes}</strong>
          </p>
        )}
      </div>
      <div className="flex flex-col items-center justify-center w-full h-full overflow-hidden">
        {kanjiInfo?.jishoData?.radical?.symbol && (
          <h1 className="text-6xl leading-tight sm:text-5xl">
            {kanjiInfo?.jishoData?.radical?.symbol}
          </h1>
        )}
      </div>
      <div className="place-self-center w-20 h-20 relative overflow-hidden">
        {kanjiInfo?.kanjialiveData?.radical?.animation && (
          <RadicalImages
            radicalImageArray={kanjiInfo?.kanjialiveData?.radical?.animation}
          />
        )}
      </div>
    </div>
  );
};
