"use client";
import * as React from "react";
import { Button } from "./ui/button";
import { CirclePlayIcon } from "lucide-react";

export const Examples = ({ kanjiInfo }: { kanjiInfo: KanjiInfo | null }) => {
  const playSound = (url: string) => {
    const audio = new Audio(url);
    void audio.play();
  };

  const highlightKanji = (text: string) => {
    if (!kanjiInfo) return;
    const textArray = text?.split(kanjiInfo.id);
    return (
      <span>
        {textArray.map((item, index) => (
          <React.Fragment key={index}>
            {item}
            {index !== textArray.length - 1 && <b>{kanjiInfo?.id}</b>}
          </React.Fragment>
        ))}
      </span>
    );
  };

  return (
    <div className="size-full grid grid-rows-[36px_1fr] p-4">
      <div>
        <h3 className="text-lg font-extrabold">Examples</h3>
      </div>
      <div>
        {/* KANJIALIVE With AUDIO */}
        {kanjiInfo?.kanjialiveData?.examples && (
          <h5 className="text-foreground/50 text-sm my-2">
            Examples with audio
          </h5>
        )}
        {kanjiInfo?.kanjialiveData?.examples?.map(
          (example: any, index: number) => {
            return (
              <div
                className="flex justify-between align-end odd:bg-muted rounded-lg items-center pl-2"
                key={index}
              >
                <p>
                  <span>
                    {highlightKanji(example?.japanese)}
                    &nbsp;&nbsp;&nbsp;
                  </span>
                  <span>
                    {example?.meaning?.english}
                    {"  "}
                  </span>
                </p>
                <Button
                  aria-label="Play sound"
                  variant="link"
                  size="icon"
                  onClick={() =>
                    example && example.audio && playSound(example?.audio?.mp3)
                  }
                >
                  <CirclePlayIcon className="size-5" />
                </Button>
              </div>
            );
          }
        )}
        {/* JISHO */}
        {kanjiInfo?.jishoData?.onyomiExamples &&
          kanjiInfo?.jishoData?.onyomiExamples?.length !== 0 && (
            <h5 className="text-foreground/50 text-sm my-2">Onyomi Examples</h5>
          )}
        {kanjiInfo?.jishoData?.onyomiExamples?.map(
          (onExample: any, index: number) => (
            <div
              key={index}
              className="flex justify-between align-end odd:bg-muted rounded-lg items-center p-2"
            >
              <p>
                {highlightKanji(onExample?.example)}
                {"  "}（{onExample?.reading}）{"  "}
                {onExample?.meaning}
              </p>
            </div>
          )
        )}
        {kanjiInfo?.jishoData?.kunyomiExamples &&
          kanjiInfo?.jishoData?.kunyomiExamples?.length !== 0 && (
            <h5 className="text-foreground/50 text-sm my-2">
              Kunyomi Examples
            </h5>
          )}
        {kanjiInfo?.jishoData?.kunyomiExamples?.map(
          (kunExample: any, index: number) => (
            <div
              key={index}
              className="flex justify-between align-end odd:bg-muted rounded-lg items-center p-2"
            >
              <p>
                {highlightKanji(kunExample?.example)}
                {"  "}（{kunExample?.reading}）{"  "}
                {kunExample?.meaning}
              </p>
            </div>
          )
        )}
      </div>
    </div>
  );
};
