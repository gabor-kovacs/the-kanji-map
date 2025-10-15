"use client";
import * as React from "react";
import RadicalImages from "./radical-images";
import radicallist from "../../data/radicallist.json";

interface Props {
  kanjiInfo: KanjiInfo | null;
}

export const Radical: React.FC<Props> = ({ kanjiInfo }) => {
  // Hardcoded list of available position SVG basenames (from data/radical-positions/*.svg)
  const POSITION_NAMES = [
    "ashi",
    "gyougamae",
    "hakogamae",
    "hen",
    "kanmuri",
    "keigamae",
    "kigamae",
    "kunigamae",
    "mongamae",
    "nyou",
    "tare",
    "tsukuri",
    "tsutsumigamae",
  ] as const;

  type PositionName = (typeof POSITION_NAMES)[number];

  // Prefer the radical character from KanjiAlive; fall back to Jisho
  const baseRadicalChar = React.useMemo(() => {
    const fromKanjiAlive = (kanjiInfo as any)?.kanjialiveData?.radical
      ?.character as string | undefined;
    const fromJisho = kanjiInfo?.jishoData?.radical?.symbol as
      | string
      | undefined;
    return (fromKanjiAlive || fromJisho || "").trim();
  }, [kanjiInfo]);

  // Helper: normalize positionRomanized to a known POSITION_NAMES value
  const toPositionName = React.useCallback(
    (pos?: string | null): PositionName | null => {
      if (!pos) return null;
      const lower = pos.toLowerCase();
      for (const name of POSITION_NAMES) {
        if (lower.includes(name)) return name;
      }
      return null;
    },
    []
  );

  // (moved below maps) Find base radical entry and its metadata

  // Build map: radical -> entry and alt -> base entry
  const radicalMap = React.useMemo(() => {
    const map = new Map<string, any>();
    (radicallist as any[]).forEach((r) => {
      if (r?.radical) map.set(r.radical, r);
    });
    return map;
  }, []);

  const altToBaseMap = React.useMemo(() => {
    const map = new Map<string, any>();
    (radicallist as any[]).forEach((r) => {
      const alts: string[] = Array.isArray((r as any)?.alternatives)
        ? (r as any).alternatives
        : [];
      alts.forEach((a) => {
        if (a && !map.has(a)) map.set(a, r);
      });
    });
    return map;
  }, []);

  // Find base radical entry and its metadata (use reverse map if symbol is an alternate)
  const baseEntry = React.useMemo(() => {
    if (!baseRadicalChar) return null;
    return (
      radicalMap.get(baseRadicalChar) ||
      altToBaseMap.get(baseRadicalChar) ||
      null
    );
  }, [baseRadicalChar, radicalMap, altToBaseMap]);

  // Compute alternatives from the base entry's alternatives array
  const alternatives = React.useMemo(() => {
    if (!baseRadicalChar)
      return [] as Array<{ char: string; posName: PositionName | null }>;
    let base = radicalMap.get(baseRadicalChar);
    if (!base) base = altToBaseMap.get(baseRadicalChar);
    const alts: string[] = Array.isArray(base?.alternatives)
      ? base.alternatives
      : [];
    return alts.map((char) => {
      const entry = radicalMap.get(char);
      const posName = toPositionName(entry?.positionRomanized || "");
      return { char, posName };
    });
  }, [baseRadicalChar, radicalMap, altToBaseMap, toPositionName]);

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
        {baseEntry?.readingJapanese && (
          <p>
            Reading: <strong>{baseEntry.readingJapanese}</strong>
          </p>
        )}
        {baseEntry?.frequency && (
          <p>
            Frequency: <strong>{baseEntry.frequency}</strong>
          </p>
        )}
        {alternatives.length > 0 && (
          <div className="mt-2">
            <p className="">Alternative forms:</p>
            <div className="flex flex-wrap gap-3 mt-1">
              {alternatives.map((alt) => (
                <span
                  key={`${alt.char}-${alt.posName || "none"}`}
                  className="inline-flex items-center gap-2 border rounded px-2 py-0.5"
                >
                  <span className="text-base">{alt.char}</span>
                  {alt.posName && (
                    <>
                      <span>(</span>
                      <img
                        alt={`alt radical position ${alt.posName}`}
                        src={`/radical-positions/${alt.posName}.svg`}
                        className="inline-block size-4 align-middle"
                      />
                      <span>)</span>
                    </>
                  )}
                </span>
              ))}
            </div>
          </div>
        )}
      </div>
      <div className="flex flex-col items-center justify-center w-full h-full overflow-hidden">
        {baseRadicalChar && (
          <h1 className="text-6xl leading-tight sm:text-5xl">
            {baseRadicalChar}
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
