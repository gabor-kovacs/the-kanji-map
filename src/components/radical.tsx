"use client";

import * as React from "react";
import Link from "next/link";
import { buildKanjiHref } from "@/lib/kanji-routing";
import { resolveKanjiId } from "@/lib/kanji-variants";
import { buttonVariants } from "@/components/ui/button";
import { cn } from "@/lib/utils";
import RadicalImages from "./radical-images";
import radicallist from "../../data/radicallist.json";

interface Props {
  kanjiInfo: KanjiInfo | null;
  navigableRadicalIds: string[];
}

export const Radical: React.FC<Props> = ({
  kanjiInfo,
  navigableRadicalIds,
}) => {
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

  const normalizeRadicalChar = React.useCallback((value?: string | null) => {
    return value?.normalize("NFKC").trim() ?? "";
  }, []);

  const navigableKanjiIds = React.useMemo(
    () => new Set(navigableRadicalIds.map((id) => resolveKanjiId(id))),
    [navigableRadicalIds]
  );

  // Prefer the Jisho radical symbol for lookups because it already uses the
  // base radical form. Fall back to a normalized KanjiAlive radical character.
  const baseRadicalChar = React.useMemo(() => {
    const fromJisho = normalizeRadicalChar(
      kanjiInfo?.jishoData?.radical?.symbol as string | undefined
    );
    const fromKanjiAlive = normalizeRadicalChar(
      (kanjiInfo as any)?.kanjialiveData?.radical?.character as
        | string
        | undefined
    );
    return fromJisho || fromKanjiAlive;
  }, [kanjiInfo, normalizeRadicalChar]);

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
      return [] as Array<{
        char: string;
        posName: PositionName | null;
        isNavigable: boolean;
      }>;
    let base = radicalMap.get(baseRadicalChar);
    if (!base) base = altToBaseMap.get(baseRadicalChar);
    const alts: string[] = Array.isArray(base?.alternatives)
      ? base.alternatives
      : [];
    return alts.map((char) => {
      const entry = radicalMap.get(char);
      const posName = toPositionName(entry?.positionRomanized || "");
      const isNavigable = navigableKanjiIds.has(resolveKanjiId(char));
      return { char, posName, isNavigable };
    });
  }, [baseRadicalChar, radicalMap, altToBaseMap, toPositionName]);

  const hasBaseRadicalPage =
    !!baseRadicalChar && navigableKanjiIds.has(resolveKanjiId(baseRadicalChar));

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
              {alternatives.map((alt) => {
                const content = (
                  <>
                    <span className="text-base">{alt.char}</span>
                    {alt.posName && (
                      <>
                        <span>(</span>
                        {/* eslint-disable-next-line @next/next/no-img-element */}
                        <img
                          alt={`alt radical position ${alt.posName}`}
                          src={`/radical-positions/${alt.posName}.svg`}
                          className="inline-block size-4 align-middle"
                        />
                        <span>)</span>
                      </>
                    )}
                  </>
                );

                const className = cn(
                  buttonVariants({ variant: "outline", size: "xs" }),
                  "inline-flex items-center gap-2 h-auto py-0.5"
                );

                if (!alt.isNavigable) {
                  return (
                    <span
                      key={`${alt.char}-${alt.posName || "none"}`}
                      className={className}
                    >
                      {content}
                    </span>
                  );
                }

                return (
                  <Link
                    key={`${alt.char}-${alt.posName || "none"}`}
                    href={buildKanjiHref(alt.char)}
                    className={className}
                    aria-label={`Open radical page for ${alt.char}`}
                  >
                    {content}
                  </Link>
                );
              })}
            </div>
          </div>
        )}
      </div>
      <div className="flex flex-col items-center justify-center w-full h-full overflow-hidden">
        {baseRadicalChar && hasBaseRadicalPage && (
          <Link
            href={buildKanjiHref(baseRadicalChar)}
            className="cursor-pointer outline-none focus-visible:underline"
            aria-label={`Open radical page for ${baseRadicalChar}`}
          >
            <h1 className="text-6xl leading-tight sm:text-5xl">
              {baseRadicalChar}
            </h1>
          </Link>
        )}
        {baseRadicalChar && !hasBaseRadicalPage && (
          <h1 className="text-6xl leading-tight sm:text-5xl">{baseRadicalChar}</h1>
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
