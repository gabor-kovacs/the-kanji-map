import "server-only";

import composition from "@/../data/composition.json";
import radicallist from "@/../data/radicallist.json";
import searchlist from "@/../data/searchlist.json";
import fsPromises from "fs/promises";
import { notFound } from "next/navigation";
import type { GraphData } from "react-force-graph-3d";
import { canonicalizeKanjiIds, resolveKanjiId } from "./kanji-variants";

import {
  SearchListEntry,
  scoreSearchEntry,
} from "@/lib/search-score";

type CompositionEntry = {
  in: string[];
  out: string[];
};

const searchFallbackByKanji = (() => {
  const fallbackEntries = new Map<string, SearchListEntry>();

  (searchlist as SearchListEntry[]).forEach((entry) => {
    const canonicalKanji = resolveKanjiId(entry.k);
    const existing = fallbackEntries.get(canonicalKanji);

    if (
      !existing ||
      scoreSearchEntry(entry, canonicalKanji) >
        scoreSearchEntry(existing, canonicalKanji)
    ) {
      fallbackEntries.set(canonicalKanji, {
        ...entry,
        k: canonicalKanji,
      });
    }
  });

  return fallbackEntries;
})();

const normalizeRadicalChar = (value?: string | null) =>
  value?.normalize("NFKC").trim() ?? "";

const radicalBaseByChar = (() => {
  const baseByChar = new Map<string, string>();

  (radicallist as any[]).forEach((entry) => {
    const radical = normalizeRadicalChar(entry?.radical);
    const original = normalizeRadicalChar(entry?.original);
    const base = original || radical;

    if (radical && base && !baseByChar.has(radical)) {
      baseByChar.set(radical, base);
    }

    const alternatives: string[] = Array.isArray(entry?.alternatives)
      ? entry.alternatives
      : [];
    alternatives.forEach((alternative) => {
      const normalizedAlternative = normalizeRadicalChar(alternative);
      if (
        normalizedAlternative &&
        base &&
        !baseByChar.has(normalizedAlternative)
      ) {
        baseByChar.set(normalizedAlternative, base);
      }
    });
  });

  return baseByChar;
})();

const canonicalComposition = (() => {
  const mergedEntries = new Map<
    string,
    { in: Set<string>; out: Set<string> }
  >();

  Object.entries(composition as Record<string, CompositionEntry>).forEach(
    ([rawId, entry]) => {
      const canonicalId = resolveKanjiId(rawId);
      const mergedEntry = mergedEntries.get(canonicalId) ?? {
        in: new Set<string>(),
        out: new Set<string>(),
      };

      canonicalizeKanjiIds(entry.in).forEach((node) => {
        if (node !== canonicalId) {
          mergedEntry.in.add(node);
        }
      });

      canonicalizeKanjiIds(entry.out).forEach((node) => {
        if (node !== canonicalId) {
          mergedEntry.out.add(node);
        }
      });

      mergedEntries.set(canonicalId, mergedEntry);
    }
  );

  return Object.fromEntries(
    Array.from(mergedEntries.entries(), ([id, entry]) => [
      id,
      {
        in: Array.from(entry.in),
        out: Array.from(entry.out),
      },
    ])
  ) as Record<string, CompositionEntry>;
})();

const readKanjiDataFile = async (id: string) => {
  const filePath = `${process.cwd()}/data/kanji/${id}.json`;
  const jsonData = await fsPromises.readFile(filePath, "utf8");
  return JSON.parse(jsonData) as KanjiInfo;
};

const toGraphNodeData = (info: KanjiInfo | null): GraphNodeData | null =>
  info?.jishoData
    ? {
        kunyomi: info.jishoData.kunyomi ?? [],
        onyomi: info.jishoData.onyomi ?? [],
        meaning: info.jishoData.meaning ?? "",
      }
    : null;

const uniqueNodesById = (nodes: GraphNode[]) =>
  Array.from(new Map(nodes.map((node) => [node.id, node])).values());

const uniqueLinks = (links: Array<{ source: string; target: string }>) =>
  Array.from(
    new Map(
      links.map((link) => [`${link.source}->${link.target}`, link]),
    ).values(),
  );

/**
 * This is used by Next.js getStaticPaths to generate possible kanji pages
 */
export const getAllKanji = () => {
  return Object.keys(canonicalComposition).map((kanji) => {
    return {
      params: {
        id: kanji,
      },
    };
  });
};

export const getNavigableRadicalIds = () => {
  const pageIds = new Set(Object.keys(canonicalComposition));
  const radicalIds = new Set<string>();

  (radicallist as any[]).forEach((entry) => {
    const candidates = [
      entry?.radical,
      ...(Array.isArray(entry?.alternatives) ? entry.alternatives : []),
    ];

    candidates.forEach((candidate) => {
      const canonicalId = resolveKanjiId(normalizeRadicalChar(candidate));
      if (canonicalId && pageIds.has(canonicalId)) {
        radicalIds.add(canonicalId);
      }
    });
  });

  return Array.from(radicalIds);
};

/**
 * Get data for input kanji from KanjiAlive and Jisho.org saved locally
 * @param id input kanji
 */
export const getKanjiDataLocal: (
  id: string
) => Promise<KanjiInfo | null> = async (id) => {
  const normalizedId = resolveKanjiId(id.trim());

  // Use Array.from to properly count characters (handles surrogate pairs)
  if (Array.from(normalizedId).length !== 1) {
    return null;
  }

  // Compute path inline to minimize Turbopack static analysis
  try {
    const effectiveData = await readKanjiDataFile(normalizedId);
    const searchFallback = searchFallbackByKanji.get(normalizedId);
    const baseRadicalId = radicalBaseByChar.get(normalizedId);
    const shouldHydrateRadicalFromBase =
      !!baseRadicalId &&
      baseRadicalId !== normalizedId &&
      !effectiveData.kanjialiveData?.radical;
    const baseRadicalDataPromise = shouldHydrateRadicalFromBase
      ? readKanjiDataFile(baseRadicalId).catch(() => null)
      : Promise.resolve(null);

    const shouldHydrateFromSearchFallback =
      (!effectiveData.jishoData || effectiveData.jishoData.found === false) &&
      !!searchFallback;
    const baseRadicalData = await baseRadicalDataPromise;

    return {
      ...effectiveData,
      id: normalizedId,
      kanjialiveData:
        shouldHydrateRadicalFromBase &&
        baseRadicalData?.kanjialiveData?.radical
          ? {
              ...(effectiveData.kanjialiveData ?? {}),
              radical: baseRadicalData.kanjialiveData.radical,
            }
          : effectiveData.kanjialiveData,
      jishoData: shouldHydrateFromSearchFallback
        ? {
            ...(effectiveData.jishoData ?? {}),
            query: normalizedId,
            found: true,
            kunyomi: searchFallback?.r
              ? searchFallback.r.split(", ").filter(Boolean)
              : [],
            meaning: searchFallback?.m ?? "",
            jlptLevel: searchFallback?.j ?? null,
            strokeCount: searchFallback?.s ?? null,
          }
        : effectiveData.jishoData,
    };
  } catch (error) {
    console.error("Failed to read or parse kanji data:", error);
    return null;
    // throw new Error("Failed to load kanji data");
  }
};
// List of directories to check the kanji animation for, in order of preference (some "kanji" may actually be Chinese, but the stroke animation can still be shown)
const SVG_DIRECTORY_LIST = [
  "svgsJa",
  "svgsJaSpecial",
  "svgsZhHans",
  "svgsZhHant",
  "svgsZhHansSpecial",
  "svgsKo",
  "svgsKoSpecial",
];

/**
 * Retrieves the stroke animation data for a given character ID.
 *
 * @param id The ID of the character to retrieve the stroke animation for.
 * @returns The stroke animation data in SVG format, or null if no animation data is found.
 */
export const getStrokeAnimation = async (id: string) => {
  const normalizedId = resolveKanjiId(id.trim());

  // Use Array.from to properly count characters (handles surrogate pairs)
  if (Array.from(normalizedId).length !== 1) {
    return null;
  }

  // Use codePointAt for proper Unicode handling (charCodeAt only returns high surrogate)
  const fileName = `${normalizedId.codePointAt(0)}.svg`;
  const cwd = process.cwd();

  for (const directory of SVG_DIRECTORY_LIST) {
    const filePath = `${cwd}/data/animCJK/${directory}/${fileName}`;
    try {
      return await fsPromises.readFile(filePath, "utf8");
    } catch {
      continue;
    }
  }
  return null;
};
/**
 * Recursively finds and returns all connected nodes in the composition graph starting from the given array of nodes.
 *
 * @param array - An array of strings representing the initial nodes to start the search from.
 * @returns An array of strings containing all connected nodes in the composition graph.
 */
const findNodes = (array: string[]) => {
  array.forEach((el) => {
    !canonicalComposition[el] && notFound();

    canonicalComposition[el].in.forEach((node) => {
      if (!array.includes(node)) {
        array.push(node);
        findNodes(array);
      }
    });
  });
  return array;
};

/**
 * Creates links between elements in the input array based on the composition data.
 *
 * @param array - An array of strings representing elements.
 * @returns An array of objects with 'source' and 'target' properties representing the links between elements.
 */
const createInLinks = (array: string[]) => {
  const links: { source: string; target: string }[] = [];
  array.forEach((end) => {
    canonicalComposition[end].in.forEach((start) => {
      if (start !== end) {
        links.push({ source: start, target: end });
      }
    });
  });
  return links;
};

/**
 * Return 2 objects for graph data:
 * with and without out links
 * @param id input kanji
 */
export const getGraphData = async (id: string) => {
  const normalizedId = resolveKanjiId(id);

  if (!canonicalComposition[normalizedId]) {
    notFound();
  }

  let inNodeList = [normalizedId];
  inNodeList = findNodes(inNodeList);
  const inLinks = createInLinks(inNodeList);

  let outLinks = canonicalComposition[normalizedId].out.map((node) => {
    return { source: normalizedId, target: node };
  });

  const outNodeList = canonicalComposition[normalizedId].out;

  const [inNodes, outNodes] = await Promise.all([
    Promise.all(
      inNodeList.map(async (x): Promise<GraphNode> => {
        return {
          id: x,
          data: toGraphNodeData(await getKanjiDataLocal(x)),
        };
      })
    ),
    Promise.all(
      outNodeList.map(async (x): Promise<GraphNode> => {
        return {
          id: x,
          data: toGraphNodeData(await getKanjiDataLocal(x)),
        };
      })
    ),
  ]);

  const allNodes = inNodes.concat(outNodes);
  const allLinks = inLinks.concat(outLinks);

  const withOutLinks: GraphData = {
    nodes: uniqueNodesById(allNodes),
    links: uniqueLinks(allLinks),
  };
  const noOutLinks: GraphData = {
    nodes: uniqueNodesById(inNodes),
    links: uniqueLinks(inLinks),
  };

  return { withOutLinks, noOutLinks };
};
