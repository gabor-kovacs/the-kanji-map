import composition from "@/../data/composition.json";
import fsPromises from "fs/promises";
import { notFound } from "next/navigation";
import path from "path";
import type { GraphData } from "react-force-graph-3d";

/**
 * This is used by Next.js getStaticPaths to generate possible kanji pages
 */
export const getAllKanji = () => {
  return Object.entries(composition).map(([kanji, _]) => {
    return {
      params: {
        id: kanji,
      },
    };
  });
};

/**
 * Get data for input kanji from KanjiAlive and Jisho.org saved locally
 * @param id input kanji
 */
export const getKanjiDataLocal: (
  id: string
) => Promise<KanjiInfo | null> = async (id) => {
  const filePath = path.join(process.cwd(), "data", "kanji", `${id}.json`);

  try {
    const jsonData = await fsPromises.readFile(filePath, "utf8");

    return JSON.parse(jsonData) as KanjiInfo;
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
  const fileName = `${id.charCodeAt(0)}.svg`;

  for (const directory of SVG_DIRECTORY_LIST) {
    const filePath = path.join(
      process.cwd(),
      "data",
      "animCJK",
      directory,
      fileName
    );
    try {
      const animationData = await fsPromises.readFile(filePath, "utf8");
      // If file is found, return the animation data
      return animationData;
    } catch (error) {
      // If the file is not found, continue to the next directory
      if (
        error instanceof Error &&
        (error as NodeJS.ErrnoException).code === "ENOENT"
      ) {
        continue; // File not found, try the next folder
      } else {
        continue; // しょうがない
      }
    }
  }
  // If the file is not found in any folder, return null or handle accordingly
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
    !composition[el as keyof typeof composition] && notFound();

    composition[el as keyof typeof composition].in.forEach((node) => {
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
    composition[end as keyof typeof composition].in.forEach((start) => {
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
  let inNodeList = [id];
  inNodeList = findNodes(inNodeList);
  const inLinks = createInLinks(inNodeList);

  let outLinks = composition[id as keyof typeof composition].out.map((node) => {
    return { source: id, target: node };
  });

  const outNodeList = composition[id as keyof typeof composition].out;

  const inNodes = await Promise.all(
    inNodeList.map(async (x) => {
      return {
        id: x,
        data: await getKanjiDataLocal(x),
      };
    })
  );

  const outNodes = await Promise.all(
    outNodeList.map(async (x) => {
      return {
        id: x,
        data: await getKanjiDataLocal(x),
      };
    })
  );

  const allNodes = inNodes.concat(outNodes);
  const allLinks = inLinks.concat(outLinks);

  const withOutLinks: GraphData = {
    nodes: allNodes.filter(
      (value, index, self) => index === self.findIndex((t) => t.id === value.id)
    ),
    links: allLinks.filter(
      (value, index, self) =>
        index ===
        self.findIndex(
          (t) => t.source === value.source && t.target === value.target
        )
    ),
  };
  const noOutLinks: GraphData = {
    nodes: inNodes.filter(
      (value, index, self) => index === self.findIndex((t) => t.id === value.id)
    ),
    links: inLinks.filter(
      (value, index, self) =>
        index ===
        self.findIndex(
          (t) => t.source === value.source && t.target === value.target
        )
    ),
  };

  return { withOutLinks, noOutLinks };
};
