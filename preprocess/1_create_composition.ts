// * Run this script from the root of the project
// * Run this script from the root of the project
import * as fs from "fs";
import * as path from "path";
import { Parser } from "xml2js";

const parser = new Parser();

interface KanjiComponent {
  $: {
    "kvg:element": string;
  };
  g?: KanjiComponent[];
}

interface KanjiEntry {
  g?: KanjiComponent[];
}

interface KanjiGraph {
  in: string[];
  out: string[];
}

const extractKanjiFromEntry = (
  entry: KanjiEntry,
  targetList: string[]
): void => {
  if (entry?.g) {
    entry.g.forEach((comp) => {
      if (comp.$ && comp.$["kvg:element"]) {
        targetList.push(comp.$["kvg:element"]);
        extractKanjiFromEntry(comp, targetList);
      }
    });
  }
};

const extractComponents = (
  entry: KanjiEntry,
  kanji: string,
  inList: string[],
  outList: string[]
): void => {
  // inList
  if (entry?.g?.[0]?.$?.["kvg:element"] === kanji) {
    entry.g[0].g?.forEach((comp) => {
      if (comp.$ && comp.$["kvg:element"]) {
        inList.push(comp.$["kvg:element"]);
      }
    });
  }
  // outList
  entry.g?.[0]?.g?.forEach((comp) => {
    if (comp.$?.["kvg:element"] === kanji) {
      entry.g?.[0]?.$?.["kvg:element"] &&
        outList.push(entry.g[0].$["kvg:element"]);
    }
  });
};

const rawXML = fs.readFileSync(path.join(__dirname, "kanjivg.xml"), "utf-8");
parser
  .parseStringPromise(rawXML)
  .then((parsedXML) => {
    // create a list with every possible kanji (some do not appear at root level in the xml)
    const allNodes: string[] = [];
    parsedXML.kanjivg.kanji.forEach((entry: KanjiEntry) => {
      const nodesInEntry: string[] = [];
      extractKanjiFromEntry(entry, nodesInEntry);
      allNodes.push(...nodesInEntry);
    });
    // remove duplicates
    const allKanji = Array.from(new Set(allNodes));

    const db: Record<string, KanjiGraph> = {}; // initialize the db object

    allKanji.forEach((kanji, idx) => {
      console.log(`${idx}/${allKanji.length}`);
      const inList: string[] = [];
      const outList: string[] = [];
      parsedXML.kanjivg.kanji.forEach((entry: KanjiEntry) => {
        extractComponents(entry, kanji, inList, outList);
      });

      db[kanji] = {
        in: Array.from(new Set(inList)),
        out: Array.from(new Set(outList)),
      };
    });

    // Ensure the directory exists
    const dirPath = path.join(path.dirname(__dirname), "data");
    if (!fs.existsSync(dirPath)) {
      fs.mkdirSync(dirPath);
    }
    // Write to the file
    const filePath = path.join(dirPath, "composition.json");
    fs.writeFileSync(filePath, JSON.stringify(db), "utf-8");

    console.log("File written successfully");
  })
  .catch((err) => {
    console.error("Error reading or processing XML:", err);
  });
