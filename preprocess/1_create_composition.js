// * Create array to store the graph structure of kanji
/*
the info is stored like this:
{ 
  "家": {
    in: ["宀", "豕" ]
    out: ["稼", "嫁" ]
  },

  "kanji55": {
    in: ...
    out: ...
  }
}
*/

const fs = require("fs");
const path = require("path");
const xml2js = require("xml2js");
const parser = new xml2js.Parser();

const extractKanjiFromEntry = (entry, targetList) => {
  entry?.g &&
    entry.g.forEach((comp) => {
      if (comp["$"]["kvg:element"]) {
        targetList.push(comp["$"]["kvg:element"]);
        extractKanjiFromEntry(comp, targetList);
      }
    });
};

// only the first level of the graph is considered otherwise the data is not always true
const extractComponents = (entry, kanji, inList, outList) => {
  // inList
  if (entry?.g?.[0]?.["$"]["kvg:element"] === kanji) {
    entry?.g?.[0]?.g &&
      entry.g[0].g.forEach((comp) => {
        comp["$"]["kvg:element"] && inList.push(comp["$"]["kvg:element"]);
      });
  }
  // outList
  entry?.g?.[0]?.g &&
    entry.g[0].g.forEach((comp) => {
      if (comp?.["$"]?.["kvg:element"] === kanji) {
        entry?.g?.[0]?.["$"]["kvg:element"] &&
          outList.push(entry?.g?.[0]?.["$"]["kvg:element"]);
      }
    });
};

let db = {};
const rawXML = fs.readFileSync(path.join(__dirname, "kanjivg.xml"));
parser
  .parseStringPromise(rawXML)
  .then((parsedXML) => {
    // create a list with every possible kanji (some do not appear at root level in the xml)
    const allNodes = [];
    parsedXML.kanjivg.kanji.forEach((entry) => {
      const nodesInEntry = [];
      extractKanjiFromEntry(entry, nodesInEntry);
      allNodes.push(...nodesInEntry);
    });
    // remove duplicates
    const allKanji = [...new Set(allNodes)];

    const db = {}; // initialize the db object

    allKanji.forEach((kanji, idx) => {
      console.log(`${idx}/${allKanji.length}`);
      const inList = [];
      const outList = [];
      parsedXML.kanjivg.kanji.forEach((entry) => {
        extractComponents(entry, kanji, inList, outList);
      });

      db[kanji] = { in: [...new Set(inList)], out: [...new Set(outList)] };
    });

    // Ensure the directory exists
    const dirPath = path.join(__dirname, "data");
    if (!fs.existsSync(dirPath)) {
      fs.mkdirSync(dirPath);
    }

    // Write to the file
    const filePath = path.join(dirPath, "composition2.json");
    fs.writeFileSync(filePath, JSON.stringify(db), "utf-8");

    console.log("File written successfully");
  })
  .catch(function (err) {
    console.log("Error reading or processing XML:", err);
  });
