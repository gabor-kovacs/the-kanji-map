// create array to store the graph structure of kanji
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

let db = {};
const rawXML = fs.readFileSync(path.join(__dirname, "kanjivg.xml"));

parser
  .parseStringPromise(rawXML)
  .then((parsedXML) => {

    // print progress
    let progress = 0;
    parsedXML.kanjivg.kanji.forEach((entry, idx) => {
      if (idx >= (parsedXML.kanjivg.kanji.length * progress) / 100) {
        progress += 10;
        console.log(`Working... ${progress}%`);
      }




      /*

      // current element
      const kanji = entry.g[0]["$"]["kvg:element"];

      if (kanji === "戋") {
        console.log("FOUND IT")
        console.dir(entry)
      }

      let inList = [];
      let outList = [];

      // * get inList
      let components = entry.g[0].g;
      if (components) {
        components.forEach((comp) => {
          comp["$"]["kvg:element"] && inList.push(comp["$"]["kvg:element"]);
          // ? TODO kvg:original
        });
      }
      // * get outList
      // if the current kanji is found in the firs composition layer of other elements, add that to the outList
      parsedXML.kanjivg.kanji.forEach((e) => {
        if (e.g[0].g) {
          e.g[0].g.forEach((c) => {
            if (c["$"]["kvg:element"] === kanji) {
              outList.push(e.g[0]["$"]["kvg:element"]);
            }
          });
        }
      });

      if (kanji) {
        db[kanji] = { in: inList, out: outList };
      }
    });

    // * There should not be any duplicates but remove just in case
    Object.entries(db).forEach(([kanji, data]) => {
      db[kanji] = {
        in: [...new Set(data.in)],
        out: [...new Set(data.out)],
      };
    });

    // * output
    console.log(db);
    fs.writeFileSync(
      path.join(__dirname, "composition.json"),
      JSON.stringify(db),
      "utf-8"
      */
    );
  })

  .catch(function (err) {
    console.log("Error reading xml");
  });
