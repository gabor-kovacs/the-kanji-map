const searchList = require("./searchlist.json");

const fs = require("fs");
const path = require("path");
const { parse } = require("csv-parse");

let count = 0;
const radicallist = [];

fs.createReadStream("./kanji-radicals.csv")
  .pipe(parse({ delimiter: ",", from_line: 2 }))
  .on("data", function (row) {
    const kanji = row[2];
    // console.log(kanji);
    searchList.forEach((entry) => {
      if (entry.k === kanji) {
        console.log(`found ${kanji}`);
        radicallist.push({
          // Number,Strokes,Radical,Alternate,Category,Meaning,Reading-J,Reading-R,Position-J,Position-R,Importance,Frequency,Examples
          n: row[0],
          r: row[2],
          s: row[1],
          a: row[3],
          m: row[5],
          rj: row[6],
          rr: row[7],
          i: row[10] == "Important" ? 1 : 0,
          f: row[11],
        });
        count++;
      }
    });
  })
  .on("end", () => {
    console.log(count);

    fs.writeFileSync(
      path.join(__dirname, "radicallist.json"),
      JSON.stringify(radicallist),
      "utf-8"
    );
  });
