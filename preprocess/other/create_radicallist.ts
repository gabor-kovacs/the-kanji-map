// * Create array to store radicals
import fs from "fs";
import path from "path";
import { parse } from "csv-parse";
import searchList from "../../data/searchlist.json";

// Define the structure of each radical entry
interface RadicalEntry {
  n: string; // Number
  r: string; // Radical (kanji)
  s: string; // Strokes
  a: string; // Alternate Radical
  m: string; // Meaning
  rj: string; // Reading-J (Japanese reading)
  rr: string; // Reading-R (Romanized reading)
  i: number; // Importance (1 if Important, otherwise 0)
  f: string; // Frequency
}

let count = 0;
const radicallist: RadicalEntry[] = [];

fs.createReadStream("./kanji-radicals.csv")
  .pipe(parse({ delimiter: ",", from_line: 2 }))
  .on("data", function (row: string[]) {
    const kanji = row[2];

    searchList.forEach((entry: { k: string }) => {
      if (entry.k === kanji) {
        console.log(`found ${kanji}`);
        radicallist.push({
          n: row[0], // Number
          r: row[2], // Radical (kanji)
          s: row[1], // Strokes
          a: row[3], // Alternate Radical
          m: row[5], // Meaning
          rj: row[6], // Reading-J
          rr: row[7], // Reading-R
          i: row[10] === "Important" ? 1 : 0, // Importance (1 if 'Important', else 0)
          f: row[11], // Frequency
        });
        count++;
      }
    });
  })
  .on("end", () => {
    console.log(count);
    fs.writeFileSync(
      path.join(__dirname, "radicallist.json"),
      JSON.stringify(radicallist, null, 2), // Added `null, 2` for pretty-printing
      "utf-8"
    );
  });
