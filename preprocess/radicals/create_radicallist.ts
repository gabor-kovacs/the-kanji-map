// * Create array to store radicals
import fs from "fs";
import path from "path";
import { parse } from "csv-parse";

// Define the structure of each radical entry
interface RadicalEntry {
  number: string; // Number
  radical: string; // Radical (main kanji or alternate form)
  strokes: string; // Strokes
  original: string; // Original main radical (empty if this is the main radical)
  category: string; // Category (copied from original if present)
  meaning: string; // Meaning (from kanji-radicals.csv)
  readingJapanese: string; // Reading-J (Japanese reading)
  readingRomanized: string; // Reading-R (Romanized reading)
  positionJapanese: string; // Position-J
  positionRomanized: string; // Position-R
  frequency: string; // Frequency
  alternatives: string[]; // Alternatives from CSV Alternate column (unique code points)
}

// Only use kanji-radicals.csv
console.log("Processing kanji-radicals.csv...");
const radicallist: RadicalEntry[] = [];
let count = 0;

// Build a map of alternate -> original from the whole CSV first
const altToOriginal = new Map<string, string>();
const allRows: string[][] = [];

// First, read all rows to construct altToOriginal
fs.createReadStream("./kanji-radicals.csv")
  .pipe(parse({ delimiter: ",", from_line: 2 }))
  .on("data", function (r: string[]) {
    allRows.push(r);
    const original = r[2];
    const altRaw = (r[3] || "").trim();
    if (altRaw) {
      // Remove whitespace and split into Unicode code points (handles concatenated alternates like "亻𠆢" or "")
      const alternates = Array.from(altRaw.replace(/\s+/g, ""));
      alternates.forEach((alt) => {
        if (alt && alt !== original && !altToOriginal.has(alt)) {
          altToOriginal.set(alt, original);
        }
      });
    }
  })
  .on("end", () => {
    // Now iterate once and emit exactly one record per CSV row (<=330)
    allRows.forEach((row) => {
      const number = row[0];
      const radical = row[2];
      const strokes = row[1];
      const meaning = row[5] || ""; // from kanji-radicals.csv
      const readingJapanese = row[6] || "";
      const readingRomanized = row[7] || "";
      const posJ = row[8] || "";
      const posR = row[9] || "";
      const frequency = row[11] || "";
      const altRaw = (row[3] || "").trim();
      const alternatives = altRaw
        ? Array.from(altRaw.replace(/\s+/g, ""))
            .filter((c) => c && c !== radical)
            .filter((c, i, a) => a.indexOf(c) === i)
        : [];

      const original = altToOriginal.get(radical) || "";
      // category: if this radical is an alternate, copy category from its original's row
      let category = row[4] || "";
      if (original) {
        // find original's row to copy its category
        const origRow = allRows.find((r) => r[2] === original);
        if (origRow && origRow[4]) category = origRow[4];
      }

      radicallist.push({
        number,
        radical,
        strokes,
        original,
        category,
        meaning,
        readingJapanese,
        readingRomanized,
        positionJapanese: posJ,
        positionRomanized: posR,
        frequency,
        alternatives,
      });
      count++;
    });

    console.log(`Final count: ${count} radicals included`);
    fs.writeFileSync(
      path.join(__dirname, "../../data/radicallist.json"),
      JSON.stringify(radicallist, null, 2),
      "utf-8"
    );
  });
