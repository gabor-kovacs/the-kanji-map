// * pull the kanji data from Jisho.org and KanjiVG
// Next.js can be used directly to fetch this data but storing locally can speed up build times later on
import fs from "fs";
import path from "path";
import searchlist from "../data/searchlist.json";
import JishoAPI from "unofficial-jisho-api";
import * as dotenv from "dotenv";

dotenv.config();

const jisho = new JishoAPI();

// Sleep function to introduce delays between API requests
const sleep = (waitTimeInMs: number): Promise<void> =>
  new Promise((resolve) => setTimeout(resolve, waitTimeInMs));

// Interface for the Kanji data response
interface KanjiData {
  id: string;
  kanjialiveData: any | null; // Type these as needed based on actual API response structure
  jishoData: any | null; // Type these as needed based on actual API response structure
}

// Function to fetch Kanji data from Kanjialive and Jisho APIs
const getKanjiData = async (id: string): Promise<KanjiData> => {
  // Fetching kanji data from Kanjialive API using fetch
  let kanjialiveData: any | null = null;
  const kanjialiveUrl = `https://kanjialive-api.p.rapidapi.com/api/public/kanji/${encodeURIComponent(
    id
  )}`;
  const options = {
    method: "GET",
    headers: {
      "x-rapidapi-key": `${process.env.KANJIALIVE_API_KEY}`,
      "x-rapidapi-host": "kanjialive-api.p.rapidapi.com",
    },
  };

  try {
    const res = await fetch(kanjialiveUrl, options);
    if (!res.ok) {
      throw new Error(`HTTP error! Status: ${res.status}`);
    }
    kanjialiveData = await res.json();
    console.log(`kanjialiveData found for ${id}`);
  } catch (error) {
    console.error(error);
    console.log("No Kanjialive data found");
  }

  // Fetching kanji data from Jisho API
  let jishoData: any | null = null;
  try {
    jishoData = await jisho.searchForKanji(id);
    console.log(`jishoData found for ${id}`);
  } catch (error) {
    console.error(error);
    console.log("No Jisho data found");
  }

  return {
    id,
    kanjialiveData,
    jishoData,
  };
};

// Extract the kanji list from searchlist data
const kanjilist: string[] = searchlist.map((el: { k: string }) => el.k);

// Main function to process the kanji list and save data to files
const main = async (): Promise<void> => {
  for (let i = 0; i < kanjilist.length; i++) {
    console.log(`${i + 1}/${kanjilist.length}`);
    const kanji = kanjilist[i];
    await sleep(1000); // Sleep to avoid exceeding rate limits
    const result = await getKanjiData(kanji);

    // Write the data to JSON file
    fs.writeFileSync(
      path.join(__dirname, "..", "data", "kanji", `${kanji}.json`),
      JSON.stringify(result, null, 2),
      "utf-8"
    );
  }
};

// Execute the main function
main().then(() => {
  console.log("done");
});
