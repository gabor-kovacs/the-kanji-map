// * pull the kanji data from Jisho.org and KanjiVG
// Next.js can be used directly to fetch this data but storing locally can speed up build times later on
import fs from "fs";
import path from "path";
import searchlist from "../data/searchlist.json";
import * as dotenv from "dotenv";
import { createRequire } from "module";

dotenv.config();

const require = createRequire(import.meta.url);
const JishoAPI = require("unofficial-jisho-api");
const jisho = new JishoAPI();

// Sleep function to introduce delays between API requests
const sleep = (waitTimeInMs: number): Promise<void> =>
  new Promise((resolve) => setTimeout(resolve, waitTimeInMs));

const truncate = (value: string, maxLength = 200): string =>
  value.length > maxLength ? `${value.slice(0, maxLength)}...` : value;

// Interface for the Kanji data response
interface KanjiData {
  id: string;
  kanjialiveData: any | null;
  jishoData: any | null;
}

interface FailedKanji {
  id: string;
  reason: string;
}

// Fetch all Kanjialive data in one request
const fetchAllKanjialiveData = async (): Promise<Map<string, any>> => {
  console.log("🚀 Fetching all Kanjialive data (6MB)...");
  const rapidApiKey = process.env.KANJIALIVE_API_KEY?.trim();
  if (!rapidApiKey) {
    throw new Error(
      "KANJIALIVE_API_KEY is missing. Aborting because Kanjialive is required."
    );
  }

  const kanjialiveUrl =
    "https://kanjialive-api.p.rapidapi.com/api/public/kanji/all";
  const options = {
    method: "GET",
    headers: {
      "x-rapidapi-key": rapidApiKey,
      "x-rapidapi-host": "kanjialive-api.p.rapidapi.com",
      accept: "application/json",
      "user-agent": "the-kanji-map-preprocess/1.0",
    },
  };

  try {
    const res = await fetch(kanjialiveUrl, options);
    if (!res.ok) {
      const responseText = truncate(await res.text().catch(() => ""));
      if (res.status === 403) {
        throw new Error(
          `Kanjialive responded with HTTP 403. Verify your RapidAPI key, active Kanjialive subscription, and monthly quota.${
            responseText ? ` Response: ${responseText}` : ""
          }`
        );
      }
      throw new Error(
        `Failed to fetch Kanjialive data. HTTP ${res.status} ${res.statusText}${
          responseText ? ` Response: ${responseText}` : ""
        }`
      );
    }
    const data = await res.json();
    console.log(`✓ Fetched Kanjialive data for ${data.length} kanji`);

    // Create a lookup map by kanji character
    const kanjiMap = new Map<string, any>();
    for (const entry of data) {
      if (entry.kanji && entry.kanji.character) {
        kanjiMap.set(entry.kanji.character, entry);
      }
    }
    return kanjiMap;
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    throw new Error(`Failed to fetch Kanjialive data: ${message}`);
  }
};

// Retry logic with exponential backoff for Jisho API
const getJishoDataWithRetry = async (
  id: string,
  maxRetries = 5,
  baseDelay = 1000
): Promise<any> => {
  for (let attempt = 0; attempt <= maxRetries; attempt++) {
    try {
      const jishoData = await jisho.searchForKanji(id);
      return jishoData;
    } catch (error: any) {
      const status = error?.response?.status || error?.statusCode;

      // Handle 404 - kanji doesn't exist in Jisho
      if (status === 404) {
        console.log(`⚠ 404 Not Found for ${id}`);
        return null;
      }

      // Handle rate limiting (429) and server errors (5xx)
      if (status === 429 || (status >= 500 && status < 600)) {
        if (attempt < maxRetries) {
          const delay = baseDelay * Math.pow(2, attempt);
          console.log(
            `⏳ Error ${status} for ${id}. Retrying in ${delay}ms... (attempt ${
              attempt + 1
            }/${maxRetries})`
          );
          await sleep(delay);
          continue;
        }
      }

      // For other errors or max retries exceeded
      console.log(
        `✗ Failed to fetch Jisho data for ${id} after ${attempt + 1} attempts`
      );
      return null;
    }
  }
  return null;
};

// Process kanji in batches
const processBatch = async (
  kanjiList: string[],
  kanjialiveMap: Map<string, any>,
  batchSize = 15
): Promise<FailedKanji[]> => {
  const totalBatches = Math.ceil(kanjiList.length / batchSize);
  const dataDir = path.join(__dirname, "..", "data", "kanji");
  const failures: FailedKanji[] = [];

  // Ensure the kanji directory exists
  if (!fs.existsSync(dataDir)) {
    fs.mkdirSync(dataDir, { recursive: true });
  }

  for (let i = 0; i < kanjiList.length; i += batchSize) {
    const batch = kanjiList.slice(i, i + batchSize);
    const batchNum = Math.floor(i / batchSize) + 1;
    console.log(
      `\n📦 Processing batch ${batchNum}/${totalBatches} (${i + 1}-${Math.min(
        i + batchSize,
        kanjiList.length
      )}/${kanjiList.length})`
    );

    await Promise.all(
      batch.map(async (kanji) => {
        try {
          const jishoData = await getJishoDataWithRetry(kanji);
          const kanjialiveData = kanjialiveMap.get(kanji) || null;

          const result: KanjiData = {
            id: kanji,
            kanjialiveData,
            jishoData,
          };

          // Write the data to JSON file
          fs.writeFileSync(
            path.join(dataDir, `${kanji}.json`),
            JSON.stringify(result, null, 2),
            "utf-8"
          );

          console.log(`✓ Written data for ${kanji}`);
        } catch (error) {
          const reason =
            error instanceof Error ? error.message : String(error);
          failures.push({ id: kanji, reason });
          console.error(`✗ Failed for ${kanji}: ${reason}`);
        }
      })
    );

    if (failures.length > 0) {
      console.log(
        `⚠ Batch ${batchNum} completed with ${failures.length} total failure(s) so far`
      );
    }

    // Small delay between batches to be respectful to Jisho API
    if (i + batchSize < kanjiList.length) {
      await sleep(200);
    }
  }

  return failures;
};

// Extract the kanji list from searchlist data
const kanjilist: string[] = searchlist.map((el: { k: string }) => el.k);

// Main function to process the kanji list and save data to files
const main = async (): Promise<void> => {
  console.log(`🎯 Total kanji to process: ${kanjilist.length}`);

  // Step 1: Fetch all Kanjialive data at once
  const kanjialiveMap = await fetchAllKanjialiveData();

  // Step 2: Process Jisho data in batches
  console.log("\n🚀 Starting batch processing with Jisho API...");
  console.log("Using batch size: 15 (processing 15 kanji in parallel)\n");
  const failures = await processBatch(kanjilist, kanjialiveMap, 15);
  if (failures.length > 0) {
    const failedPath = path.join(__dirname, "..", "data", "failed-kanji.json");
    fs.writeFileSync(failedPath, JSON.stringify(failures, null, 2), "utf-8");
    throw new Error(
      `Completed with ${failures.length} failed kanji. See data/failed-kanji.json for details.`
    );
  }

  console.log("\n✅ All done!");
};

// Execute the main function
main()
  .then(() => {
    console.log("Process complete!");
  })
  .catch((error) => {
    const message = error instanceof Error ? error.message : String(error);
    console.error(`\n❌ Process terminated: ${message}`);
    process.exit(1);
  });

process.on("unhandledRejection", (reason) => {
  console.error("\n❌ Unhandled promise rejection:", reason);
  process.exit(1);
});

process.on("uncaughtException", (error) => {
  console.error("\n❌ Uncaught exception:", error);
  process.exit(1);
});
