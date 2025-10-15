// * Create array to store every possible kanji
import { joyoList } from "./joyo";
import { jinmeiyoList } from "./jinmeiyo";
import * as composition from "../data/composition.json";
import JishoAPI from "unofficial-jisho-api";
import * as fs from "fs";
import * as path from "path";

const jisho = new JishoAPI();

interface KanjiInfo {
  k: string; // Kanji character
  r: string; // Reading (Kunyomi)
  m: string; // Meaning
  g: number; // Group
}

// Helper function to sleep
const sleep = (ms: number) => new Promise((resolve) => setTimeout(resolve, ms));

// Retry logic with exponential backoff
const getKanjiInfoWithRetry = async (
  id: string,
  maxRetries = 5,
  baseDelay = 1000
): Promise<any> => {
  for (let attempt = 0; attempt <= maxRetries; attempt++) {
    try {
      const jishoData = await jisho.searchForKanji(id);
      console.log(`✓ Data found for ${id}`);
      return jishoData;
    } catch (error: any) {
      const status = error?.response?.status || error?.statusCode;

      // Handle 404 - kanji doesn't exist in Jisho
      if (status === 404) {
        console.log(`⚠ 404 Not Found for ${id} - kanji doesn't exist in Jisho`);
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
        `✗ Failed to fetch data for ${id} after ${attempt + 1} attempts:`,
        error?.message || error
      );
      return null;
    }
  }
  return null;
};

const getGroup = (id: string): number => {
  if (joyoList.includes(id)) return 1;
  if (jinmeiyoList.includes(id)) return 2;
  return 3;
};

// Process kanji in batches for better performance
const processBatch = async (
  entries: [string, any][],
  batchSize = 5
): Promise<KanjiInfo[]> => {
  const results: KanjiInfo[] = [];
  const totalBatches = Math.ceil(entries.length / batchSize);

  for (let i = 0; i < entries.length; i += batchSize) {
    const batch = entries.slice(i, i + batchSize);
    const batchNum = Math.floor(i / batchSize) + 1;
    console.log(
      `\n📦 Processing batch ${batchNum}/${totalBatches} (${i + 1}-${Math.min(
        i + batchSize,
        entries.length
      )}/${entries.length})`
    );

    const batchResults = await Promise.all(
      batch.map(async ([kanji, _]) => {
        const info = await getKanjiInfoWithRetry(kanji);
        const group = getGroup(kanji);
        return {
          k: kanji,
          r: info?.kunyomi ? info.kunyomi.join(", ") : "",
          m: info?.meaning ?? "",
          g: group,
        };
      })
    );

    results.push(...batchResults);

    // Small delay between batches to be respectful to the API
    if (i + batchSize < entries.length) {
      await sleep(200);
    }
  }

  return results;
};

(async () => {
  const entries = Object.entries(composition);
  console.log(`🚀 Starting to process ${entries.length} kanji...`);
  console.log(`Using batch size: 20 (processing 20 kanji in parallel)`);

  const searchList = await processBatch(entries, 20);

  console.log(`\n💾 Writing results to searchlist.json...`);
  fs.writeFileSync(
    path.join(path.dirname(__dirname), "data", "searchlist.json"),
    JSON.stringify(searchList, null, 2),
    "utf-8"
  );

  console.log(`✅ Done! Processed ${searchList.length} kanji.`);
})();
