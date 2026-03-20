// * pull the kanji data from Jisho.org and KanjiVG
// Next.js can be used directly to fetch this data but storing locally can speed up build times later on
import fs from "fs";
import path from "path";
import searchlist from "../data/searchlist.json";
import {
  canonicalizeKanjiIds,
  resolveKanjiId,
} from "../src/lib/kanji-variants";
import * as dotenv from "dotenv";
import { createRequire } from "module";

dotenv.config({ path: path.join(__dirname, "..", ".env") });

const require = createRequire(import.meta.url);
const JishoAPI = require("unofficial-jisho-api");
const jisho = new JishoAPI();
const REQUEST_TIMEOUT_MS = 15000;
const FINAL_RETRY_REQUEST_TIMEOUT_MS = 30000;
const LAST_CHANCE_REQUEST_TIMEOUT_MS = 45000;
const DEFAULT_MAX_RETRIES = 6;
const DEFAULT_BASE_DELAY_MS = 1000;
const RETRY_PASS_COOLDOWN_MS = 3000;

type RetryOptions = {
  maxRetries?: number;
  baseDelay?: number;
  requestTimeoutMs?: number;
};

type RetryPass = RetryOptions & {
  label: string;
};

const FINAL_RETRY_PASSES: RetryPass[] = [
  {
    label: "longer timeout",
    maxRetries: 8,
    baseDelay: 1500,
    requestTimeoutMs: FINAL_RETRY_REQUEST_TIMEOUT_MS,
  },
  {
    label: "last-chance timeout",
    maxRetries: 10,
    baseDelay: 2500,
    requestTimeoutMs: LAST_CHANCE_REQUEST_TIMEOUT_MS,
  },
];

// Sleep function to introduce delays between API requests
const sleep = (waitTimeInMs: number): Promise<void> =>
  new Promise((resolve) => setTimeout(resolve, waitTimeInMs));

const withTimeout = async <T>(
  promise: Promise<T>,
  ms: number,
  context: string
): Promise<T> => {
  let timeoutId: ReturnType<typeof setTimeout> | undefined;

  try {
    return await Promise.race([
      promise,
      new Promise<T>((_, reject) => {
        timeoutId = setTimeout(() => {
          reject(new Error(`${context} timed out after ${ms}ms`));
        }, ms);
      }),
    ]);
  } finally {
    if (timeoutId !== undefined) {
      clearTimeout(timeoutId);
    }
  }
};

const isRetryableError = (error: unknown): boolean => {
  const status =
    typeof error === "object" && error !== null
      ? (error as { response?: { status?: number }; statusCode?: number })
          .response?.status ||
        (error as { response?: { status?: number }; statusCode?: number })
          .statusCode
      : undefined;

  if (status === 429 || (typeof status === "number" && status >= 500)) {
    return true;
  }

  const code =
    typeof error === "object" && error !== null
      ? (error as { code?: string }).code
      : undefined;
  const message =
    typeof error === "object" && error !== null
      ? String((error as { message?: unknown }).message ?? "")
      : String(error ?? "");

  const retryableCodes = new Set([
    "ECONNABORTED",
    "ECONNRESET",
    "ENOTFOUND",
    "EAI_AGAIN",
    "ETIMEDOUT",
    "UND_ERR_CONNECT_TIMEOUT",
    "UND_ERR_HEADERS_TIMEOUT",
  ]);

  return (
    (typeof code === "string" && retryableCodes.has(code)) ||
    message.toLowerCase().includes("timeout") ||
    message.toLowerCase().includes("timed out")
  );
};

const getErrorMessage = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

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

type UnsupportedJishoPageFallback = {
  meaning: string;
  kunyomi: string[];
  onyomi: string[];
  jlptLevel: string | null;
  strokeCount: number | null;
  taughtIn?: string;
};

const UNSUPPORTED_JISHO_PAGE_FALLBACKS: Record<
  string,
  UnsupportedJishoPageFallback
> = {
  业: {
    meaning: "business, vocation, arts, performance",
    kunyomi: ["わざ"],
    onyomi: ["ギョウ", "ゴウ"],
    jlptLevel: "N4",
    strokeCount: 13,
    taughtIn: "grade 3",
  },
  亚: {
    meaning: "Asia, rank next, come after, -ous",
    kunyomi: ["つ.ぐ"],
    onyomi: ["ア"],
    jlptLevel: "N1",
    strokeCount: 7,
    taughtIn: "junior high",
  },
};

const buildUnsupportedJishoPageData = (id: string) => {
  const fallback = UNSUPPORTED_JISHO_PAGE_FALLBACKS[id];
  if (!fallback) {
    return null;
  }

  return {
    query: id,
    found: true,
    meaning: fallback.meaning,
    kunyomi: fallback.kunyomi,
    onyomi: fallback.onyomi,
    jlptLevel: fallback.jlptLevel,
    strokeCount: fallback.strokeCount,
    taughtIn: fallback.taughtIn,
    onyomiExamples: [],
    kunyomiExamples: [],
  };
};

const removeStaleKanjiJsonFiles = (
  dataDir: string,
  kanjiList: string[]
): number => {
  const expectedKanjiIds = new Set(canonicalizeKanjiIds(kanjiList));
  let removedCount = 0;

  for (const entry of fs.readdirSync(dataDir, { withFileTypes: true })) {
    if (!entry.isFile() || path.extname(entry.name) !== ".json") {
      continue;
    }

    const kanjiId = path.basename(entry.name, ".json");
    if (!expectedKanjiIds.has(kanjiId)) {
      fs.rmSync(path.join(dataDir, entry.name), { force: true });
      removedCount += 1;
    }
  }

  return removedCount;
};

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
  {
    maxRetries = DEFAULT_MAX_RETRIES,
    baseDelay = DEFAULT_BASE_DELAY_MS,
    requestTimeoutMs = REQUEST_TIMEOUT_MS,
  }: RetryOptions = {}
): Promise<any> => {
  for (let attempt = 0; attempt <= maxRetries; attempt++) {
    try {
      const jishoData = await withTimeout(
        jisho.searchForKanji(id),
        requestTimeoutMs,
        `Jisho search for ${id}`
      );
      return jishoData;
    } catch (error: any) {
      const status = error?.response?.status || error?.statusCode;

      // Handle 404 - kanji doesn't exist in Jisho
      if (status === 404) {
        console.log(`⚠ 404 Not Found for ${id}`);
        throw new Error(`Jisho does not serve kanji ${id}`);
      }

      // Retry rate limits, server errors, and transient network/timeout failures.
      if (isRetryableError(error)) {
        if (attempt < maxRetries) {
          const delay = baseDelay * Math.pow(2, attempt);
          console.log(
            `⏳ Retryable failure for ${id}. Retrying in ${delay}ms... (attempt ${
              attempt + 1
            }/${maxRetries})`
          );
          await sleep(delay);
          continue;
        }
      }

      throw new Error(
        `Failed to fetch Jisho data for ${id} after ${attempt + 1} attempts: ${getErrorMessage(
          error
        )}`
      );
    }
  }

  throw new Error(
    `Failed to fetch Jisho data for ${id}: retry loop exited unexpectedly`
  );
};

const fetchJishoDataOrFallback = async (
  canonicalKanji: string,
  retryOptions?: RetryOptions
): Promise<any | null> => {
  try {
    return await getJishoDataWithRetry(canonicalKanji, retryOptions);
  } catch (error) {
    const fallbackData = buildUnsupportedJishoPageData(canonicalKanji);
    if (!fallbackData) {
      throw error;
    }

    console.log(
      `⚠ Using fallback page data for ${canonicalKanji} because Jisho does not serve this character`
    );
    return fallbackData;
  }
};

const writeKanjiData = async (
  kanji: string,
  kanjialiveMap: Map<string, any>,
  dataDir: string,
  retryOptions?: RetryOptions
): Promise<FailedKanji | null> => {
  try {
    const canonicalKanji = resolveKanjiId(kanji);
    const jishoData = await fetchJishoDataOrFallback(
      canonicalKanji,
      retryOptions
    );
    const kanjialiveData = kanjialiveMap.get(canonicalKanji) || null;

    const result: KanjiData = {
      id: canonicalKanji,
      kanjialiveData,
      jishoData,
    };

    fs.writeFileSync(
      path.join(dataDir, `${canonicalKanji}.json`),
      JSON.stringify(result, null, 2),
      "utf-8"
    );

    console.log(`✓ Written data for ${canonicalKanji}`);
    return null;
  } catch (error) {
    const reason = getErrorMessage(error);
    console.error(`✗ Failed for ${kanji}: ${reason}`);
    return { id: kanji, reason };
  }
};

const retryFailedKanjiWrites = async (
  failedKanji: FailedKanji[],
  kanjialiveMap: Map<string, any>,
  dataDir: string
): Promise<FailedKanji[]> => {
  let remainingFailures = failedKanji;

  for (let i = 0; i < FINAL_RETRY_PASSES.length; i++) {
    if (remainingFailures.length === 0) {
      break;
    }

    const retryPass = FINAL_RETRY_PASSES[i];
    console.log(
      `\n🔁 Retry pass ${i + 1}/${FINAL_RETRY_PASSES.length} for ${
        remainingFailures.length
      } kanji (${retryPass.label})...`
    );

    const nextFailures: FailedKanji[] = [];
    for (const { id } of remainingFailures) {
      const failure = await writeKanjiData(id, kanjialiveMap, dataDir, retryPass);
      if (failure) {
        nextFailures.push(failure);
      }
    }

    console.log(
      `Recovered ${remainingFailures.length - nextFailures.length}/${
        remainingFailures.length
      } failed kanji on retry pass ${i + 1}`
    );
    remainingFailures = nextFailures;

    if (remainingFailures.length > 0 && i < FINAL_RETRY_PASSES.length - 1) {
      console.log(
        `⏸ Cooling down for ${RETRY_PASS_COOLDOWN_MS}ms before the next retry pass...`
      );
      await sleep(RETRY_PASS_COOLDOWN_MS);
    }
  }

  return remainingFailures;
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

  const removedCount = removeStaleKanjiJsonFiles(dataDir, kanjiList);
  console.log(`🧹 Removed ${removedCount} stale kanji JSON file(s)`);

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
        const failure = await writeKanjiData(kanji, kanjialiveMap, dataDir);
        if (failure) {
          failures.push(failure);
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

  if (failures.length === 0) {
    return [];
  }

  const uniqueFailures = Array.from(
    new Map(failures.map((failure) => [failure.id, failure])).values()
  );

  return retryFailedKanjiWrites(uniqueFailures, kanjialiveMap, dataDir);
};

// Extract the kanji list from searchlist data
const kanjilist: string[] = canonicalizeKanjiIds(
  searchlist.map((el: { k: string }) => el.k)
);

// Main function to process the kanji list and save data to files
const main = async (): Promise<void> => {
  const failedPath = path.join(__dirname, "..", "data", "failed-kanji.json");
  console.log(`🎯 Total kanji to process: ${kanjilist.length}`);

  // Step 1: Fetch all Kanjialive data at once
  const kanjialiveMap = await fetchAllKanjialiveData();

  // Step 2: Process Jisho data in batches
  console.log("\n🚀 Starting batch processing with Jisho API...");
  console.log("Using batch size: 15 (processing 15 kanji in parallel)\n");
  const failures = await processBatch(kanjilist, kanjialiveMap, 15);
  if (failures.length > 0) {
    fs.writeFileSync(failedPath, JSON.stringify(failures, null, 2), "utf-8");
    throw new Error(
      `Completed with ${failures.length} failed kanji. See data/failed-kanji.json for details.`
    );
  }

  if (fs.existsSync(failedPath)) {
    fs.rmSync(failedPath, { force: true });
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
