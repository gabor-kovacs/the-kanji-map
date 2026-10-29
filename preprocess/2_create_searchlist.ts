// * Create array to store every possible kanji
import { joyoList } from "./joyo";
import { jinmeiyoList } from "./jinmeiyo";
import composition from "../data/composition.json";
import {
  canonicalizeKanjiIds,
  resolveKanjiId,
} from "../src/lib/kanji-variants";
import * as fs from "fs";
import * as path from "path";
import { createRequire } from "module";

const require = createRequire(import.meta.url);
const JishoAPI = require("unofficial-jisho-api");

const jisho = new JishoAPI();

const REQUEST_TIMEOUT_MS = 15000;
const FINAL_RETRY_REQUEST_TIMEOUT_MS = 30000;
const LAST_CHANCE_REQUEST_TIMEOUT_MS = 45000;
const DEFAULT_MAX_RETRIES = 6;
const DEFAULT_BASE_DELAY_MS = 1000;
const RETRY_PASS_COOLDOWN_MS = 3000;

interface KanjiInfo {
  k: string; // Kanji character
  r: string; // Reading (Kunyomi)
  m: string; // Meaning
  g: number; // Group
  j: "N5" | "N4" | "N3" | "N2" | "N1" | null; // JLPT level
  s: number | null; // Stroke count
}

interface FailedKanji {
  id: string;
  reason: string;
}

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

type UnsupportedSearchFallback = {
  kunyomi: string[];
  meaning: string;
  jlptLevel: KanjiInfo["j"];
  strokeCount: number | null;
};

const UNSUPPORTED_JISHO_SEARCH_FALLBACKS: Record<
  string,
  UnsupportedSearchFallback
> = {
  业: {
    kunyomi: ["わざ"],
    meaning: "business, vocation, arts, performance",
    jlptLevel: "N4",
    strokeCount: 13,
  },
  亚: {
    kunyomi: ["つ.ぐ"],
    meaning: "Asia, rank next, come after, -ous",
    jlptLevel: "N1",
    strokeCount: 7,
  },
};

const VALID_JLPT_LEVELS = new Set(["N5", "N4", "N3", "N2", "N1"]);
const isSearchableEntry = (id: string): boolean => {
  const canonicalId = resolveKanjiId(id);
  return canonicalId !== "default" && !canonicalId.startsWith("CDP-");
};

const getErrorMessage = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

const getUnsupportedSearchFallback = (id: string) =>
  UNSUPPORTED_JISHO_SEARCH_FALLBACKS[id];

// Helper function to sleep
const sleep = (ms: number) => new Promise((resolve) => setTimeout(resolve, ms));

const withTimeout = async <T>(
  promise: Promise<T>,
  ms: number,
  context: string,
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

// Retry logic with exponential backoff
const getKanjiInfoWithRetry = async (
  id: string,
  {
    maxRetries = DEFAULT_MAX_RETRIES,
    baseDelay = DEFAULT_BASE_DELAY_MS,
    requestTimeoutMs = REQUEST_TIMEOUT_MS,
  }: RetryOptions = {},
): Promise<any> => {
  const attemptFetch = async (attempt: number): Promise<any> => {
    try {
      const jishoData = await withTimeout(
        jisho.searchForKanji(id),
        requestTimeoutMs,
        `Jisho search for ${id}`,
      );
      console.log(`✓ Data found for ${id}`);
      return jishoData;
    } catch (error: any) {
      const status = error?.response?.status || error?.statusCode;

      // Handle 404 - kanji doesn't exist in Jisho
      if (status === 404) {
        console.log(`⚠ 404 Not Found for ${id} - kanji doesn't exist in Jisho`);
        throw new Error(`Jisho does not serve kanji ${id}`);
      }

      // Retry rate limits, server errors, and transient network/timeout failures.
      if (isRetryableError(error)) {
        if (attempt < maxRetries) {
          const delay = baseDelay * Math.pow(2, attempt);
          console.log(
            `⏳ Retryable failure for ${id}. Retrying in ${delay}ms... (attempt ${
              attempt + 1
            }/${maxRetries})`,
          );
          await sleep(delay);
          return attemptFetch(attempt + 1);
        }
      }

      throw new Error(
        `Failed to fetch data for ${id} after ${attempt + 1} attempts: ${getErrorMessage(
          error,
        )}`,
      );
    }

    throw new Error(
      `Failed to fetch data for ${id}: retry loop exited unexpectedly`,
    );
  };

  return attemptFetch(0);
};

const getGroup = (id: string): number => {
  const canonicalId = resolveKanjiId(id);
  if (joyoList.includes(canonicalId)) return 1;
  if (jinmeiyoList.includes(canonicalId)) return 2;
  return 3;
};

// Process kanji in batches for better performance
const processBatch = async (
  entries: [string, any][],
  batchSize = 5,
): Promise<{ results: KanjiInfo[]; failures: FailedKanji[] }> => {
  const results: KanjiInfo[] = [];
  const failures: FailedKanji[] = [];
  const totalBatches = Math.ceil(entries.length / batchSize);

  const processBatchRange = async (i: number): Promise<void> => {
    if (i >= entries.length) {
      return;
    }

    const batch = entries.slice(i, i + batchSize);
    const batchNum = Math.floor(i / batchSize) + 1;
    console.log(
      `\n📦 Processing batch ${batchNum}/${totalBatches} (${i + 1}-${Math.min(
        i + batchSize,
        entries.length,
      )}/${entries.length})`,
    );

    const batchResults = await Promise.all(
      batch.map(async ([kanji, _]) => {
        try {
          const info = await getKanjiInfoWithRetry(kanji);
          const group = getGroup(kanji);
          const jlptLevel =
            typeof info?.jlptLevel === "string" &&
            VALID_JLPT_LEVELS.has(info.jlptLevel)
              ? info.jlptLevel
              : null;
          const strokeCount =
            typeof info?.strokeCount === "number" ? info.strokeCount : null;

          return {
            k: kanji,
            r: info?.kunyomi ? info.kunyomi.join(", ") : "",
            m: info?.meaning ?? "",
            g: group,
            j: jlptLevel,
            s: strokeCount,
          };
        } catch (error) {
          const unsupportedFallback = getUnsupportedSearchFallback(kanji);
          if (unsupportedFallback) {
            console.log(
              `⚠ Using fallback search metadata for ${kanji} because Jisho does not serve this character`,
            );
            return {
              k: kanji,
              r: unsupportedFallback.kunyomi.join(", "),
              m: unsupportedFallback.meaning,
              g: getGroup(kanji),
              j: unsupportedFallback.jlptLevel,
              s: unsupportedFallback.strokeCount,
            };
          }

          const reason = getErrorMessage(error);
          failures.push({ id: kanji, reason });
          console.error(`✗ Failed for ${kanji}: ${reason}`);
          return null;
        }
      }),
    );

    results.push(
      ...batchResults.filter((entry): entry is KanjiInfo => entry !== null),
    );

    if (failures.length > 0) {
      console.log(
        `⚠ Batch ${batchNum} completed with ${failures.length} total failure(s) so far`,
      );
    }

    // Small delay between batches to be respectful to the API
    if (i + batchSize < entries.length) {
      await sleep(200);
    }

    await processBatchRange(i + batchSize);
  };

  await processBatchRange(0);

  return { results, failures };
};

type RetryPassResult = {
  recovered: KanjiInfo[];
  nextFailures: FailedKanji[];
};

const runRetryPass = async (
  remainingFailures: FailedKanji[],
  retryPass: RetryPass,
  getGroupForKanji: (id: string) => number,
): Promise<RetryPassResult> => {
  const retryResults = await Promise.all(
    remainingFailures.map(async ({ id }) => {
      try {
        const info = await getKanjiInfoWithRetry(id, retryPass);
        const jlptLevel =
          typeof info?.jlptLevel === "string" &&
          VALID_JLPT_LEVELS.has(info.jlptLevel)
            ? info.jlptLevel
            : null;
        const strokeCount =
          typeof info?.strokeCount === "number" ? info.strokeCount : null;

        return {
          recovered: {
            k: id,
            r: info?.kunyomi ? info.kunyomi.join(", ") : "",
            m: info?.meaning ?? "",
            g: getGroupForKanji(id),
            j: jlptLevel,
            s: strokeCount,
          } satisfies KanjiInfo,
        };
      } catch (error) {
        const unsupportedFallback = getUnsupportedSearchFallback(id);
        if (unsupportedFallback) {
          console.log(
            `⚠ Using fallback search metadata for ${id} because Jisho does not serve this character`,
          );
          return {
            recovered: {
              k: id,
              r: unsupportedFallback.kunyomi.join(", "),
              m: unsupportedFallback.meaning,
              g: getGroupForKanji(id),
              j: unsupportedFallback.jlptLevel,
              s: unsupportedFallback.strokeCount,
            } satisfies KanjiInfo,
          };
        }

        return {
          failure: { id, reason: getErrorMessage(error) } satisfies FailedKanji,
        };
      }
    }),
  );

  return {
    recovered: retryResults.flatMap((result) =>
      result.recovered ? [result.recovered] : [],
    ),
    nextFailures: retryResults.flatMap((result) =>
      result.failure ? [result.failure] : [],
    ),
  };
};

const retryFailedSearchEntries = async (
  failures: FailedKanji[],
  getGroupForKanji: (id: string) => number,
): Promise<{ recovered: KanjiInfo[]; remainingFailures: FailedKanji[] }> => {
  const retryPasses = async (
    passIndex: number,
    remainingFailures: FailedKanji[],
    recovered: KanjiInfo[],
  ): Promise<{ recovered: KanjiInfo[]; remainingFailures: FailedKanji[] }> => {
    if (
      remainingFailures.length === 0 ||
      passIndex >= FINAL_RETRY_PASSES.length
    ) {
      return { recovered, remainingFailures };
    }

    const retryPass = FINAL_RETRY_PASSES[passIndex];
    console.log(
      `\n🔁 Retry pass ${passIndex + 1}/${FINAL_RETRY_PASSES.length} for ${
        remainingFailures.length
      } kanji (${retryPass.label})...`,
    );

    const { recovered: recoveredOnPass, nextFailures } = await runRetryPass(
      remainingFailures,
      retryPass,
      getGroupForKanji,
    );

    console.log(
      `Recovered ${remainingFailures.length - nextFailures.length}/${
        remainingFailures.length
      } failed kanji on retry pass ${passIndex + 1}`,
    );

    if (nextFailures.length > 0 && passIndex < FINAL_RETRY_PASSES.length - 1) {
      console.log(
        `⏸ Cooling down for ${RETRY_PASS_COOLDOWN_MS}ms before the next retry pass...`,
      );
      await sleep(RETRY_PASS_COOLDOWN_MS);
    }

    return retryPasses(passIndex + 1, nextFailures, [
      ...recovered,
      ...recoveredOnPass,
    ]);
  };

  return retryPasses(0, failures, []);
};

const main = async (): Promise<void> => {
  const entries = canonicalizeKanjiIds(Object.keys(composition)).reduce<
    [string, unknown][]
  >((allEntries, id) => {
    if (isSearchableEntry(id)) {
      allEntries.push([id, composition[id as keyof typeof composition]]);
    }

    return allEntries;
  }, []);
  const failedPath = path.join(
    path.dirname(__dirname),
    "data",
    "failed-searchlist-kanji.json",
  );
  console.log(`🚀 Starting to process ${entries.length} kanji...`);
  console.log(`Using batch size: 20 (processing 20 kanji in parallel)`);

  const { results: initialResults, failures } = await processBatch(entries, 20);
  const uniqueFailures = Array.from(
    new Map(failures.map((failure) => [failure.id, failure])).values(),
  );
  const { recovered, remainingFailures } = await retryFailedSearchEntries(
    uniqueFailures,
    getGroup,
  );
  const searchListById = new Map(
    [...initialResults, ...recovered].map((entry) => [entry.k, entry]),
  );
  const searchList = entries.reduce<KanjiInfo[]>((resolvedEntries, [id]) => {
    const entry = searchListById.get(id);

    if (entry) {
      resolvedEntries.push(entry);
    }

    return resolvedEntries;
  }, []);

  if (remainingFailures.length > 0) {
    fs.writeFileSync(
      failedPath,
      JSON.stringify(remainingFailures, null, 2),
      "utf-8",
    );
    throw new Error(
      `Completed with ${remainingFailures.length} failed kanji. See data/failed-searchlist-kanji.json for details.`,
    );
  }

  if (fs.existsSync(failedPath)) {
    fs.rmSync(failedPath, { force: true });
  }

  console.log(`\n💾 Writing results to searchlist.json...`);
  fs.writeFileSync(
    path.join(path.dirname(__dirname), "data", "searchlist.json"),
    JSON.stringify(searchList, null, 2),
    "utf-8",
  );

  console.log(`✅ Done! Processed ${searchList.length} kanji.`);
};

main().catch((error) => {
  const message = error instanceof Error ? error.message : String(error);
  console.error(`\n❌ Process terminated: ${message}`);
  process.exit(1);
});
