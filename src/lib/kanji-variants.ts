export const aliasToCanonical = {
  "冷": "冷",
  "令": "令",
  "嶺": "嶺",
  "玲": "玲",
  "羚": "羚",
  "聆": "聆",
  "鈴": "鈴",
  "零": "零",
  "領": "領",
} as const;

type AliasKanji = keyof typeof aliasToCanonical;

const hasOwn = (value: string) =>
  Object.prototype.hasOwnProperty.call(aliasToCanonical, value);

export const aliasIds = Object.keys(aliasToCanonical) as AliasKanji[];

export const canonicalToAliases = aliasIds.reduce(
  (acc, alias) => {
    const canonical = aliasToCanonical[alias];
    acc[canonical] ??= [];
    acc[canonical].push(alias);
    return acc;
  },
  {} as Record<string, string[]>
);

export const resolveKanjiId = (id: string) =>
  hasOwn(id) ? aliasToCanonical[id as AliasKanji] : id;

export const isAliasKanji = (id: string) => hasOwn(id);

export const getCanonicalAliases = (id: string) =>
  canonicalToAliases[resolveKanjiId(id)] ?? [];

export const getKanjiVariants = (id: string) => {
  const canonicalId = resolveKanjiId(id);

  return {
    aliases: getCanonicalAliases(canonicalId),
  };
};

export const canonicalizeKanjiIds = (ids: Iterable<string>) =>
  Array.from(new Set(Array.from(ids, (id) => resolveKanjiId(id))));
