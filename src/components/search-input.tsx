"use client";
import { Button } from "@/components/ui/button";
import {
  Combobox,
  ComboboxChip,
  ComboboxChips,
  ComboboxChipsInput,
  ComboboxContent,
  ComboboxEmpty,
  ComboboxItem,
  ComboboxList,
  ComboboxValue,
  useComboboxAnchor,
} from "@/components/ui/combobox";
import {
  Command,
  CommandEmpty,
  CommandInput,
  CommandItem,
  CommandList,
} from "@/components/ui/command";
import {
  Popover,
  PopoverContent,
  PopoverTrigger,
} from "@/components/ui/popover";
import { Slider } from "@/components/ui/slider";
import { ChevronsUpDown, Filter, RotateCcw } from "lucide-react";
import { useRouter } from "next/navigation";
import * as React from "react";
import { Virtuoso } from "react-virtuoso";

import searchlist from "@/../data/searchlist.json";
import { buildKanjiHref, resolveKanjiId } from "@/lib/kanji-variants";

const JLPT_LEVELS = ["N5", "N4", "N3", "N2", "N1"] as const;
type JLPTLevel = (typeof JLPT_LEVELS)[number];
type JlptFilter = JLPTLevel | "unknown";

const COLLECTION_ITEMS = ["joyo", "jinmeiyo", "other"] as const;

const SEARCH_FILTERS_STORAGE_KEY = "kanji-map:search-filters:v1";

const DEFAULT_GROUP_FILTERS = {
  joyo: true,
  jinmeiyo: true,
  other: true,
} as const;

const DEFAULT_JLPT_FILTERS = {
  N5: true,
  N4: true,
  N3: true,
  N2: true,
  N1: true,
  unknown: true,
} as const;

type SearchOption = {
  kanji: string;
  kunyomi: string;
  meaning: string;
  group: "joyo" | "jinmeiyo" | "other";
  jlptLevel: JLPTLevel | null;
  strokeCount: number | null;
  aliases: string[];
  searchTerms: string[];
};

type SearchGroup = SearchOption["group"];
type GroupFilters = Record<SearchGroup, boolean>;
type JLPTFilters = Record<JlptFilter, boolean>;

const GROUP_LABELS: Record<SearchGroup, string> = {
  joyo: "Joyo",
  jinmeiyo: "Jinmeiyo",
  other: "Other",
};

const JLPT_LABELS: Record<JlptFilter, string> = {
  N5: "N5",
  N4: "N4",
  N3: "N3",
  N2: "N2",
  N1: "N1",
  unknown: "Unknown",
};

const JLPT_FILTER_ITEMS = [...JLPT_LEVELS, "unknown"] as const;

type SearchListEntry = {
  k: string;
  r: string;
  m: string;
  g: number;
  j?: string | null;
  s?: number | null;
};

type PersistedFilters = {
  groupFilters: GroupFilters;
  jlptFilters: JLPTFilters;
  strokeRange: [number, number];
};

interface VirtualizedCommandProps {
  height: string;
  options: SearchOption[];
  placeholder: string;
  searchInputRef: React.RefObject<HTMLInputElement | null>;
  onSelectOption?: (option: SearchOption) => void;
}

const VirtualizedScroller = React.forwardRef<
  HTMLDivElement,
  React.ComponentProps<"div">
>(({ className, ...props }, ref) => (
  <div
    ref={ref}
    className={[
      "[scrollbar-width:auto]",
      "[scrollbar-color:hsl(var(--muted-foreground)/0.35)_transparent]",
      "[&::-webkit-scrollbar]:w-2.5",
      "[&::-webkit-scrollbar-track]:bg-transparent",
      "[&::-webkit-scrollbar-button]:hidden",
      "[&::-webkit-scrollbar-button]:h-0", // Forces height to 0
      "[&::-webkit-scrollbar-button]:w-0", // Forces width to 0
      "[&::-webkit-scrollbar-button]:bg-transparent", // Fallback transparency
      "[&::-webkit-scrollbar-thumb]:rounded-full",
      "[&::-webkit-scrollbar-thumb]:bg-muted-foreground/35",
      "[&::-webkit-scrollbar-thumb:hover]:bg-muted-foreground/50",
      className,
    ]
      .filter(Boolean)
      .join(" ")}
    {...props}
  />
));
VirtualizedScroller.displayName = "VirtualizedScroller";

const normalizeJlptLevel = (
  value: string | null | undefined,
): JLPTLevel | null =>
  JLPT_LEVELS.includes(value as JLPTLevel) ? (value as JLPTLevel) : null;

const scoreSearchEntry = (entry: SearchListEntry, canonicalKanji: string) =>
  (entry.k === canonicalKanji ? 100 : 0) +
  (entry.g !== 3 ? 10 : 0) +
  (entry.m ? 5 : 0) +
  (entry.r ? 5 : 0) +
  (entry.j ? 3 : 0) +
  (typeof entry.s === "number" ? 2 : 0);

const OPTIONS: SearchOption[] = (() => {
  const mergedEntries = new Map<
    string,
    { entry: SearchListEntry; aliases: Set<string> }
  >();

  (searchlist as SearchListEntry[]).forEach((entry) => {
    const canonicalKanji = resolveKanjiId(entry.k);
    const existing = mergedEntries.get(canonicalKanji);
    const aliases = existing?.aliases ?? new Set<string>();

    if (entry.k !== canonicalKanji) {
      aliases.add(entry.k);
    }

    if (
      !existing ||
      scoreSearchEntry(entry, canonicalKanji) >
        scoreSearchEntry(existing.entry, canonicalKanji)
    ) {
      mergedEntries.set(canonicalKanji, {
        entry: {
          ...entry,
          k: canonicalKanji,
        },
        aliases,
      });
      return;
    }

    mergedEntries.set(canonicalKanji, {
      entry: existing.entry,
      aliases,
    });
  });

  return Array.from(mergedEntries.values()).map(({ entry, aliases }) => {
    const mergedAliases = Array.from(aliases);

    return {
      kanji: entry.k,
      kunyomi: entry.r,
      meaning: entry.m,
      group: entry.g === 1 ? "joyo" : entry.g === 2 ? "jinmeiyo" : "other",
      jlptLevel: normalizeJlptLevel(entry.j ?? null),
      strokeCount: typeof entry.s === "number" ? entry.s : null,
      aliases: mergedAliases,
      searchTerms: [entry.k, entry.r, entry.m, ...mergedAliases].filter(
        Boolean,
      ),
    };
  });
})();

const getStrokeBounds = (options: SearchOption[]) => {
  let min = Number.POSITIVE_INFINITY;
  let max = Number.NEGATIVE_INFINITY;

  options.forEach((option) => {
    if (typeof option.strokeCount === "number") {
      min = Math.min(min, option.strokeCount);
      max = Math.max(max, option.strokeCount);
    }
  });

  if (!Number.isFinite(min) || !Number.isFinite(max)) {
    return { min: 1, max: 1 };
  }

  return { min, max };
};

const STROKE_BOUNDS = getStrokeBounds(OPTIONS);
const DEFAULT_STROKE_RANGE: [number, number] = [
  STROKE_BOUNDS.min,
  STROKE_BOUNDS.max,
];

const hasAtLeastOneEnabled = (filters: Record<string, boolean>) =>
  Object.values(filters).some(Boolean);

const clamp = (value: number, min: number, max: number) =>
  Math.min(Math.max(value, min), max);

const parsePersistedFilters = (): PersistedFilters | null => {
  try {
    const raw = localStorage.getItem(SEARCH_FILTERS_STORAGE_KEY);
    if (!raw) {
      return null;
    }

    const parsed = JSON.parse(raw) as Partial<PersistedFilters>;

    const groupFilters: GroupFilters = {
      joyo:
        typeof parsed.groupFilters?.joyo === "boolean"
          ? parsed.groupFilters.joyo
          : DEFAULT_GROUP_FILTERS.joyo,
      jinmeiyo:
        typeof parsed.groupFilters?.jinmeiyo === "boolean"
          ? parsed.groupFilters.jinmeiyo
          : DEFAULT_GROUP_FILTERS.jinmeiyo,
      other:
        typeof parsed.groupFilters?.other === "boolean"
          ? parsed.groupFilters.other
          : DEFAULT_GROUP_FILTERS.other,
    };

    const jlptFilters: JLPTFilters = {
      N5:
        typeof parsed.jlptFilters?.N5 === "boolean"
          ? parsed.jlptFilters.N5
          : DEFAULT_JLPT_FILTERS.N5,
      N4:
        typeof parsed.jlptFilters?.N4 === "boolean"
          ? parsed.jlptFilters.N4
          : DEFAULT_JLPT_FILTERS.N4,
      N3:
        typeof parsed.jlptFilters?.N3 === "boolean"
          ? parsed.jlptFilters.N3
          : DEFAULT_JLPT_FILTERS.N3,
      N2:
        typeof parsed.jlptFilters?.N2 === "boolean"
          ? parsed.jlptFilters.N2
          : DEFAULT_JLPT_FILTERS.N2,
      N1:
        typeof parsed.jlptFilters?.N1 === "boolean"
          ? parsed.jlptFilters.N1
          : DEFAULT_JLPT_FILTERS.N1,
      unknown:
        typeof parsed.jlptFilters?.unknown === "boolean"
          ? parsed.jlptFilters.unknown
          : DEFAULT_JLPT_FILTERS.unknown,
    };

    let strokeRange: [number, number] = [...DEFAULT_STROKE_RANGE];
    if (
      Array.isArray(parsed.strokeRange) &&
      parsed.strokeRange.length === 2 &&
      typeof parsed.strokeRange[0] === "number" &&
      typeof parsed.strokeRange[1] === "number"
    ) {
      const a = clamp(
        parsed.strokeRange[0],
        STROKE_BOUNDS.min,
        STROKE_BOUNDS.max,
      );
      const b = clamp(
        parsed.strokeRange[1],
        STROKE_BOUNDS.min,
        STROKE_BOUNDS.max,
      );
      strokeRange = a <= b ? [a, b] : [b, a];
    }

    return {
      groupFilters: hasAtLeastOneEnabled(groupFilters)
        ? groupFilters
        : { ...DEFAULT_GROUP_FILTERS },
      jlptFilters: hasAtLeastOneEnabled(jlptFilters)
        ? jlptFilters
        : { ...DEFAULT_JLPT_FILTERS },
      strokeRange,
    };
  } catch {
    return null;
  }
};

const countActiveFilters = (
  groupFilters: GroupFilters,
  jlptFilters: JLPTFilters,
  strokeRange: [number, number],
) => {
  let count = 0;

  if (!Object.values(groupFilters).every(Boolean)) {
    count += 1;
  }

  if (!Object.values(jlptFilters).every(Boolean)) {
    count += 1;
  }

  if (
    strokeRange[0] !== DEFAULT_STROKE_RANGE[0] ||
    strokeRange[1] !== DEFAULT_STROKE_RANGE[1]
  ) {
    count += 1;
  }

  return count;
};

const VirtualizedCommand = ({
  height,
  options,
  placeholder,
  searchInputRef,
  onSelectOption,
}: VirtualizedCommandProps) => {
  const collectionAnchor = useComboboxAnchor();
  const jlptAnchor = useComboboxAnchor();

  const [search, setSearch] = React.useState("");
  const [groupFilters, setGroupFilters] = React.useState<GroupFilters>({
    ...DEFAULT_GROUP_FILTERS,
  });
  const [jlptFilters, setJlptFilters] = React.useState<JLPTFilters>({
    ...DEFAULT_JLPT_FILTERS,
  });
  const [strokeRange, setStrokeRange] =
    React.useState<[number, number]>(DEFAULT_STROKE_RANGE);
  const [hasLoadedPersistedFilters, setHasLoadedPersistedFilters] =
    React.useState(false);

  React.useEffect(() => {
    const persisted = parsePersistedFilters();
    if (persisted) {
      setGroupFilters(persisted.groupFilters);
      setJlptFilters(persisted.jlptFilters);
      setStrokeRange(persisted.strokeRange);
    }
    setHasLoadedPersistedFilters(true);
  }, []);

  React.useEffect(() => {
    if (!hasLoadedPersistedFilters) {
      return;
    }

    const payload: PersistedFilters = {
      groupFilters,
      jlptFilters,
      strokeRange,
    };

    localStorage.setItem(SEARCH_FILTERS_STORAGE_KEY, JSON.stringify(payload));
  }, [groupFilters, hasLoadedPersistedFilters, jlptFilters, strokeRange]);

  const selectedGroups = React.useMemo(
    () => COLLECTION_ITEMS.filter((item) => groupFilters[item]),
    [groupFilters],
  );

  const selectedJlptLevels = React.useMemo(
    () => JLPT_FILTER_ITEMS.filter((item) => jlptFilters[item]),
    [jlptFilters],
  );

  const filteredOptions = React.useMemo(() => {
    const normalizedSearch = search.toLowerCase();
    const hasStrokeFilter =
      strokeRange[0] !== DEFAULT_STROKE_RANGE[0] ||
      strokeRange[1] !== DEFAULT_STROKE_RANGE[1];

    return options.filter((option) => {
      if (!groupFilters[option.group]) {
        return false;
      }

      const jlptKey = (option.jlptLevel ?? "unknown") as JlptFilter;
      if (!jlptFilters[jlptKey]) {
        return false;
      }

      if (hasStrokeFilter) {
        if (option.strokeCount === null) {
          return false;
        }

        if (
          option.strokeCount < strokeRange[0] ||
          option.strokeCount > strokeRange[1]
        ) {
          return false;
        }
      }

      return option.searchTerms.some((term) =>
        term.toLowerCase().includes(normalizedSearch),
      );
    });
  }, [groupFilters, jlptFilters, options, search, strokeRange]);

  const flattenedOptions = React.useMemo(() => {
    const groups: Record<SearchGroup, SearchOption[]> = {
      joyo: [],
      jinmeiyo: [],
      other: [],
    };

    filteredOptions.forEach((option) => {
      groups[option.group].push(option);
    });

    const flatList: Array<
      | { type: "group"; value: SearchGroup }
      | { type: "item"; value: SearchOption }
    > = [];

    (Object.keys(groups) as SearchGroup[]).forEach((groupName) => {
      if (groups[groupName].length > 0) {
        flatList.push({ type: "group", value: groupName });
        groups[groupName].forEach((option) =>
          flatList.push({ type: "item", value: option }),
        );
      }
    });

    return flatList;
  }, [filteredOptions]);

  const activeFilterCount = React.useMemo(
    () => countActiveFilters(groupFilters, jlptFilters, strokeRange),
    [groupFilters, jlptFilters, strokeRange],
  );

  const updateGroupSelection = (values: string[]) => {
    const nextValues = COLLECTION_ITEMS.filter((item) => values.includes(item));
    if (nextValues.length === 0) {
      return;
    }

    setGroupFilters({
      joyo: nextValues.includes("joyo"),
      jinmeiyo: nextValues.includes("jinmeiyo"),
      other: nextValues.includes("other"),
    });
  };

  const updateJlptSelection = (values: string[]) => {
    const nextValues = JLPT_FILTER_ITEMS.filter((item) =>
      values.includes(item),
    );
    if (nextValues.length === 0) {
      return;
    }

    setJlptFilters({
      N5: nextValues.includes("N5"),
      N4: nextValues.includes("N4"),
      N3: nextValues.includes("N3"),
      N2: nextValues.includes("N2"),
      N1: nextValues.includes("N1"),
      unknown: nextValues.includes("unknown"),
    });
  };

  const resetFilters = () => {
    setGroupFilters({ ...DEFAULT_GROUP_FILTERS });
    setJlptFilters({ ...DEFAULT_JLPT_FILTERS });
    setStrokeRange([...DEFAULT_STROKE_RANGE]);
  };

  const updateStrokeRange = (value: number | readonly number[]) => {
    const values = Array.isArray(value) ? [...value] : [value];
    if (values.length !== 2) {
      return;
    }

    const start = clamp(values[0], STROKE_BOUNDS.min, STROKE_BOUNDS.max);
    const end = clamp(values[1], STROKE_BOUNDS.min, STROKE_BOUNDS.max);
    setStrokeRange(start <= end ? [start, end] : [end, start]);
  };

  return (
    <Command shouldFilter={false}>
      <div className="flex items-center justify-between gap-2 px-2 py-1.5">
        <Popover>
          <PopoverTrigger
            render={
              <Button
                variant="outline"
                size="sm"
                className="h-7 gap-1 text-xs"
              />
            }
          >
            <Filter
              className={
                activeFilterCount > 0 ? "size-3.5 text-primary" : "size-3.5"
              }
            />
            Filters{activeFilterCount > 0 ? ` (${activeFilterCount})` : ""}
          </PopoverTrigger>
          <PopoverContent
            className="w-[202px] max-w-[calc(100vw-2rem)] p-3 mt-2"
            align="start"
          >
            <div className="mb-2 flex items-center justify-between">
              <div className="text-sm font-semibold">Search filters</div>
              <Button
                variant="ghost"
                size="sm"
                className="h-7 px-2 text-xs"
                onClick={resetFilters}
              >
                <RotateCcw className="mr-1 size-3.5" />
                Reset
              </Button>
            </div>

            <div className="space-y-2">
              <div className="space-y-1">
                <div className="text-xs text-muted-foreground">Collection</div>
                <Combobox
                  multiple
                  autoHighlight
                  items={COLLECTION_ITEMS}
                  value={selectedGroups as string[]}
                  onValueChange={updateGroupSelection}
                >
                  <ComboboxChips
                    ref={collectionAnchor}
                    className="min-h-8 rounded-md border px-1.5 py-1"
                  >
                    <ComboboxValue>
                      {(values: string[]) => (
                        <React.Fragment>
                          {values.map((value: string) => (
                            <ComboboxChip key={value}>
                              {GROUP_LABELS[value as SearchGroup]}
                            </ComboboxChip>
                          ))}
                          <ComboboxChipsInput className="h-5 text-xs" />
                        </React.Fragment>
                      )}
                    </ComboboxValue>
                  </ComboboxChips>
                  <ComboboxContent
                    anchor={collectionAnchor}
                    align="start"
                    className="w-[220px]"
                  >
                    <ComboboxEmpty>No items found.</ComboboxEmpty>
                    <ComboboxList>
                      {(item) => (
                        <ComboboxItem key={item} value={item}>
                          {GROUP_LABELS[item as SearchGroup]}
                        </ComboboxItem>
                      )}
                    </ComboboxList>
                  </ComboboxContent>
                </Combobox>
              </div>

              <div className="space-y-1">
                <div className="text-xs text-muted-foreground">JLPT Level</div>
                <Combobox
                  multiple
                  autoHighlight
                  items={JLPT_FILTER_ITEMS}
                  value={selectedJlptLevels as string[]}
                  onValueChange={updateJlptSelection}
                >
                  <ComboboxChips
                    ref={jlptAnchor}
                    className="min-h-8 rounded-md border px-1.5 py-1"
                  >
                    <ComboboxValue>
                      {(values: string[]) => (
                        <React.Fragment>
                          {values.map((value: string) => (
                            <ComboboxChip key={value}>
                              {JLPT_LABELS[value as JlptFilter]}
                            </ComboboxChip>
                          ))}
                          <ComboboxChipsInput className="h-5 text-xs" />
                        </React.Fragment>
                      )}
                    </ComboboxValue>
                  </ComboboxChips>
                  <ComboboxContent
                    anchor={jlptAnchor}
                    align="start"
                    className="w-[220px]"
                  >
                    <ComboboxEmpty>No items found.</ComboboxEmpty>
                    <ComboboxList>
                      {(item) => (
                        <ComboboxItem key={item} value={item}>
                          {JLPT_LABELS[item as JlptFilter]}
                        </ComboboxItem>
                      )}
                    </ComboboxList>
                  </ComboboxContent>
                </Combobox>
              </div>

              <div className="space-y-2 pt-1">
                <div className="flex items-center justify-between text-xs">
                  <span className="text-muted-foreground">Stroke count</span>
                  <span className="text-muted-foreground">
                    {strokeRange[0]}-{strokeRange[1]}
                  </span>
                </div>
                <Slider
                  min={STROKE_BOUNDS.min}
                  max={STROKE_BOUNDS.max}
                  step={1}
                  value={strokeRange}
                  onValueChange={updateStrokeRange}
                  className="px-2 py-1"
                />
              </div>
            </div>
          </PopoverContent>
        </Popover>

        <div className="text-xs text-muted-foreground">
          {filteredOptions.length} results
        </div>
      </div>
      <CommandInput
        ref={searchInputRef}
        value={search}
        onValueChange={setSearch}
        placeholder={placeholder}
      />
      <CommandList style={{ height, overflow: "auto", marginTop: "0.5rem" }}>
        {flattenedOptions.length === 0 ? (
          <CommandEmpty>No results found.</CommandEmpty>
        ) : (
          <Virtuoso
            data={flattenedOptions}
            components={{ Scroller: VirtualizedScroller }}
            itemContent={(_, item) =>
              item.type === "group" ? (
                <div className="z-10 p-1 pl-2 text-sm text-foreground/50">
                  {item.value}
                </div>
              ) : (
                <CommandItem
                  value={item.value.kanji}
                  onSelect={() => onSelectOption?.(item.value)}
                >
                  <div className="flex items-center gap-2">
                    <div className="text-xl font-bold">{item.value.kanji}</div>

                    <div className="text-xs">
                      <div>{item.value.kunyomi}</div>
                      <div className="line-clamp-1">{item.value.meaning}</div>
                      <div className="text-muted-foreground">
                        {item.value.jlptLevel ?? "Unknown"} JLPT{" • "}
                        {item.value.strokeCount ?? "?"} strokes
                      </div>
                    </div>
                  </div>
                </CommandItem>
              )
            }
            style={{ height }}
          />
        )}
      </CommandList>
    </Command>
  );
};

export const SearchInput = ({
  searchPlaceholder = "Search kanji...",
}: {
  searchPlaceholder?: string;
}) => {
  const router = useRouter();
  const searchInputRef = React.useRef<HTMLInputElement>(null);

  const [open, setOpen] = React.useState<boolean>(false);
  const [selectedOption, setSelectedOption] =
    React.useState<SearchOption | null>(null);

  return (
    <Popover open={open} onOpenChange={setOpen}>
      <PopoverTrigger
        render={
          <Button
            variant="outline"
            role="combobox"
            aria-expanded={open}
            className="w-full max-w-80 justify-between md:w-[220px]"
          />
        }
      >
        <span>{selectedOption ? selectedOption.kanji : searchPlaceholder}</span>
        <ChevronsUpDown className="ml-2 h-4 w-4 shrink-0 opacity-50" />
      </PopoverTrigger>
      <PopoverContent
        className="w-full max-w-80 p-0 md:w-[220px]"
        initialFocus={() => searchInputRef.current}
      >
        <VirtualizedCommand
          height="215px"
          options={OPTIONS}
          placeholder={searchPlaceholder}
          searchInputRef={searchInputRef}
          onSelectOption={(currentValue) => {
            setSelectedOption(currentValue);
            setOpen(false);
            router.push(buildKanjiHref(currentValue.kanji));
          }}
        />
      </PopoverContent>
    </Popover>
  );
};
