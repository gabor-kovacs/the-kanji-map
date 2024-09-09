"use client";
import * as React from "react";
import { Button } from "@/components/ui/button";
import {
  Command,
  CommandEmpty,
  CommandInput,
  CommandList,
  CommandItem,
} from "@/components/ui/command";
import {
  Popover,
  PopoverContent,
  PopoverTrigger,
} from "@/components/ui/popover";
import { ChevronsUpDown } from "lucide-react";
import { Virtuoso } from "react-virtuoso";
import { useRouter } from "next/navigation";

import searchlist from "@/../data/searchlist.json";

const OPTIONS: SearchOption[] = searchlist.map((el) => {
  return {
    kanji: el.k,
    kunyomi: el.r,
    meaning: el.m,
    group: el.g === 1 ? "joyo" : el.g === 2 ? "jinmeiyo" : "other",
  };
});

type SearchOption = {
  kanji: string;
  kunyomi: string;
  meaning: string;
  group: "joyo" | "jinmeiyo" | "other";
};

interface VirtualizedCommandProps {
  height: string;
  options: SearchOption[];
  placeholder: string;
  selectedOption: SearchOption | null;
  onSelectOption?: (option: SearchOption) => void;
}

const VirtualizedCommand = ({
  height,
  options,
  placeholder,
  selectedOption,
  onSelectOption,
}: VirtualizedCommandProps) => {
  const [filteredOptions, setFilteredOptions] =
    React.useState<SearchOption[]>(options);

  const handleSearch = (search: string) => {
    setFilteredOptions(
      options.filter(
        (option) =>
          option.kanji.toLowerCase().includes(search.toLowerCase()) ||
          option.kunyomi.toLowerCase().includes(search.toLowerCase()) ||
          option.meaning.toLowerCase().includes(search.toLowerCase())
      )
    );
  };

  const flattenedOptions = React.useMemo(() => {
    const groups: { [key: string]: SearchOption[] } = {
      joyo: [],
      jinmeiyo: [],
      other: [],
    };

    filteredOptions.forEach((option) => {
      if (groups[option.group]) {
        groups[option.group].push(option);
      }
    });

    const flatList: { type: "group" | "item"; value: any }[] = [];

    Object.entries(groups).forEach(([groupName, groupOptions]) => {
      if (groupOptions.length > 0) {
        flatList.push({ type: "group", value: groupName });
        groupOptions.forEach((option) =>
          flatList.push({ type: "item", value: option })
        );
      }
    });

    return flatList;
  }, [filteredOptions]);

  return (
    <Command shouldFilter={false}>
      <CommandInput onValueChange={handleSearch} placeholder={placeholder} />
      <CommandList style={{ height, overflow: "auto" }}>
        {flattenedOptions.length === 0 ? (
          <CommandEmpty>No results found.</CommandEmpty>
        ) : (
          <Virtuoso
            data={flattenedOptions}
            itemContent={(index, data) =>
              data.type === "group" ? (
                <div className="z-10 p-1 pl-2 text-sm text-foreground/50">
                  {data.value}
                </div>
              ) : (
                <CommandItem
                  key={data.value.kanji}
                  value={data.value.kanji}
                  onSelect={() => onSelectOption?.(data.value)}
                >
                  <div className="flex items-center gap-2">
                    <div className="text-xl font-bold">{data.value.kanji}</div>

                    <div className="text-xs">
                      <div>{data.value.kunyomi}</div>
                      <div>{data.value.meaning}</div>
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

  const [open, setOpen] = React.useState<boolean>(false);
  const [selectedOption, setSelectedOption] =
    React.useState<SearchOption | null>(null);

  return (
    <Popover open={open} onOpenChange={setOpen}>
      <PopoverTrigger asChild>
        <Button
          variant="outline"
          role="combobox"
          aria-expanded={open}
          className="w-full max-w-80 md:w-[220px] justify-between"
        >
          {selectedOption ? `${selectedOption.kanji}` : searchPlaceholder}
          <ChevronsUpDown className="ml-2 h-4 w-4 shrink-0 opacity-50" />
        </Button>
      </PopoverTrigger>
      <PopoverContent className="p-0 w-full max-w-80 md:w-[220px]">
        <VirtualizedCommand
          height={"215px"}
          options={OPTIONS}
          placeholder={searchPlaceholder}
          selectedOption={selectedOption}
          onSelectOption={(currentValue) => {
            setSelectedOption(currentValue);
            setOpen(false);
            router.push(`/${currentValue.kanji}`);
          }}
        />
      </PopoverContent>
    </Popover>
  );
};
