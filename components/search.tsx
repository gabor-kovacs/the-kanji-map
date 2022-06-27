import * as React from "react";

import TextField from "@mui/material/TextField";
import Autocomplete, { autocompleteClasses } from "@mui/material/Autocomplete";
import ListSubheader from "@mui/material/ListSubheader";
import Popper from "@mui/material/Popper";
import { VariableSizeList, ListChildComponentProps } from "react-window";

import SearchList from "../data/searchlist.json";
import type { FilterOptionsState } from "@mui/material/useAutocomplete";

import styled from "@emotion/styled";

import { useTheme } from "next-themes";
import { NextRouter, useRouter } from "next/router";
import Link from "next/link";

type SearchOption = {
  kanji: string;
  kunyomi: string;
  meaning: string;
  group: "joyo" | "jinmeiyo" | "other";
};

interface ListChildComponentPropsWithRouter extends ListChildComponentProps {
  router: NextRouter;
}

const filterOptions = (
  options: SearchOption[],
  input: FilterOptionsState<SearchOption>
) => {
  if (input?.inputValue?.length > 0) {
    return options.filter(
      (e) =>
        e?.meaning?.toLowerCase()?.includes(input?.inputValue?.toLowerCase()) ||
        e?.kanji?.toLowerCase()?.includes(input?.inputValue?.toLowerCase())
    );
  } else {
    return [];
  }
};

const OPTIONS: SearchOption[] = SearchList.map((el) => {
  return {
    kanji: el.k,
    kunyomi: el.r,
    meaning: el.m,
    group: el.g === 1 ? "joyo" : el.g === 2 ? "jinmeiyo" : "other",
  };
});

const sortFunction = (a: SearchOption, b: SearchOption) => {
  if (a.group === "joyo") {
    return -1;
  }
  if (a.group === "jinmeiyo" && b.group === "joyo") {
    return 1;
  }
  if (a.group === "jinmeiyo" && b.group === "jinmeiyo") {
    return 0;
  }
  if (a.group === "jinmeiyo" && b.group === "other") {
    return -1;
  }
  if (a.group === "other") {
    return 1;
  } else {
    return 0;
  }
};

/******************************************* */

const LISTBOX_PADDING = 5; // px

// function renderRow(props: ListChildComponentPropsWithRouter) {
function renderRow(props: ListChildComponentProps) {
  const { data, index, style } = props;
  // const { data, index, style, router } = props;
  const dataSet = data[index];

  const inlineStyle = {
    ...style,
    top: (style.top as number) + LISTBOX_PADDING,
  };

  if (dataSet.hasOwnProperty("group")) {
    return (
      <ListSubheader
        key={dataSet.key}
        component="div"
        style={{
          ...inlineStyle,
          backgroundColor: "var(--color-background)",
          color: "var(--color-light)",
        }}
      >
        {dataSet.group}
      </ListSubheader>
    );
  }

  return (
    <Link href={`/${dataSet[1]?.kanji}`}>
      <ListElement
        {...dataSet[0]}
        style={inlineStyle}
        // onMouseEnter={prefetchRoute}
      >
        <div>
          <h3> {dataSet[1]?.kanji ?? ""}</h3>
        </div>
        <div>
          <p>{dataSet[1]?.kunyomi ?? ""}</p>
          <p>{dataSet[1]?.meaning ?? ""}</p>
        </div>
      </ListElement>
    </Link>
  );
}

const OuterElementContext = React.createContext({});

const OuterElementType = React.forwardRef<HTMLDivElement>((props, ref) => {
  const outerProps = React.useContext(OuterElementContext);
  return <div ref={ref} {...props} {...outerProps} />;
});
OuterElementType.displayName = "OuterElementType";

function useResetCache(data: any) {
  const ref = React.useRef<VariableSizeList>(null);
  React.useEffect(() => {
    if (ref.current != null) {
      ref.current.resetAfterIndex(0, true);
    }
  }, [data]);
  return ref;
}

// Adapter for react-window
const ListboxComponent = React.forwardRef<
  HTMLDivElement,
  React.HTMLAttributes<HTMLElement>
>(function ListboxComponent(props, ref) {
  // const router = useRouter();
  const { children, ...other } = props;
  const itemData: React.ReactChild[] = [];
  (children as React.ReactChild[]).forEach(
    (item: React.ReactChild & { children?: React.ReactChild[] }) => {
      itemData.push(item);
      itemData.push(...(item.children || []));
    }
  );

  const itemCount = itemData.length;
  const itemSize = 40;

  const getChildSize = (child: React.ReactChild) => {
    if (child.hasOwnProperty("group")) {
      return 36;
    }
    return 40;
  };

  const getHeight = () => {
    if (itemCount > 8) {
      return 8 * itemSize;
    }
    return itemData.length * itemSize;
  };

  const gridRef = useResetCache(itemCount);

  return (
    <div ref={ref}>
      <OuterElementContext.Provider value={other}>
        <VariableSizeList
          itemData={itemData}
          height={getHeight() + 2 * LISTBOX_PADDING}
          // height={400}
          width="100%"
          ref={gridRef}
          outerElementType={OuterElementType}
          innerElementType="ul"
          itemSize={(index) => getChildSize(itemData[index])}
          // itemSize={() => 40}
          overscanCount={5}
          itemCount={itemCount}
          style={{ backgroundColor: "var(--color-background)" }}
        >
          {renderRow}
          {/* {(props) => renderRow({ ...props, router })} */}
        </VariableSizeList>
      </OuterElementContext.Provider>
    </div>
  );
});

const StyledPopper = styled(Popper)({
  [`& .${autocompleteClasses.listbox}`]: {
    boxSizing: "border-box",
    "& ul": {
      padding: 0,
      margin: 0,
    },
  },
});

/******************************************* */

const Search: React.FC = () => {
  const router = useRouter();
  const inputRef = React.useRef<HTMLInputElement>(null);

  React.useEffect(() => {
    router.events.on("routeChangeComplete", () => {
      inputRef?.current?.blur();
    });
    return () => {
      router.events.off("routeChangeComplete", () => {});
    };
  }, [router.events]);

  return (
    <Autocomplete
      freeSolo={true}
      filterOptions={(options, inputValue) =>
        filterOptions(options, inputValue)
      }
      disableListWrap
      PopperComponent={StyledPopper}
      ListboxComponent={ListboxComponent}
      openOnFocus={true}
      groupBy={(option) => option.group}
      options={OPTIONS.sort(sortFunction)}
      getOptionLabel={(option: any) => ``}
      // getOptionLabel={(option: any) => `${option?.kanji}`}
      renderInput={(params) => (
        <StyledTextField
          // sx={[{ color: "var(--color-primary)" }]}
          inputRef={inputRef}
          {...params}
          label="Search"
          variant="outlined"
          size="small"
        />
      )}
      renderOption={(props, option) =>
        [props, option] as unknown as React.ReactNode
      }
      renderGroup={(params) => params as unknown as React.ReactNode}
    />
  );
};

export default Search;

const ListElement = styled.li`
  display: grid;
  grid-template-columns: 20px 1fr;
  grid-column-gap: 8px;
  width: 100%;
  height: 40px;
  overflow: hidden;
  border-top: 1px solid var(--color-lighter);

  p {
    white-space: nowrap;
    width: 150px;
    overflow: hidden;
    text-overflow: ellipsis;
  }

  background-color: var(--color-background);
  color: var(--color-foreground);

  &:hover {
    background-color: var(--color-lighter) !important;
  }

  div:first-of-type {
    font-size: 20px;
    line-height: 40px;
  }
  div:last-child {
    color: var(--color-light);
    padding: 4px 0;
    grid-template-rows: 1fr 1fr;
    font-size: 12px;
    line-height: 16px;
    vertical-align: center;
  }
`;

const StyledTextField = styled(TextField)`
  background-color: var(--color-background);

  /* placeholder color */
  & .MuiInputLabel-root {
    color: var(--color-light) !important;
  }
  /* & label.Mui-focused {
    color: var(--color-primary) !important;
  } */
  & .MuiInputBase-input {
    color: var(--color-light) !important;
  }

  & fieldset {
    border-color: var(--color-light) !important;
  }

  &:hover fieldset {
    border-color: var(--color-primary) !important;
  }
  &.Mui-focused fieldset {
    border-color: var(--color-primary) !important;
  }

  svg {
    fill: var(--color-light);
  }
`;
