import React, { useState, useEffect, SetStateAction } from "react";
import PropTypes from "prop-types";

import styled from "styled-components/macro";
import TextField from "@material-ui/core/TextField";
import IconButton from "@material-ui/core/IconButton";
import CreateIcon from "@material-ui/icons/Create";

import {
  createMuiTheme,
  ThemeProvider as MuiThemeProvider,
} from "@material-ui/core/styles";

import Autocomplete from "@material-ui/lab/Autocomplete";

const filterOptions = (options, input) => {
  if (input?.inputValue?.length > 0) {
    return options.filter(
      (e) =>
        e?.meaning?.toLowerCase()?.includes(input?.inputValue?.toLowerCase()) ||
        e?.id?.toLowerCase()?.includes(input?.inputValue?.toLowerCase())
    );
  } else {
    return [];
  }
};

interface Props {
  data: Record<string, KanjiInfo>;
  kanjiHistory: (string | null)[];
  drawInputOpen: boolean;
  setDrawInputOpen: React.Dispatch<SetStateAction<boolean>>;
  mobile: boolean;
  smallmobile: boolean;
  setCurrent: React.Dispatch<SetStateAction<ChiseEntry>>;
  chise: Record<string, ChiseEntry>;
  inputValue: string;
  setInputValue: React.Dispatch<SetStateAction<string>>;
  inputRef: React.RefObject<HTMLInputElement | null>;
}

export const SearchAndHistory: React.FC<Props> = (props) => {
  const {
    data,
    kanjiHistory,
    drawInputOpen,
    setDrawInputOpen,
    mobile,
    smallmobile,
    setCurrent,
    chise,
    inputValue,
    setInputValue,
    inputRef,
  } = props;

  const selectHistory = (e) => {
    const kanji = e?.currentTarget?.value;
    chise && kanji && chise[kanji] && setCurrent(chise[kanji]);
  };

  const [options, setOptions] = useState(null);

  useEffect(() => {
    if (data) {
      const newOptions = [];
      for (const [kanji, kanjidata] of Object.entries(data)) {
        if (kanji.length <= 4) {
          newOptions.push({
            id: kanji,
            meaning:
              kanjidata?.jishoData?.meaning !== undefined
                ? kanjidata?.jishoData?.meaning
                : "",
          });
        }
      }
      setOptions(newOptions);
    }
  }, [data]);

  const handleInputSelect = (selected) => {
    if (chise && selected?.id && chise[selected.id]) {
      setCurrent(chise[selected.id]);
      inputRef.current.blur();
    }
  };

  // * LIST

  return (
    <MuiThemeProvider theme={inputTheme}>
      <SearchAndHistoryWrapper>
        <SearchDiv>
          {options && (
            <Autocomplete
              filterOptions={(options, inputValue) =>
                filterOptions(options, inputValue)
              }
              disableListWrap
              openOnFocus={true}
              options={options}
              getOptionLabel={(option: any) =>
                `${option?.id} 
						${option?.meaning}`
              }
              renderInput={(params) => (
                <TextField
                  {...params}
                  inputRef={inputRef}
                  style={{
                    width: mobile ? (smallmobile ? "135px" : "160px") : "200px",
                  }}
                  color="primary"
                  label="Search"
                  variant="outlined"
                  size="small"
                />
              )}
              onChange={(e, newValue) => {
                handleInputSelect(newValue);
              }}
              inputValue={inputValue}
              onInputChange={(e, newInputValue) => {
                setInputValue(newInputValue);
              }}
            />
          )}
          {mobile && (
            <IconButton
              className="openDrawInput"
              aria-label="Draw Input"
              size="small"
              style={{
                color: drawInputOpen ? "#2B99CF" : "#c4c4c4",
                marginLeft: "5px",
              }}
              onClick={() => setDrawInputOpen(!drawInputOpen)}
            >
              <CreateIcon />
            </IconButton>
          )}
        </SearchDiv>

        <HistoryDiv>
          {/* <p>recent: </p> */}
          {kanjiHistory
            .filter((e) => e)
            .map((historyElement, index) => (
              <IconButton
                key={index}
                value={historyElement}
                aria-label={historyElement}
                size="small"
                style={{ fontSize: 16, padding: 8 }}
                onClick={(e) => selectHistory(e)}
              >
                {historyElement}
              </IconButton>
            ))}
        </HistoryDiv>
      </SearchAndHistoryWrapper>
    </MuiThemeProvider>
  );
};

export default SearchAndHistory;

SearchAndHistory.propTypes = {
  data: PropTypes.any.isRequired,
  kanjiHistory: PropTypes.any.isRequired,
  drawInputOpen: PropTypes.bool.isRequired,
  setDrawInputOpen: PropTypes.func.isRequired,
  mobile: PropTypes.bool.isRequired,
  smallmobile: PropTypes.bool.isRequired,
  setCurrent: PropTypes.func.isRequired,
  chise: PropTypes.any.isRequired,
  inputValue: PropTypes.string.isRequired,
  setInputValue: PropTypes.func.isRequired,
  inputRef: PropTypes.any.isRequired,
};

interface Props {
  data: Record<string, KanjiInfo>;
  kanjiHistory: (string | null)[];
  drawInputOpen: boolean;
  setDrawInputOpen: React.Dispatch<SetStateAction<boolean>>;
  mobile: boolean;
  smallmobile: boolean;
  setCurrent: React.Dispatch<SetStateAction<ChiseEntry>>;
  chise: Record<string, ChiseEntry>;
  inputValue: string;
  setInputValue: React.Dispatch<SetStateAction<string>>;
  inputRef: React.RefObject<HTMLInputElement | null>;
}

// * STYLES **************************************************************************************************

const SearchAndHistoryWrapper = styled.div`
  height: 50px;
  max-width: 1000px;
  margin: 0 auto;
  padding: 0 16px;
  display: flex;
  align-items: center;
  justify-content: flex-start;

  p {
    margin: 0;
    line-height: 30px;
    padding-left: 10px;
    padding-right: 10px;
  }

  @media (min-width: 1000px) {
    height: 100px;
    flex-direction: column;
    width: 200px;
    padding: 0;
    margin: 0;

    p {
      padding-left: 4px;
    }
  }
`;

const HistoryDiv = styled.div`
  width: 100%;
  display: flex;
  align-items: center;
  justify-content: flex-start;
  @media (min-width: 1000px) {
    justify-content: center;
  }
`;

const inputTheme = createMuiTheme({
  palette: {
    primary: {
      main: "#2B99CF",
    },
  },
});

const SearchDiv = styled.div`
  display: flex;
  align-items: center;
`;
