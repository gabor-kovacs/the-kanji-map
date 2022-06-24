import React, { useState, useEffect, SetStateAction } from "react";

import styled from "@emotion/styled";
import IconButton from "@mui/material/IconButton";
import SearchIcon from "@mui/icons-material/Search";
import HighlightOffIcon from "@mui/icons-material/HighlightOff";

import handwriting from "../lib/handwriting";

import Searchlist from "../preprocess/searchlist.json";
import Link from "next/link";
import { useTheme } from "next-themes";

export const DrawInput: React.FC = () => {
  const { theme } = useTheme();
  //  innitialize draw input
  const [canvas, setCanvas] = useState<any>(null);

  // show returned options
  const [inputSuggestions, setInputSuggestions] = useState<string[]>([]);

  const inputOptions = {
    width: 220, //int, width of the writing area, default: undefined
    height: 220, //int, height of the writing area, default: undefined
    language: "ja", //string, language of input trace, default: "zh_TW"
    numOfWords: 1, //int, number of words of input trace, default: undefined
    numOfReturn: 64, //int, number of maximum returned results, default: undefined // ! was 4
  };

  const inputCallback = (result: string[], err: string) => {
    if (err) {
      console.log(err);
    } else {
      const kanjiList = Searchlist.map((entry) => entry.k);
      const filtered = result
        .filter((entry) => kanjiList.includes(entry))
        .slice(0, 4);

      setInputSuggestions(filtered);
    }
  };

  // init
  useEffect(() => {
    eraseKanji();
    const can = new handwriting.Canvas(
      document.getElementById("handInput"),
      theme
    );
    setCanvas(can);
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [theme]);

  const recognizeKanji = () => {
    canvas && canvas.recognize(canvas.trace, inputOptions, inputCallback);
  };

  const eraseKanji = () => {
    canvas && canvas.erase();
    setInputSuggestions([]);
  };

  return (
    <DrawInputWrapper>
      <CanvasCrosshair />
      <Canvas width={220} height={220} id={"handInput"} />
      <DrawInputBottom>
        <IconButton
          style={{ color: "var(--color-danger)" }}
          aria-label="Erase"
          size="small"
          onClick={eraseKanji}
        >
          <HighlightOffIcon />
        </IconButton>
        {inputSuggestions.map((suggestion, index) => (
          <Link key={index} href={`/kanji/${suggestion}`}>
            <IconButton
              key={index}
              aria-label={suggestion}
              value={suggestion}
              size="small"
              style={{
                color: "var(--color-light)",
                width: "35px",
                fontSize: 18,
                padding: 4,
              }}
              onClick={eraseKanji}
            >
              {suggestion}
            </IconButton>
          </Link>
        ))}

        <IconButton
          style={{ color: "var(--color-primary)" }}
          aria-label="Recognize"
          size="small"
          onClick={recognizeKanji}
        >
          <SearchIcon />
        </IconButton>
      </DrawInputBottom>
    </DrawInputWrapper>
  );
};

export default DrawInput;

// * STYLES **************************************************************************************************

const DrawInputWrapper = styled.div`
  position: relative;
  width: 100%;
  height: 220px;
  padding-bottom: 0;
  background: var(--color-background);
`;

const DrawInputBottom = styled.div`
  height: 40px;
  width: 100%;
  padding: 0 5px;
  display: flex;
  align-items: center;
  justify-content: space-between;
`;

const Canvas = styled.canvas`
  position: relative;
  width: 220px;
  height: 220px;
  border: 1px solid var(--color-light);
  border-radius: 4px;
  cursor: crosshair;
`;

const CanvasCrosshair = styled.div`
  position: absolute;
  left: 110px;
  height: 220px;
  width: 10px;
  border-left: 1px dashed var(--color-lighter);
  pointer-events: none;

  &:after {
    content: "";
    position: absolute;
    top: 110px;
    left: -110px;
    height: 10px;
    width: 220px;
    border-top: 1px dashed var(--color-lighter);
  }
`;
