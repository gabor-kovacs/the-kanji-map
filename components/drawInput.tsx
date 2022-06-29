import * as React from "react";

import styled from "@emotion/styled";
import IconButton from "@mui/material/IconButton";
import SearchIcon from "@mui/icons-material/Search";
import HighlightOffIcon from "@mui/icons-material/HighlightOff";

import handwriting from "../lib/handwriting";

import Searchlist from "../data/searchlist.json";
import Link from "next/link";
import { useTheme } from "next-themes";
import DeadSpace from "./deadspace";

export const DrawInput: React.FC = () => {
  const { theme, systemTheme } = useTheme();
  //  innitialize draw input
  const [canvas, setCanvas] = React.useState<any>(null);

  const [actualTheme, setActualTheme] = React.useState<null | "light" | "dark">(
    null
  );
  React.useEffect(() => {
    if (theme === "light" || (theme === "system" && systemTheme === "light")) {
      setActualTheme("light");
    }
    if (theme === "dark" || (theme === "system" && systemTheme === "dark")) {
      setActualTheme("dark");
    }
  }, [theme, systemTheme]);

  // show returned options
  const [inputSuggestions, setInputSuggestions] = React.useState<string[]>([]);

  const inputOptions = {
    width: 220, //int, width of the writing area, default: undefined
    height: 220, //int, height of the writing area, default: undefined
    language: "ja", //string, language of input trace, default: "zh_TW"
    numOfWords: 1, //int, number of words of input trace, default: undefined
    numOfReturn: 64, //int, number of maximum returned results, default: undefined // ! was 4
  };

  const inputCallback = (result: string[], err: string) => {
    if (err) {
      return;
      // console.log(err);
    } else {
      const kanjiList = Searchlist.map((entry) => entry.k);
      const filtered = result
        .filter((entry) => kanjiList.includes(entry))
        .slice(0, 4);

      setInputSuggestions(filtered);
    }
  };

  // init
  React.useEffect(() => {
    eraseKanji();
    const can = new handwriting.Canvas(
      document.getElementById("handInput"),
      actualTheme
    );
    setCanvas(can);
  }, [actualTheme]);

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
      <DeadSpace>
        <Canvas width={220} height={220} id={"handInput"} />
      </DeadSpace>
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
          <Link key={index} href={`/${suggestion}`}>
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
  width: 220px;
  margin: 0 auto;
  height: 220px;
  padding-bottom: 0;
  background: var(--color-background);

  @media (max-width: 767px) {
    margin-top: 64px;
  }
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
