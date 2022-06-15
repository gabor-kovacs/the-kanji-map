import React, { useState, useEffect, SetStateAction } from "react";

import styled from "@emotion/styled";
import IconButton from "@mui/material/IconButton";
import SearchIcon from "@mui/icons-material/Search";
import HighlightOffIcon from "@mui/icons-material/HighlightOff";

import handwriting from "../lib/handwriting";

import Searchlist from "../preprocess/searchlist.json";
import Link from "next/link";

export const DrawInput: React.FC = () => {
  //  innitialize draw input
  const [canvas, setCanvas] = useState<any>(null);

  // show returned options
  const [inputSuggestions, setInputSuggestions] = useState<string[]>([]);

  const inputOptions = {
    width: 180, //int, width of the writing area, default: undefined
    height: 180, //int, height of the writing area, default: undefined
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
    const can = new handwriting.Canvas(document.getElementById("handInput"));
    setCanvas(can);
  }, []);

  const recognizeKanji = () => {
    canvas && canvas.recognize(canvas.trace, inputOptions, inputCallback);
  };

  const eraseKanji = () => {
    canvas && canvas.erase();
    setInputSuggestions([]);
  };

  return (
    <DrawInputWrapper>
      {/* <CanvasCrosshair /> */}
      <Canvas width={180} height={180} id={"handInput"} />
      <DrawInputBottom>
        <IconButton
          color="secondary"
          aria-label="Erase"
          size="small"
          onClick={eraseKanji}
        >
          <HighlightOffIcon />
        </IconButton>
        {inputSuggestions.map((suggestion, index) => (
          <Link key={index} href={`/kanji/${suggestion}`}>
            <IconButton
              color="primary"
              key={index}
              aria-label={suggestion}
              value={suggestion}
              size="small"
              style={{ color: "#212121", fontSize: 18, padding: 4 }}
              onClick={eraseKanji}
            >
              {suggestion}
            </IconButton>
          </Link>
        ))}
        <IconButton
          color="primary"
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
  /* pointer-events: auto; */
  width: 200px;
  height: 230px;

  border-radius: 20px;
  background: white;
  box-shadow: 0 10px 20px rgba(0, 0, 0, 0.19), 0 6px 6px rgba(0, 0, 0, 0.23);
`;

const DrawInputBottom = styled.div`
  height: 40px;
  width: 100%;
  padding: 0 5px;
  margin-top: -13px;
  border-bottom-left-radius: 20px;
  border-bottom-right-radius: 20px;

  display: flex;
  align-items: center;
  justify-content: space-between;
`;

const Canvas = styled.canvas`
  position: relative;
  margin: 10px;
  /* top: 10px; */
  width: 180px;
  height: 180px;
  border: 1px solid #c4c4c4;
  /* border-radius: 10px; */
  cursor: crosshair;
`;

const CanvasCrosshair = styled.div`
  position: absolute;
  top: 10px;
  left: 100px;
  height: 180px;
  width: 10px;
  border-left: 1px dotted #c4c4c4;
  pointer-events: none;

  &:after {
    content: "";
    position: absolute;
    top: 90px;
    left: -90px;
    height: 10px;
    width: 180px;
    border-top: 1px dotted #c4c4c4;
  }
`;

// const theme = createMuiTheme({
//   palette: {
//     primary: {
//       main: "#2B99CF",
//     },
//     secondary: {
//       main: "#f44336",
//     },
//   },
// });
// const innerTheme = createMuiTheme({
//   palette: {
//     primary: {
//       main: "#212121",
//     },
//   },
// });
