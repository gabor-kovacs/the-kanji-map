import * as React from "react";
import Link from "next/link";
import styled from "@emotion/styled";
import handwriting from "../lib/handwriting";
import useActualTheme from "../lib/useActualTheme";
import DeadSpace from "./deadspace";
import Searchlist from "../data/searchlist.json";
import SearchIcon from "@mui/icons-material/Search";
import HighlightOffIcon from "@mui/icons-material/HighlightOff";
import IconButton from "@mui/material/IconButton";

export const DrawInput: React.FC = () => {
  //  innitialize draw input
  const [canvas, setCanvas] = React.useState<any>(null);

  const actualTheme = useActualTheme();

  // show returned options
  const [inputSuggestions, setInputSuggestions] = React.useState<string[]>([]);

  const inputOptions = {
    width: 220,
    height: 220,
    language: "ja",
    numOfWords: 1,
    numOfReturn: 64,
  };

  const inputCallback = (result: string[], err: string) => {
    if (err) {
      return;
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
    // eslint-disable-next-line react-hooks/exhaustive-deps
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
