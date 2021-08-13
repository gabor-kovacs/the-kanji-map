import React, { useState, useEffect, SetStateAction } from "react";
import PropTypes from "prop-types";

import styled from "styled-components";
import IconButton from "@material-ui/core/IconButton";
import SearchIcon from "@material-ui/icons/Search";
import HighlightOffIcon from "@material-ui/icons/HighlightOff";

import {
  createMuiTheme,
  ThemeProvider as MuiThemeProvider,
} from "@material-ui/core/styles";
import { useSpring, animated } from "react-spring";

// * using handwriting.js *******************************************************************************

interface Props {
  drawInputOpen: boolean;
  setDrawInputOpen: React.Dispatch<SetStateAction<boolean>>;
  mobile: boolean;
  setInputValue: React.Dispatch<SetStateAction<string>>;
  inputRef: React.RefObject<HTMLInputElement | null>;
}

export const DrawInput: React.FC<Props> = (props) => {
  const { drawInputOpen, setDrawInputOpen, mobile, setInputValue, inputRef } =
    props;
  //  innitialize draw input
  const [canvas, setCanvas] = useState<any>(null);
  // show returned options
  const [inputSuggestions, setInputSuggestions] = useState<string[]>([]);

  const inputOptions = {
    width: 180, //int, width of the writing area, default: undefined
    height: 180, //int, height of the writing area, default: undefined
    language: "ja", //string, language of input trace, default: "zh_TW"
    numOfWords: 1, //int, number of words of input trace, default: undefined
    numOfReturn: 4, //int, number of maximum returned results, default: undefined
  };

  const inputCallback = (result: string[], err: string) => {
    if (err) throw err;
    else setInputSuggestions(result);
  };

  useEffect(() => {
    //@ts-ignore
    const can1 = new window.handwriting.Canvas(
      document.getElementById("handInput")
    );
    setCanvas(can1);
  }, []);

  const recognizeKanji = () => {
    canvas && canvas.recognize(canvas.trace, inputOptions, inputCallback);
  };

  const eraseKanji = () => {
    canvas && canvas.erase();
    setInputSuggestions([]);
  };

  const chooseInput = (e: React.MouseEvent<HTMLButtonElement>) => {
    const chosenInput = e?.currentTarget?.value;
    setInputValue(chosenInput);
    inputRef && inputRef.current && inputRef.current.focus();
    eraseKanji();
    mobile && setDrawInputOpen(!drawInputOpen);
  };

  // const handleClickAway = (e: React.MouseEvent<HTMLDivElement>) => {
  const handleClickAway = () => {
    mobile && setDrawInputOpen(false);
    eraseKanji();
  };

  const drawInputSpringProps = useSpring({
    opacity: drawInputOpen ? 1 : 0,
    scale: drawInputOpen ? 1 : 0,
  });
  const backDropColor = useSpring({
    background: drawInputOpen ? "rgba(0, 0, 0, 0.5)" : "rgba(0, 0, 0, 0)",
  });

  return (
    <>
      {mobile && (
        <BackDrop
          style={backDropColor}
          $drawInputOpen={drawInputOpen}
          onClick={handleClickAway}
        />
      )}
      <DrawInputWrapper style={drawInputSpringProps}>
        <CanvasCrosshair />
        <Canvas width={180} height={180} id={"handInput"} />
        <DrawInputBottom>
          <MuiThemeProvider theme={theme}>
            <IconButton
              color="secondary"
              aria-label="Erase"
              size="small"
              onClick={eraseKanji}
            >
              <HighlightOffIcon />
            </IconButton>

            <MuiThemeProvider theme={innerTheme}>
              {inputSuggestions.map((suggestion, index) => (
                <IconButton
                  color="primary"
                  key={index}
                  aria-label={suggestion}
                  value={suggestion}
                  size="small"
                  style={{ color: "#212121", fontSize: 18, padding: 4 }}
                  onClick={(e) => chooseInput(e)}
                >
                  {suggestion}
                </IconButton>
              ))}
            </MuiThemeProvider>
            <IconButton
              color="primary"
              aria-label="Recognize"
              size="small"
              onClick={recognizeKanji}
            >
              <SearchIcon />
            </IconButton>
          </MuiThemeProvider>
        </DrawInputBottom>
      </DrawInputWrapper>
    </>
  );
};

export default DrawInput;

DrawInput.propTypes = {
  drawInputOpen: PropTypes.bool.isRequired,
  setDrawInputOpen: PropTypes.func.isRequired,
  mobile: PropTypes.bool.isRequired,
  setInputValue: PropTypes.func.isRequired,
  inputRef: PropTypes.any.isRequired,
};

type BackDropProps = {
  $drawInputOpen?: boolean;
};

// * STYLES **************************************************************************************************

const BackDrop = styled(animated.div)`
  position: absolute;
  top: 0;
  bottom: 0;
  left: 0;
  right: 0;
  pointer-events: ${(props: BackDropProps) =>
    props.$drawInputOpen ? "unset" : "none"};
`;

const DrawInputWrapper = styled(animated.div)`
  pointer-events: auto;
  //mobile
  position: absolute;
  top: 100px;
  left: calc(50% - 100px);
  width: 200px;
  height: 230px;
  border-radius: 20px;
  background: white;
  box-shadow: 0 10px 20px rgba(0, 0, 0, 0.19), 0 6px 6px rgba(0, 0, 0, 0.23);

  //desktop
  @media (min-width: 1000px) {
    position: relative;
    top: 0;
    left: 0;
    grid-area: drawArea;
  } ;
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
  width: 180px;
  height: 180px;
  border: 1px solid #c4c4c4;
  border-radius: 10px;
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

const theme = createMuiTheme({
  palette: {
    primary: {
      main: "#2B99CF",
    },
    secondary: {
      main: "#f44336",
    },
  },
});
const innerTheme = createMuiTheme({
  palette: {
    primary: {
      main: "#212121",
    },
  },
});
