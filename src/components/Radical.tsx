import React, { useEffect } from "react";
import PropTypes from "prop-types";
import RadicalImages from "./RadicalImages";
import styled from "styled-components/macro";

import { useSpring, animated } from "react-spring";

import IconButton from "@material-ui/core/IconButton";
import HighlightOffIcon from "@material-ui/icons/HighlightOff";

interface Props {
  kanjiInfo: KanjiInfo;
  focusRadical: () => void;
  layoutView: Layout;
  normalView: () => void;
  mobile: boolean;
  desktop: boolean;
}

export const Radical: React.FC<Props> = (props) => {
  const { kanjiInfo, focusRadical, layoutView, normalView, mobile, desktop } =
    props;

  const [springProps] = useSpring(() => ({
    opacity: 1,
    gridTemplateColumns: "1fr 0fr",
    gridTemplateRows: "1fr 0fr",
    padding: "16px",
    scale: 0,
  }));

  useEffect(() => {
    setTimeout(() => {
      springProps.scale.start(1);
    }, 200);
  }, []);

  const closeIconSpringProps = useSpring({
    opacity: layoutView.radicalFocused ? 1 : 0,
    cursor: layoutView.radicalFocused ? "pointer" : "default",
  });

  useEffect(() => {
    if (mobile && !desktop) {
      if (
        layoutView.examplesFocused === true ||
        layoutView.kanjiFocused === true
      ) {
        springProps.opacity.start(0);
        springProps.gridTemplateColumns.start("1fr 0fr");
        springProps.gridTemplateRows.start("1fr 0fr");
        springProps.padding.start("0px");
      } else {
        springProps.opacity.start(1);
        springProps.gridTemplateColumns.start("1fr 0fr");
        springProps.gridTemplateRows.start("1fr 0fr");
        springProps.padding.start("16px");
      }
      if (layoutView.radicalFocused === true) {
        springProps.opacity.start(1);
        springProps.gridTemplateColumns.start("1fr 2fr");
        springProps.gridTemplateRows.start("1fr 1fr");
        springProps.padding.start("16px");
      }
    }
    if (desktop && !mobile) {
      springProps.opacity.start(1);
      springProps.gridTemplateColumns.start("100px 1fr");
      springProps.gridTemplateRows.start("1fr 1fr");
      springProps.padding.start("16px");
    }
  }, [layoutView, mobile, desktop]);

  const handleClose = (e) => {
    e.stopPropagation();
    normalView();
  };

  return (
    <RadicalWrapper style={springProps} onClick={focusRadical}>
      <CloseIcon style={closeIconSpringProps}>
        <IconButton
          aria-label="Close"
          size="small"
          style={{ cursor: "inherit", color: "#c4c4c4" }}
          onClick={handleClose}
        >
          <HighlightOffIcon />
        </IconButton>
      </CloseIcon>
      <Main>
        <h3>Radical</h3>
        {kanjiInfo?.jishoData?.radical?.symbol && (
          <h1>{kanjiInfo?.jishoData?.radical?.symbol}</h1>
        )}
      </Main>

      <Animation>
        {kanjiInfo?.kanjialiveData?.radical?.animation && (
          <RadicalImages
            radicalImageArray={kanjiInfo?.kanjialiveData?.radical?.animation}
          />
        )}
      </Animation>

      <Info>
        {kanjiInfo?.jishoData?.radical?.symbol && (
          <p>
            Radical: <strong>{kanjiInfo?.jishoData?.radical?.symbol}</strong>
          </p>
        )}

        {kanjiInfo?.kanjialiveData?.radical?.name?.hiragana && (
          <p>
            Reading:{" "}
            <strong>
              {kanjiInfo?.kanjialiveData?.radical?.name?.hiragana}
            </strong>
          </p>
        )}
        {kanjiInfo?.jishoData?.radical?.meaning && (
          <p>
            Meaning: <strong>{kanjiInfo?.jishoData?.radical?.meaning}</strong>
          </p>
        )}
        {kanjiInfo?.kanjialiveData?.radical?.strokes && (
          <p>
            Strokes:{" "}
            <strong>{kanjiInfo?.kanjialiveData?.radical?.strokes}</strong>
          </p>
        )}
      </Info>
    </RadicalWrapper>
  );
};

export default Radical;

Radical.propTypes = {
  kanjiInfo: PropTypes.any.isRequired,
  focusRadical: PropTypes.func.isRequired,
  layoutView: PropTypes.any.isRequired,
  normalView: PropTypes.func.isRequired,
  mobile: PropTypes.bool.isRequired,
  desktop: PropTypes.bool.isRequired,
};

// * STYLES **************************************************************************************************

const RadicalWrapper = styled(animated.div)`
  grid-area: radicalArea;
  position: relative;
  width: 100%;
  height: 100%;
  border-radius: 20px;
  background: white;
  box-shadow: 0 10px 20px rgba(0, 0, 0, 0.19), 0 6px 6px rgba(0, 0, 0, 0.23);
  overflow: hidden;
  display: grid;
  grid-template-areas:
    "main info "
    "anim info ";
`;

const Main = styled.div`
  grid-area: main;
  display: flex;
  flex-direction: column;
  align-items: center;
  width: 100%;
  height: 100%;
  overflow: hidden;

  h3 {
    font-size: 15px;
    margin: 0 0 10px 0;
  }
  h1 {
    margin: 0;
    font-size: 72px;
    line-height: 80px;
    @media (max-width: 374px) {
      font-size: 60px;
      line-height: 70px;
    }
  }
`;

const Animation = styled.div`
  grid-area: anim;
  display: flex;
  flex-direction: column;
  align-items: center;
  width: 100%;
  height: 100%;
  position: relative;
  overflow: hidden;
`;

const Info = styled.div`
  padding-top: 10px;
  grid-area: info;
  width: 100%;
  height: 100%;
  overflow: hidden;
  font-size: 15px;
  line-height: 22px;
  p {
    margin: 0;
    padding-bottom: 5px;
  }
`;

const CloseIcon = styled(animated.div)`
  position: absolute;
  top: 0;
  right: 0;
  padding: 4px;
`;
