import React, { useState, useEffect, SetStateAction } from "react";
import PropTypes from "prop-types";
import styled from "styled-components/macro";

import { joyoList } from "../data/joyo";
import { jinmeiyoList } from "../data/jinmeiyo";

import { useSpring, animated } from "react-spring";

import IconButton from "@material-ui/core/IconButton";
import HighlightOffIcon from "@material-ui/icons/HighlightOff";

interface Props {
  current: ChiseEntry;
  inputRef: React.RefObject<HTMLInputElement | null>;
  setInputValue: React.Dispatch<SetStateAction<string>>;
  kanjiInfo: KanjiInfo;
  focusKanji: () => void;
  layoutView: Layout;
  normalView: () => void;
  mobile: boolean;
  desktop: boolean;
}

export const Kanji: React.FC<Props> = (props) => {
  const {
    current,
    inputRef,
    setInputValue,
    kanjiInfo,
    focusKanji,
    layoutView,
    normalView,
    mobile,
    desktop,
  } = props;

  // restarting stroke animation
  const [hash, setHash] = useState(Date.now());

  const [animationExists, setAnimationExists] = useState(false);
  useEffect(() => {
    fetch(`/data/animCJK/svgsJa/${current?.kanji?.charCodeAt(0)}.svg`, {
      method: "HEAD",
    })
      .then((res) => {
        res.headers.get("content-type") === "image/svg+xml"
          ? setAnimationExists(true)
          : setAnimationExists(false);
      })
      .catch((err) => {
        console.log(err);
        setAnimationExists(false);
      });
  }, [current]);

  const select = (e) => {
    const kanji = e?.currentTarget?.value;
    setInputValue(kanji);
    inputRef.current.focus();
  };

  const [springProps] = useSpring(() => ({
    opacity: 1,
    gridTemplateColumns: "1fr 0fr",
    gridTemplateRows: "1fr 0fr",
    padding: "16px",
    scale: 0,
  }));

  useEffect(() => {
    springProps.scale.start(1);
  }, []);

  const closeIconSpringProps = useSpring({
    opacity: layoutView.kanjiFocused ? 1 : 0,
    cursor: layoutView.kanjiFocused ? "pointer" : "default",
  });

  useEffect(() => {
    if (mobile) {
      if (
        layoutView.examplesFocused === true ||
        layoutView.radicalFocused === true
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
      if (layoutView.kanjiFocused === true) {
        springProps.opacity.start(1);
        springProps.gridTemplateColumns.start("1fr 2fr");
        springProps.gridTemplateRows.start("1fr 1fr");
        springProps.padding.start("16px");
      }
    }
  }, [layoutView, mobile, desktop]);

  const handleClose = (e) => {
    e.stopPropagation();
    normalView();
  };

  useEffect(() => {
    // change from mobile to desktop
    if (desktop && !mobile) {
      springProps.gridTemplateColumns.start("100px 1fr");
      springProps.gridTemplateRows.start("1fr 1fr");
      springProps.padding.start("16px");
    }
    // change from desktop to mobile
    if (!desktop && mobile) {
      springProps.opacity.start(1);
      springProps.gridTemplateColumns.start("1fr 0fr");
      springProps.gridTemplateRows.start("1fr 0fr");
      springProps.padding.start("16px");
    }
  }, [mobile, desktop]);

  return (
    <KanjiWrapper style={springProps} onClick={focusKanji}>
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
        <h3>Kanji</h3>
        {current?.kanji && <h1>{current?.kanji}</h1>}
      </Main>
      <Animation>
        <img
          onClick={() => setHash(Date.now)} // force restart
          alt=""
          style={{ width: "80px", height: "80px", cursor: "pointer" }}
          src={
            animationExists
              ? `/data/animCJK/svgsJa/${current?.kanji?.charCodeAt(
                  0
                )}.svg?${hash}` // 1x1 transparent pixel
              : `data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNkYAAAAAYAAjCB0C8AAAAASUVORK5CYII=`
          }
        />
      </Animation>

      <Info>
        {joyoList?.includes(current?.kanji) && (
          <p>
            <strong>Jōyō kanji</strong>
            {kanjiInfo?.jishoData?.taughtIn && (
              <span>
                , Taught in <strong>{kanjiInfo?.jishoData?.taughtIn}</strong>
              </span>
            )}
          </p>
        )}

        {jinmeiyoList?.includes(current?.kanji) && (
          <p>Jinmeiyō kanji, used in names</p>
        )}

        {kanjiInfo?.jishoData?.jlptLevel && (
          <p>
            JLPT level: <strong>{kanjiInfo?.jishoData?.jlptLevel}</strong>
          </p>
        )}
        {kanjiInfo?.jishoData?.newspaperFrequencyRank && (
          <p>
            <strong>{kanjiInfo?.jishoData?.newspaperFrequencyRank}</strong> of
            2500 most used kanji in newspapers
          </p>
        )}
        {kanjiInfo?.jishoData?.strokeCount && (
          <p>
            Stroke count: <strong>{kanjiInfo?.jishoData?.strokeCount}</strong>
          </p>
        )}
        {kanjiInfo?.jishoData?.meaning && (
          <>
            <p>
              Meaning:{" "}
              {/* </p>
						<p> */}
              <strong>{kanjiInfo?.jishoData?.meaning}</strong>
            </p>
          </>
        )}
        {kanjiInfo?.jishoData?.kunyomi && (
          <>
            <p>
              Kunyomi:{" "}
              {/* </p>
						<p> */}
              <strong>
                {kanjiInfo?.jishoData?.kunyomi.map((kun, index) => {
                  return <span key={index}>{kun} </span>;
                })}
              </strong>
            </p>
          </>
        )}
        {kanjiInfo?.jishoData?.onyomi && (
          <>
            <p>
              Onyomi:{" "}
              {/* </p>
						<p> */}
              <strong>
                {kanjiInfo?.jishoData?.onyomi.map((on, index) => {
                  return <span key={index}>{on} </span>;
                })}
              </strong>
            </p>
          </>
        )}

        {current?.structure?.length ? (
          <>
            <p>
              Structure:{" "}
              {/* </p>
						<p> */}
              <strong>{current?.structure}</strong>
            </p>
          </>
        ) : null}
        {current?.composition?.length >= 2 ? (
          <>
            <p>
              Composition: {/* </p> */}
              {/* <p> */}
              {current?.composition.map((comp, index) => (
                <IconButton
                  key={index}
                  value={comp}
                  aria-label={comp}
                  size="small"
                  style={{ color: "#212121", fontSize: 15, padding: 8 }}
                  onClick={(e) => select(e)}
                >
                  {comp.length <= 4 ? comp : "�"}
                </IconButton>
              ))}
            </p>
          </>
        ) : null}
      </Info>
    </KanjiWrapper>
  );
};

export default Kanji;

Kanji.propTypes = {
  current: PropTypes.any.isRequired,
  inputRef: PropTypes.any.isRequired,
  setInputValue: PropTypes.func.isRequired,
  kanjiInfo: PropTypes.any.isRequired,
  focusKanji: PropTypes.any.isRequired,
  layoutView: PropTypes.any.isRequired,
  normalView: PropTypes.func.isRequired,
  mobile: PropTypes.bool.isRequired,
  desktop: PropTypes.bool.isRequired,
};

// * STYLES **************************************************************************************************

const KanjiWrapper = styled(animated.div)`
  position: relative;
  grid-area: kanjiArea;
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
  /* box-shadow: inset 0 0 16px #fff; */
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
  overflow: hidden;
`;

const Info = styled.div`
  padding-top: 10px;
  grid-area: info;
  width: 100%;
  height: 100%;
  overflow: auto;
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
