import React, { useEffect } from "react";
import PropTypes from "prop-types";
import RadicalImages from "./radicalImages";
import styled from "@emotion/styled";
import { useSpring, animated } from "react-spring";

import IconButton from "@mui/material/IconButton";

type KanjiInfo = {
  id: string;
  kanjialiveData?: any;
  jishoData?: any;
};

interface Props {
  kanjiInfo: KanjiInfo;
}

export const Radical: React.FC<Props> = ({ kanjiInfo }) => {
  return (
    <RadicalWrapper>
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

// * STYLES **************************************************************************************************

const RadicalWrapper = styled.div`
  padding: 16px;
  position: relative;
  width: 100%;
  height: 100%;
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
