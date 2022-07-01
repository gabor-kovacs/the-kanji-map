import * as React from "react";
import RadicalImages from "./radicalImages";
import styled from "@emotion/styled";

interface Props {
  kanjiInfo: KanjiInfo | null;
}

export const Radical: React.FC<Props> = ({ kanjiInfo }) => {
  return (
    <RadicalWrapper>
      <Title>
        <h3>Radical</h3>
      </Title>
      <Main>
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

const RadicalWrapper = styled.div`
  padding: 16px;
  position: relative;
  width: 100%;
  height: 100%;
  overflow: hidden;
  display: grid;
  grid-template-areas:
    "title info"
    "main info "
    "anim info ";
  grid-template-rows: 36px 1fr 1fr;
  grid-template-columns: 100px 1fr;
  grid-column-gap: 10px;

  @media (max-width: 767px) {
    grid-template-rows: 36px 150px 1fr;
  }
`;

const Title = styled.div`
  grid-area: title;
  align-self: center;
`;

const Main = styled.div`
  grid-area: main;
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
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
  place-self: center;
  width: 80px;
  height: 80px;
  position: relative;
  overflow: hidden;

  @media (max-width: 767px) {
    align-self: start;
  }
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
