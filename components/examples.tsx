import * as React from "react";
import styled from "@emotion/styled";
import IconButton from "@mui/material/IconButton";
import PlayCircleFilledIcon from "@mui/icons-material/PlayCircleFilled";
import HighlightOffIcon from "@mui/icons-material/HighlightOff";

type KanjiInfo = {
  id: string;
  kanjialiveData?: any;
  jishoData?: any;
};

interface Props {
  kanjiInfo: KanjiInfo;
}

export const Examples: React.FC<Props> = ({ kanjiInfo }) => {
  const playSound = (url: string) => {
    const audio = new Audio(url);
    audio.play();
  };

  return (
    <>
      <ExamplesWrapper>
        <h3>Examples</h3>
        {/* KANJIALIVE With AUDIO */}
        {kanjiInfo?.kanjialiveData?.examples && <h5>Examples with audio</h5>}
        {kanjiInfo?.kanjialiveData?.examples?.map(
          (example: any, index: number) => {
            return (
              <AudioExampleDiv key={index}>
                <p>
                  <span>{example?.japanese}&nbsp;&nbsp;&nbsp;</span>
                  <span>{example?.meaning?.english}&nbsp;&nbsp;&nbsp;</span>
                </p>
                <IconButton
                  aria-label="Play sound"
                  size="small"
                  onClick={() =>
                    example && example.audio && playSound(example?.audio?.mp3)
                  }
                >
                  <PlayCircleFilledIcon fontSize="small" />
                </IconButton>
              </AudioExampleDiv>
            );
          }
        )}
        {/* JISHO */}
        {kanjiInfo?.jishoData?.onyomiExamples &&
          kanjiInfo?.jishoData?.onyomiExamples?.length !== 0 && (
            <h5>Onyomi Examples</h5>
          )}
        {kanjiInfo?.jishoData?.onyomiExamples?.map(
          (onExample: any, index: number) => (
            <p key={index}>
              {onExample?.example}&nbsp;&nbsp;（{onExample?.reading}
              ）&nbsp;&nbsp;&nbsp;
              {onExample?.meaning}
            </p>
          )
        )}

        {kanjiInfo?.jishoData?.kunyomiExamples &&
          kanjiInfo?.jishoData?.kunyomiExamples?.length !== 0 && (
            <h5>Kunyomi Examples</h5>
          )}
        {kanjiInfo?.jishoData?.kunyomiExamples?.map(
          (kunExample: any, index: number) => (
            <p key={index}>
              {kunExample?.example}&nbsp;&nbsp;（{kunExample?.reading}
              ）&nbsp;&nbsp;&nbsp;
              {kunExample?.meaning}
            </p>
          )
        )}
      </ExamplesWrapper>
    </>
  );
};

export default Examples;

// * STYLES **************************************************************************************************

const ExamplesWrapper = styled.div`
  position: relative;
  width: 100%;
  height: 100%;
  padding: 16px;
  /* overflow: scroll; */
  overflow-y: scroll;

  h3 {
    font-size: 15px;
    margin: 0;
    text-align: center;
    padding-bottom: 16px;
    @media (max-width: 374px) {
      font-size: 11px;
    }
    @media (min-width: 1000px) {
      width: 100px;
    }
  }

  h5 {
    margin: 10px 0;
  }
  p {
    font-size: 15px;
    line-height: 22px;
    margin: 0;
    padding-bottom: 5px;
  }
`;

const AudioExampleDiv = styled.div`
  display: flex;
  justify-content: space-between;
  align-items: flex-end;
`;
