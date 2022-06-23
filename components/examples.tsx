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

  const highlightKanji = (text: string) => {
    const textArray = text?.split(kanjiInfo.id);
    return (
      <span>
        {textArray.map((item, index) => (
          <>
            {item}
            {index !== textArray.length - 1 && <b>{kanjiInfo.id}</b>}
          </>
        ))}
      </span>
    );
  };

  return (
    <>
      <ExamplesWrapper>
        <div>
          <h3>Examples</h3>
        </div>
        <div>
          {/* KANJIALIVE With AUDIO */}
          {kanjiInfo?.kanjialiveData?.examples && <h5>Examples with audio</h5>}
          {kanjiInfo?.kanjialiveData?.examples?.map(
            (example: any, index: number) => {
              return (
                <AudioExampleDiv key={index}>
                  <p>
                    <span>
                      {highlightKanji(example?.japanese)}
                      &nbsp;&nbsp;&nbsp;
                    </span>
                    <span>{example?.meaning?.english}&nbsp;&nbsp;&nbsp;</span>
                  </p>
                  <IconButton
                    aria-label="Play sound"
                    size="small"
                    style={{ color: "var(--color-light)" }}
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
                {highlightKanji(onExample?.example)}&nbsp;&nbsp;（
                {onExample?.reading}
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
                {highlightKanji(kunExample?.example)}&nbsp;&nbsp;（
                {kunExample?.reading}
                ）&nbsp;&nbsp;&nbsp;
                {kunExample?.meaning}
              </p>
            )
          )}
        </div>
      </ExamplesWrapper>
    </>
  );
};

export default Examples;

// * STYLES **************************************************************************************************

const ExamplesWrapper = styled.div`
  display: grid;
  grid-template-rows: 36px 1fr;
  width: 100%;
  height: 100%;
  padding: 16px;
  /* overflow: scroll; */
  overflow-y: scroll;

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
