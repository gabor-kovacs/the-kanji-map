/* eslint-disable @next/next/no-img-element */
import * as React from "react";

import styled from "@emotion/styled";

import { joyoList } from "../data/joyo";
import { jinmeiyoList } from "../data/jinmeiyo";
import IconButton from "@mui/material/IconButton";

type KanjiInfo = {
  id: string;
  kanjialiveData?: any;
  jishoData?: any;
};

interface Props {
  kanjiInfo: KanjiInfo;
  graphData: any;
  strokeAnimation: string;
}

export const Kanji: React.FC<Props> = ({
  kanjiInfo,
  graphData,
  strokeAnimation,
}) => {
  // restarting stroke animation
  const [hash, setHash] = React.useState(Date.now());

  return (
    <KanjiWrapper>
      <Main>
        <h3>Kanji</h3>
        <h1>{kanjiInfo.id}</h1>
      </Main>
      <Animation>
        {strokeAnimation && (
          <div dangerouslySetInnerHTML={{ __html: strokeAnimation }} />
        )}
      </Animation>

      <Info>
        {joyoList?.includes(kanjiInfo.id) && (
          <p>
            <strong>Jōyō kanji</strong>
            {kanjiInfo?.jishoData?.taughtIn && (
              <span>
                , Taught in <strong>{kanjiInfo?.jishoData?.taughtIn}</strong>
              </span>
            )}
          </p>
        )}

        {jinmeiyoList?.includes(kanjiInfo.id) && (
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
                {kanjiInfo?.jishoData?.kunyomi.map(
                  (kun: any, index: number) => {
                    return <span key={index}>{kun} </span>;
                  }
                )}
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
                {kanjiInfo?.jishoData?.onyomi.map((on: any, index: number) => {
                  return <span key={index}>{on} </span>;
                })}
              </strong>
            </p>
          </>
        )}

        {graphData?.noOutLinks?.links && (
          <>
            <p>
              Composition:
              {graphData.noOutLinks.links
                .filter((link: any) => link.target === kanjiInfo.id)
                .map((link: any) => link.source)
                .map((comp: any, index: number) => (
                  <IconButton
                    key={index}
                    value={comp}
                    aria-label={comp}
                    size="small"
                    style={{ color: "#212121", fontSize: 15, padding: 8 }}
                    // onClick={(e) => select(e)}
                  >
                    {comp.length <= 4 ? comp : "�"}
                  </IconButton>
                ))}
            </p>
          </>
        )}
      </Info>
    </KanjiWrapper>
  );
};

export default Kanji;

{
  /* <img
src={`data:image/svg+xml;utf8,${encodeURIComponent(
  strokeAnimation
)}`}
/> */
  // {current?.structure?.length ? (
  //   <>
  //     <p>
  //       Structure:{" "}
  //       {/* </p>
  //     <p> */}
  //       <strong>{current?.structure}</strong>
  //     </p>
  //   </>
  // ) : null}
  // {current?.composition?.length >= 2 ? (
  //   <>
  //     <p>
  //       Composition: {/* </p> */}
  //       {/* <p> */}
  //       {current?.composition.map((comp: any, index: number) => (
  //         <IconButton
  //           key={index}
  //           value={comp}
  //           aria-label={comp}
  //           size="small"
  //           style={{ color: "#212121", fontSize: 15, padding: 8 }}
  //           onClick={(e) => select(e)}
  //         >
  //           {comp.length <= 4 ? comp : "�"}
  //         </IconButton>
  //       ))}
  //     </p>
  //   </>
  // ) : null}
}

// * STYLES **************************************************************************************************

const KanjiWrapper = styled.div`
  position: relative;
  width: 100%;
  height: 100%;

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
