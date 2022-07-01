/* eslint-disable @next/next/no-img-element */
import * as React from "react";
import styled from "@emotion/styled";
import { joyoList } from "../data/joyo";
import { jinmeiyoList } from "../data/jinmeiyo";
import { useTimer } from "react-timer-hook";

interface Props {
  kanjiInfo: KanjiInfo | null;
  graphData: BothGraphData | null;
  strokeAnimation: string | null;
}

export const Kanji: React.FC<Props> = ({
  kanjiInfo,
  graphData,
  strokeAnimation,
}) => {
  // restarting stroke animation
  const [hash, setHash] = React.useState(Date.now());

  const restartAnimation = () => {
    setHash(Date.now);
  };

  const time = new Date();
  time.setSeconds(time.getSeconds() + 3);
  const { restart } = useTimer({
    expiryTimestamp: time,
    onExpire: () => {
      restartAnimation();
    },
  });

  return (
    <KanjiWrapper>
      <Title>
        <h3>Kanji</h3>
      </Title>
      <Main>
        <h1>{kanjiInfo?.id}</h1>
      </Main>
      <Animation
        onAnimationEnd={() => {
          const time = new Date();
          time.setSeconds(time.getSeconds() + 3);
          restart(time);
        }}
      >
        {strokeAnimation && (
          <div
            style={{ cursor: "pointer" }}
            onClick={restartAnimation}
            key={hash}
            dangerouslySetInnerHTML={{ __html: strokeAnimation }}
          />
        )}
      </Animation>

      <Info>
        {kanjiInfo && joyoList?.includes(kanjiInfo.id) && (
          <p>
            <strong>Jōyō kanji</strong>
            {kanjiInfo?.jishoData?.taughtIn && (
              <span>
                , Taught in <strong>{kanjiInfo?.jishoData?.taughtIn}</strong>
              </span>
            )}
          </p>
        )}

        {kanjiInfo && jinmeiyoList?.includes(kanjiInfo.id) && (
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
              Meaning: <strong>{kanjiInfo?.jishoData?.meaning}</strong>
            </p>
          </>
        )}
        {kanjiInfo?.jishoData?.kunyomi && (
          <>
            <p>
              Kunyomi:{" "}
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
              {graphData.noOutLinks.links.filter(
                (link: any) => link.target === kanjiInfo?.id
              ).length > 0 && "Composition: "}
              {graphData.noOutLinks.links
                .filter((link: any) => link.target === kanjiInfo?.id)
                .map((link: any) => link.source)
                .map((comp: any, index: number) => (
                  <span key={index}>{comp} </span>
                ))}
            </p>
          </>
        )}
      </Info>
    </KanjiWrapper>
  );
};

export default Kanji;

const KanjiWrapper = styled.div`
  position: relative;
  width: 100%;
  height: 100%;
  padding: 16px;
  overflow: hidden;
  display: grid;
  grid-template-areas:
    "title info"
    "main info "
    "anim info ";
  grid-template-rows: 36px 1fr 1fr;
  grid-template-columns: 100px 1fr;
  grid-column-gap: 10px;
  border-left: 1px solid var(--color-lighter);
  border-right: 1px solid var(--color-lighter);

  @media (max-width: 767px) {
    border: none;
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
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  width: 100%;
  height: 100%;
  overflow: hidden;
  svg {
    width: 80px;
    height: 80px;
  }
  svg path[id] {
    fill: var(--color-lighter) !important;
  }
  svg path[clip-path] {
    stroke: var(--color-foreground) !important;
  }

  @media (max-width: 767px) {
    justify-content: flex-start;
  }
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
