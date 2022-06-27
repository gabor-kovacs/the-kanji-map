import * as React from "react";
import styled from "@emotion/styled";
import Head from "next/head";
import Header from "../components/header";
import Image from "next/image";
import { NextSeo } from "next-seo";

const About: React.FC = () => {
  return (
    <>
      <NextSeo
        title="About | The Kanji Map"
        description="The Kanji Map is a Japanese language learning tool that shows kanji information and decomposition in graph form."
      />
      <Header />
      <AboutWrapper>
        <h1>About</h1>
        <p>
          The Kanji Map is a Japanese language learning tool that shows kanji
          information and decomposition in graph form.
        </p>
        <h1>Giving Back</h1>
        <a
          href="https://www.paypal.com/donate?hosted_button_id=U867B8RRZUN7E"
          target="_blank"
          rel="noreferrer"
        >
          <Image
            alt="Donate"
            width={74}
            height={21}
            src={"/btn_donate_SM.gif"}
          />
        </a>
        <p>
          If this project was useful for you and you would like to contribute
          back, you can always{" "}
          <StyledLink
            href="https://www.paypal.com/donate?hosted_button_id=U867B8RRZUN7E"
            target="_blank"
            rel="noreferrer"
          >
            Donate!
          </StyledLink>
        </p>
        <p>
          Donations are used to pay for hosting, maintenance costs and
          improvements.
        </p>
        <h1>How to use this site</h1>
        <p>
          Kanji are represented with nodes and the connection between them with
          edges in a 2D or 3D force-directed graph. Click/tap on visible nodes
          or use the search field to change the selected node. If connected
          nodes have the same onyomi it is displayed over the link. Nodes are
          colored based on type:{" "}
          <svg
            style={{ display: "inline", height: " 12px" }}
            viewBox="0 0 100 100"
          >
            <circle fill="#black" cx={50} cy={50} r={50} />
            <circle fill="#2b99cf" cx={50} cy={50} r={40} />
          </svg>{" "}
          currently selected kanji,{" "}
          <svg
            style={{ display: "inline", height: " 12px" }}
            viewBox="0 0 100 100"
          >
            <circle fill="#black" cx={50} cy={50} r={50} />
            <circle fill="#80c2e2" cx={50} cy={50} r={40} />
          </svg>{" "}
          jōyō kanji,{" "}
          <svg
            style={{ display: "inline", height: " 12px" }}
            viewBox="0 0 100 100"
          >
            <circle fill="#black" cx={50} cy={50} r={50} />
            <circle fill="#d5ebf5" cx={50} cy={50} r={40} />
          </svg>{" "}
          jinmeiyō kanji,{" "}
          <svg
            style={{ display: "inline", height: " 12px" }}
            viewBox="0 0 100 100"
          >
            <circle fill="#black" cx={50} cy={50} r={50} />
            <circle fill="#fff" cx={50} cy={50} r={40} />
          </svg>{" "}
          neither.
        </p>
        <p>Displayed kanji information (where available):</p>
        <ul>
          <li>
            Type: jōyō kanji (taught in school), jinmeiyō kanji (used in names)
            or neither
          </li>
          <li>JLPT (Japanese-Language Proficiency) Test level</li>
          <li>
            Frequency rank out of 2500 most used kanji found in newspapers
          </li>
          <li>Stroke count</li>
          <li>Meaning</li>
          <li>Kunyomi (Japanese reading of the kanji)</li>
          <li>Onnyomi (Chinese/Sino-Japanese reading of the kanji)</li>
          <li>Examples with audio, kunyomi and onyomi</li>
          <li>Radical with kunyomi and meaning</li>
        </ul>
        <h1>Credits</h1>
        <ul>
          <li>
            Kanji and decomposition is based on{" "}
            <StyledLink
              href="https://github.com/KanjiVG/kanjivg"
              target="_blank"
              rel="noreferrer"
            >
              KanjiVG
            </StyledLink>
            , released under the Creative Commons Attribution-Share Alike 3.0
            licence.
          </li>
          {/* <li>
            List of radicals are provided by{" "}
            <StyledLink
              href="https://github.com/sylhare/kanji"
              target="_blank"
              rel="noreferrer"
            >
              github.com/sylhare/kanji
            </StyledLink>
            , licensed under MIT.
          </li> */}
          <li>
            Stroke animations are provided by{" "}
            <StyledLink
              target="_blank"
              href="https://github.com/parsimonhi/animCJK"
              rel="noreferrer"
            >
              animCJK
            </StyledLink>{" "}
            under the Arphic Public License.
          </li>
          <li>
            Kanji, examples and radical information is provided by{" "}
            <StyledLink
              target="_blank"
              href="https://jisho.org"
              rel="noreferrer"
            >
              jisho.org
            </StyledLink>{" "}
            sourcing from multiple open source{" "}
            <StyledLink
              target="_blank"
              href="https://jisho.org/about "
              rel="noreferrer"
            >
              dicionaries
            </StyledLink>{" "}
            and{" "}
            <StyledLink
              target="_blank"
              href="https://kanjialive.com/"
              rel="noreferrer"
            >
              Kanji alive
            </StyledLink>{" "}
            released under CC 4.0.
          </li>
          <li>
            Graph is created by{" "}
            <StyledLink
              target="_blank"
              href="https://github.com/vasturiano/react-force-graph"
              rel="noreferrer"
            >
              react-force-graph
            </StyledLink>{" "}
            and{" "}
            <StyledLink
              target="_blank"
              href="https://github.com/vasturiano/three-spritetext"
              rel="noreferrer"
            >
              three-spritetext
            </StyledLink>{" "}
            released under MIT.
          </li>
          <li>
            Hand written kanji recognition uses{" "}
            <StyledLink
              target="_blank"
              href="https://github.com/ChenYuHo/handwriting.js"
              rel="noreferrer"
            >
              handwriting.js
            </StyledLink>{" "}
            released under MIT.
          </li>
        </ul>
        <h1>Github repository</h1>
        <p>
          The source code can be found at{" "}
          <StyledLink
            target="_blank"
            href="https://github.com/gabor-kovacs/the-kanji-map"
            rel="noreferrer"
          >
            github.com/gabor-kovacs/the-kanji-map
          </StyledLink>
        </p>
        <h1>Copyright</h1>
        <p>
          ©The Kanji Map 2017-{new Date().getFullYear()} by{" "}
          <StyledLink
            target="_blank"
            href="https://drgaborkovacs.com/index_en.html"
            rel="noreferrer"
          >
            Gabor Kovacs
          </StyledLink>{" "}
          - released under the MIT license.
        </p>
      </AboutWrapper>
    </>
  );
};

export default About;

// * STYLES **************************************************************************************************

const AboutWrapper = styled.div`
  max-width: 1320px;
  margin: 0 auto;
  height: calc(100% - 48px);
  padding: 16px;
  overflow: auto;
  padding-bottom: 32px;
`;

const StyledLink = styled.a`
  text-decoration: none;
  color: #2b99cf;
  display: inline-block;

  &:after {
    content: "";
    width: 0px;
    height: 1px;
    display: block;
    background: #2b99cf;
    transition: 200ms;
  }

  &:hover::after {
    width: 100%;
  }
`;
