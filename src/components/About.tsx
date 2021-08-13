import styled from "styled-components/macro";

export const About: React.FC = () => {
  return (
    <>
      <AboutWrapper>
        <h3>Giving Back</h3>
        <a
          href="https://www.paypal.com/donate?hosted_button_id=U867B8RRZUN7E"
          target="_blank"
          rel="noreferrer"
        >
          <img
            alt="Donate"
            src="https://www.paypalobjects.com/en_US/i/btn/btn_donate_SM.gif"
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
        <h3>How to use this site</h3>
        <p>
          Kanji are represented with nodes and the connection between them with
          edges in a 3D force-directed graph. Click/tap on visible nodes or use
          the search field to change the selected node. If connected nodes have
          the same onyomi it is displayed over the link. Nodes are colored based
          on type:{" "}
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
        <h3>Credits</h3>
        <p>
          The Kanji Map uses the Ideographic Description Sequence found in the{" "}
          <StyledLink
            href="http://www.chise.org/ids/index.html"
            target="_blank"
            rel="noreferrer"
          >
            CHISE project
          </StyledLink>
          . The CHISE-IDS database is published under the GNU General Public
          License.
        </p>

        <p>
          Non-standard/unicode kanji are displayed as images and imported from{" "}
          <StyledLink
            href="https://glyphwiki.org/"
            target="_blank"
            rel="noreferrer"
          >
            GlyphWiki
          </StyledLink>
          .
        </p>

        <p>
          Stroke animations are provided by{" "}
          <StyledLink
            target="_blank"
            href="https://github.com/parsimonhi/animCJK"
            rel="noreferrer"
          >
            animCJK
          </StyledLink>{" "}
          under the Arphic Public License.
        </p>
        <p>
          Kanji, examples and radical information is provided by{" "}
          <StyledLink target="_blank" href="https://jisho.org" rel="noreferrer">
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
        </p>
        <p>
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
        </p>
        <p>
          Hand written kanji recognition uses{" "}
          <StyledLink
            target="_blank"
            href="https://github.com/ChenYuHo/handwriting.js"
            rel="noreferrer"
          >
            handwriting.js
          </StyledLink>{" "}
          released under MIT.
        </p>

        <h3>Github repository</h3>
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

// * STYLES **************************************************************************************************

const AboutWrapper = styled.div`
  max-width: 1320px;
  margin: 0 auto;
  height: calc(100% - 50px);
  padding: 16px;
  overflow: auto;
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
