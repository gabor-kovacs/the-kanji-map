import * as React from "react";
import { Header } from "@/components/header";
import Image from "next/image";
import { ScrollArea } from "@/components/ui/scroll-area";
import { Metadata } from "next";

export const metadata: Metadata = {
  title: "About",
};

const About = () => {
  return (
    <>
      <div className="relative h-screen grid grid-rows-[50px_1fr]">
        <Header route="about" />
        <ScrollArea className="w-full">
          <div className="p-4 max-w-2xl mx-auto mb-8">
            <h1 className="text-3xl font-extrabold tracking-tight mt-4 mb-4">
              About
            </h1>
            <p>
              The Kanji Map is a Japanese language learning tool that shows
              kanji information and decomposition in graph form.
            </p>
            <h1 className="text-3xl font-extrabold tracking-tight mt-8 mb-4">
              How to use this site
            </h1>
            <p>
              Kanji are represented with nodes and the connection between them
              with edges in a 2D or 3D force-directed graph. Click/tap on
              visible nodes or use the search field to change the selected node.
              If connected nodes have the same onyomi it is displayed over the
              link. Nodes are colored based on type:{" "}
              <svg className="inline h-[12px]" viewBox="0 0 100 100">
                <circle fill="black" cx={50} cy={50} r={50} />
                <circle fill="#2b99cf" cx={50} cy={50} r={40} />
              </svg>{" "}
              currently selected kanji,{" "}
              <svg className="inline h-[12px]" viewBox="0 0 100 100">
                <circle fill="black" cx={50} cy={50} r={50} />
                <circle fill="#80c2e2" cx={50} cy={50} r={40} />
              </svg>{" "}
              jōyō kanji,{" "}
              <svg className="inline h-[12px]" viewBox="0 0 100 100">
                <circle fill="black" cx={50} cy={50} r={50} />
                <circle fill="#d5ebf5" cx={50} cy={50} r={40} />
              </svg>{" "}
              jinmeiyō kanji,{" "}
              <svg className="inline h-[12px]" viewBox="0 0 100 100">
                <circle fill="black" cx={50} cy={50} r={50} />
                <circle fill="#fff" cx={50} cy={50} r={40} />
              </svg>{" "}
              neither.
            </p>
            <p>Displayed kanji information (where available):</p>
            <ul className="list-disc ml-6">
              <li>
                Type: jōyō kanji (taught in school), jinmeiyō kanji (used in
                names) or neither
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
            <h1 className="text-3xl font-extrabold tracking-tight mt-8 mb-4">
              Changelog
            </h1>
            <div className="space-y-4">
              <div>
                <div>
                  <span className="font-mono">2025-10-15</span>
                  {" - "}
                  <span className="font-semibold">Version 6.0.0</span>
                </div>
                <ul className="list-disc ml-6">
                  <li>
                    Added to go script to make kangxi/cjk compatibility radicals
                    consistent, thanks to{" "}
                    <a
                      href="https://github.com/mochi-co/equivalent-unified-ideograph"
                      target="_blank"
                      rel="noreferrer"
                      className="text-primary hover:underline"
                    >
                      mochi-co/equivalent-unified-ideograph
                    </a>
                    , avoiding issues like 忄 and ⺖ being mixed up as they are
                    different unicode characters
                  </li>
                  <li>
                    Reworked the data composition pipeline, radical alternative
                    forms now have a link from the original form
                  </li>
                  <li>updated the data</li>
                  <li>
                    fixed a bug where kanji with no stroke animation were not
                    displayed
                  </li>
                  <li>
                    fixed radicals and added alternative forms with position
                    information
                  </li>
                  <li>
                    added a custom font to display radicals, thanks to{" "}
                    <a
                      href="https://github.com/KanjiVG/kanjivg"
                      target="_blank"
                      rel="noreferrer"
                      className="text-primary inline-block hover:underline"
                    >
                      KanjiVG
                    </a>
                  </li>
                </ul>
              </div>
            </div>
            <h1 className="text-3xl font-extrabold tracking-tight mt-8 mb-4">
              Credits
            </h1>
            <ul className="list-disc ml-6">
              <li>
                Kanji and decomposition is based on{" "}
                <a
                  href="https://github.com/KanjiVG/kanjivg"
                  target="_blank"
                  rel="noreferrer"
                  className="text-primary inline-block hover:underline"
                >
                  KanjiVG
                </a>
                , released under the Creative Commons Attribution-Share Alike
                3.0 licence.
              </li>
              <li>
                Stroke animations are provided by{" "}
                <a
                  target="_blank"
                  href="https://github.com/parsimonhi/animCJK"
                  rel="noreferrer"
                  className="text-primary inline-block hover:underline"
                >
                  animCJK
                </a>{" "}
                under the Arphic Public License.
              </li>
              <li>
                Kanji, examples and radical information is provided by{" "}
                <a
                  target="_blank"
                  href="https://jisho.org"
                  rel="noreferrer"
                  className="text-primary inline-block hover:underline"
                >
                  jisho.org
                </a>{" "}
                sourcing from multiple open source{" "}
                <a
                  target="_blank"
                  href="https://jisho.org/about"
                  rel="noreferrer"
                  className="text-primary inline-block hover:underline"
                >
                  dictionaries
                </a>{" "}
                and{" "}
                <a
                  target="_blank"
                  href="https://kanjialive.com/"
                  rel="noreferrer"
                  className="text-primary inline-block hover:underline"
                >
                  Kanji alive
                </a>{" "}
                released under CC 4.0.
              </li>
              <li>
                Graph is created by{" "}
                <a
                  target="_blank"
                  href="https://github.com/vasturiano/react-force-graph"
                  rel="noreferrer"
                  className="text-primary inline-block hover:underline"
                >
                  react-force-graph
                </a>{" "}
                and{" "}
                <a
                  target="_blank"
                  href="https://github.com/vasturiano/three-spritetext"
                  rel="noreferrer"
                  className="text-primary inline-block hover:underline"
                >
                  three-spritetext
                </a>{" "}
                released under MIT.
              </li>
              <li>
                Hand written kanji recognition uses{" "}
                <a
                  target="_blank"
                  href="https://github.com/ChenYuHo/handwriting.js"
                  rel="noreferrer"
                  className="text-primary inline-block hover:underline"
                >
                  handwriting.js
                </a>{" "}
                released under MIT.
              </li>
            </ul>
            <h1 className="text-3xl font-extrabold tracking-tight mt-8 mb-4">
              Github repository
            </h1>
            <p>
              The source code can be found at{" "}
              <a
                target="_blank"
                href="https://github.com/gabor-kovacs/the-kanji-map"
                rel="noreferrer"
                className="text-primary inline-block hover:underline"
              >
                github.com/gabor-kovacs/the-kanji-map
              </a>
            </p>
            <h1 className="text-3xl font-extrabold tracking-tight mt-8 mb-4">
              Donations
            </h1>
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
              If this project was useful for you and you would like to
              contribute back, you can always{" "}
              <a
                href="https://www.paypal.com/donate?hosted_button_id=U867B8RRZUN7E"
                target="_blank"
                rel="noreferrer"
                className="text-primary inline-block hover:underline"
              >
                Donate!
              </a>
            </p>
            <p>
              Donations are used to pay for hosting, maintenance costs and
              improvements.
            </p>
            <h1 className="text-3xl font-extrabold tracking-tight mt-8 mb-8">
              Copyright
            </h1>
            <p>
              The Kanji Map is an open-source project (2017-
              {new Date().getFullYear()}) maintained under the MIT license. Feel
              free to fork, contribute, or share it with other learners.
            </p>
          </div>
        </ScrollArea>
      </div>
    </>
  );
};

export default About;
