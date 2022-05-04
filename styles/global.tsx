/** @jsxImportSource @emotion/react */
// import { jsx } from "@emotion/react";
import React from "react";
import { css, Global } from "@emotion/react";
import emotionNormalize from "emotion-normalize";

export const globalStyles = (
  <Global
    styles={css`
      ${emotionNormalize}

      *, *::before, *::after {
        box-sizing: border-box;
      }

      * {
        margin: 0;
      }
      ::-moz-selection {
        color: white;
        background: var(--color-primary);
      }
      ::selection {
        color: white;
        background: var(--color-primary);
      }

      html,
      body,
      #__next,
      #___gatsby {
        width: 100vw;
        height: 100vh;
        height: -webkit-fill-available;
        overflow: hidden;
      }

      body {
        line-height: 1.5;
        background: white;
        -webkit-font-smoothing: antialiased;
        margin: 0;
        padding: 0;
        font-family: -apple-system, BlinkMacSystemFont, avenir next, avenir,
          segoe ui, helvetica neue, helvetica, Ubuntu, roboto, noto, arial,
          sans-serif;
        -webkit-font-smoothing: antialiased;
        overflow: hidden;
      }

      img,
      picture,
      video,
      canvas,
      svg {
        display: block;
      }

      input,
      button,
      textarea,
      select {
        font: inherit;
      }

      p,
      h1,
      h2,
      h3,
      h4,
      h5,
      h6 {
        overflow-wrap: break-word;
      }

      #root,
      #__next,
      #___gatsby {
        isolation: isolate;
      }
    `}
  />
);
