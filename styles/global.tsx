import * as React from "react";
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
        background-color: var(--color-primary);
      }
      ::selection {
        color: white;
        background-color: var(--color-primary);
      }

      :root {
        --color-background: #ffffff;
        --color-foreground: #1f1f1f;
        --color-primary: #2b99cf;
        --color-danger: #f44336;
        --color-light: #555;
        --color-lighter: #eee;
      }
      [data-theme="dark"] {
        --color-background: #1f1f1f;
        --color-foreground: #ffffff;
        --color-light: #aaa;
        --color-lighter: #333;
      }

      html,
      body,
      #__next,
      #___gatsby {
        background-color: var(--color-background);
        color: var(--color-foreground);
        position: relative;
        height: 100%;
      }

      body {
        line-height: 1.5;
        -webkit-font-smoothing: antialiased;
        margin: 0;
        padding: 0;
        font-family: Roboto, -apple-system, BlinkMacSystemFont, avenir next,
          avenir, segoe ui, helvetica neue, helvetica, Ubuntu, roboto, noto,
          arial, sans-serif;
        -webkit-font-smoothing: antialiased;
        overflow: hidden;
      }

      body {
        min-height: 100vh;
        min-height: -webkit-fill-available;
      }
      html {
        height: -webkit-fill-available;
        width: 100vw;
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

      h1,
      h2,
      h3,
      h4,
      h5,
      h6,
      b,
      strong {
        font-weight: 500;
      }

      #root,
      #__next,
      #___gatsby {
        isolation: isolate;
      }
    `}
  />
);
