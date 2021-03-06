import * as React from "react";
import { css, Global } from "@emotion/react";
import emotionNormalize from "emotion-normalize";
import styled from "@emotion/styled";

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

export const Wrapper = styled.div`
  position: relative;
  height: 100vh;
  height: -webkit-fill-available;
  height: -moz-fill-available;
  height: fill-available;
  overflow: hidden;
  display: grid;
  grid-template-rows: 50px 1fr;
  @media (max-width: 767px) {
    grid-template-rows: 50px 1fr 50px;
  }
`;

export const Main = styled.main`
  width: 100%;
  height: 100%;
  overflow: hidden;
  display: grid;
  grid-template-columns: 1fr;
  grid-template-rows: 330px 1fr;

  @media (max-width: 767px) {
    overflow: auto;
    grid-template-rows: 1fr;
    & > div {
      position: relative;
      height: 100%;
      & > div {
        position: relative;
        height: 100%;
      }
    }
  }
`;

export const Top = styled.div`
  display: grid;
  grid-template-columns: 252px 1fr 1fr;
  overflow: hidden;
  border-bottom: 1px solid var(--color-lighter);
`;

export const Bottom = styled.div`
  display: grid;
  grid-template-columns: 2fr 3fr;
  overflow: hidden;
`;

export const SearchWrapper = styled.div`
  position: relative;
  padding: 16px;

  & > div:first-of-type {
    margin-bottom: 10px;
  }
`;

// * Mobile
export const Controls = styled.div`
  border-top: 1px solid var(--color-lighter);
  border-bottom: 1px solid var(--color-lighter);
  height: 50px;

  & .Mui-selected {
    color: var(--color-primary) !important;
  }
  & .MuiTabs-indicator {
    background-color: var(--color-primary) !important;
  }

  button {
    min-width: 50px;
  }

  @media (max-width: 767px) {
    position: absolute;
    bottom: 0;
    left: 0;
    right: 0;
    height: 50px;
  }
`;
