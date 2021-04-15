import React, { useState, useEffect } from 'react';
import ReactDOM from 'react-dom';

import App from './components/App';
import About from './components/About';
import Kanjis from './components/Kanjis';
import Header from './components/Header';

import Main from './components/Main';

import { createGlobalStyle } from 'styled-components';
import { normalize } from 'styled-normalize';

import { BrowserRouter as Router, Route, Switch } from 'react-router-dom';

const GlobalStyle = createGlobalStyle`
  ${normalize}

  *, *::before, *::after {
    box-sizing: border-box;
  }

  body {
    /* position: fixed; */
    width: 100vw;
    height: 100vh;
    height: -webkit-fill-available;
    overflow: hidden;
  } 

  html {
    height: -webkit-fill-available;
  }

  html, body {
    margin: 0;
    padding: 0;
    // SANS SERIF
    font-family: -apple-system, BlinkMacSystemFont, avenir next, avenir, segoe ui, helvetica neue, helvetica, Ubuntu, roboto, noto, arial, sans-serif;
   // SERIF
   /* font-family: Iowan Old Style, Apple Garamond, Baskerville, Times New Roman, Droid Serif, Times, Source Serif Pro, serif, Apple Color Emoji, Segoe UI Emoji, Segoe UI Symbol; */
   -webkit-font-smoothing: antialiased;
    overflow: hidden;
  }

#root {
    width: 100%;
    height: 100vh;
    height: -webkit-fill-available;
}

/* canvas {
  width: 100% !important;
} */

`;

ReactDOM.render(
	<React.Fragment>
		<GlobalStyle />
		<Main />
	</React.Fragment>,
	document.getElementById('root')
);