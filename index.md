---
title       : Reproducible Pitch
subtitle    : Production for Coursera assignement (Course 9 - Week 4)
author      : Idriss . S
job         : 
framework   : revealjs        # {io2012, html5slides, shower, dzslides, ...}
revealjs    : {theme: moon, transition: cube}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : []            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
ext_widgets : {rCharts: [libraries/nvd3]}
---


<style>
@import url(https://fonts.googleapis.com/css?family=Lato:400,700,400italic,700italic);
/**
 * Solarized Dark theme for reveal.js.
 * Author: Achim Staebler
 */
@font-face {
  font-family: 'League Gothic';
  src: url("../../lib/font/league_gothic-webfont.eot");
  src: url("../../lib/font/league_gothic-webfont.eot?#iefix") format("embedded-opentype"), url("../../lib/font/league_gothic-webfont.woff") format("woff"), url("../../lib/font/league_gothic-webfont.ttf") format("truetype"), url("../../lib/font/league_gothic-webfont.svg#LeagueGothicRegular") format("svg");
  font-weight: normal;
  font-style: normal; }

/**
 * Solarized colors by Ethan Schoonover
 */
html * {
  color-profile: sRGB;
  rendering-intent: auto; }

/*********************************************
 * GLOBAL STYLES
 *********************************************/
body {
  background: #002b36;
  background-color: #002b36; }

.reveal {
  font-family: "Lato", sans-serif;
  font-size: 36px;
  font-weight: 200;
  letter-spacing: -0.02em;
  color: #93a1a1; }

::selection {
  color: white;
  background: #d33682;
  text-shadow: none; }

/*********************************************
 * HEADERS
 *********************************************/
.reveal h1,
.reveal h2,
.reveal h3,
.reveal h4,
.reveal h5,
.reveal h6 {
  margin: 0 0 20px 0;
  color: #eee8d5;
  font-family: "League Gothic", Impact, sans-serif;
  line-height: 0.9em;
  letter-spacing: 0.02em;
  text-transform: uppercase;
  text-shadow: none; }

.reveal h1 {
  text-shadow: 0px 0px 6px rgba(0, 0, 0, 0.2); }

/*********************************************
 * LINKS
 *********************************************/
.reveal a:not(.image) {
  color: #268bd2;
  text-decoration: none;
  -webkit-transition: color .15s ease;
  -moz-transition: color .15s ease;
  -ms-transition: color .15s ease;
  -o-transition: color .15s ease;
  transition: color .15s ease; }

.reveal a:not(.image):hover {
  color: #78b9e6;
  text-shadow: none;
  border: none; }

.reveal .roll span:after {
  color: #fff;
  background: #1a6091; }

/*********************************************
 * IMAGES
 *********************************************/
.reveal section img {
  margin: 15px 0px;
  background: rgba(255, 255, 255, 0.12);
  border: 4px solid #93a1a1;
  box-shadow: 0 0 10px rgba(0, 0, 0, 0.15);
  -webkit-transition: all .2s linear;
  -moz-transition: all .2s linear;
  -ms-transition: all .2s linear;
  -o-transition: all .2s linear;
  transition: all .2s linear; }

.reveal a:hover img {
  background: rgba(255, 255, 255, 0.2);
  border-color: #268bd2;
  box-shadow: 0 0 20px rgba(0, 0, 0, 0.55); }

/*********************************************
 * NAVIGATION CONTROLS
 *********************************************/
.reveal .controls div.navigate-left,
.reveal .controls div.navigate-left.enabled {
  border-right-color: #268bd2; }

.reveal .controls div.navigate-right,
.reveal .controls div.navigate-right.enabled {
  border-left-color: #268bd2; }

.reveal .controls div.navigate-up,
.reveal .controls div.navigate-up.enabled {
  border-bottom-color: #268bd2; }

.reveal .controls div.navigate-down,
.reveal .controls div.navigate-down.enabled {
  border-top-color: #268bd2; }

.reveal .controls div.navigate-left.enabled:hover {
  border-right-color: #78b9e6; }

.reveal .controls div.navigate-right.enabled:hover {
  border-left-color: #78b9e6; }

.reveal .controls div.navigate-up.enabled:hover {
  border-bottom-color: #78b9e6; }

.reveal .controls div.navigate-down.enabled:hover {
  border-top-color: #78b9e6; }

/*********************************************
 * PROGRESS BAR
 *********************************************/
.reveal .progress {
  background: rgba(0, 0, 0, 0.2); }

.reveal .progress span {
  background: #268bd2;
  -webkit-transition: width 800ms cubic-bezier(0.26, 0.86, 0.44, 0.985);
  -moz-transition: width 800ms cubic-bezier(0.26, 0.86, 0.44, 0.985);
  -ms-transition: width 800ms cubic-bezier(0.26, 0.86, 0.44, 0.985);
  -o-transition: width 800ms cubic-bezier(0.26, 0.86, 0.44, 0.985);
  transition: width 800ms cubic-bezier(0.26, 0.86, 0.44, 0.985); }

@charset "UTF-8";
/*!
 * reveal.js
 * http://lab.hakim.se/reveal-js
 * MIT licensed
 *
 * Copyright (C) 2013 Hakim El Hattab, http://hakim.se
 */
html, body, .reveal div, .reveal span, .reveal applet, .reveal object, .reveal iframe, .reveal h1, .reveal h2, .reveal h3, .reveal h4, .reveal h5, .reveal h6, .reveal p, .reveal blockquote, .reveal pre, .reveal a, .reveal abbr, .reveal acronym, .reveal address, .reveal big, .reveal cite, .reveal code, .reveal del, .reveal dfn, .reveal em, .reveal img, .reveal ins, .reveal kbd, .reveal q, .reveal s, .reveal samp, .reveal small, .reveal strike, .reveal strong, .reveal sub, .reveal sup, .reveal tt, .reveal var, .reveal b, .reveal u, .reveal i, .reveal center, .reveal dl, .reveal dt, .reveal dd, .reveal ol, .reveal ul, .reveal li, .reveal fieldset, .reveal form, .reveal label, .reveal legend, .reveal table, .reveal caption, .reveal tbody, .reveal tfoot, .reveal thead, .reveal tr, .reveal th, .reveal td, .reveal article, .reveal aside, .reveal canvas, .reveal details, .reveal embed, .reveal figure, .reveal figcaption, .reveal footer, .reveal header, .reveal hgroup, .reveal menu, .reveal nav, .reveal output, .reveal ruby, .reveal section, .reveal summary, .reveal time, .reveal mark, .reveal audio, video {
    margin: 0;
    padding: 0;
    border: 0;
    font-size: 100%;
    font: inherit;
    vertical-align:baseline
}

.reveal article, .reveal aside, .reveal details, .reveal figcaption, .reveal figure, .reveal footer, .reveal header, .reveal hgroup, .reveal menu, .reveal nav, .reveal section {
    display:block
}

html, body {
    width: 100%;
    height: 100%;
    overflow:hidden
}

body {
    position: relative;
    line-height:1
}

::selection {
    background: #FF5E99;
    color: #fff;
    text-shadow:none
}

.reveal h1, .reveal h2, .reveal h3, .reveal h4, .reveal h5, .reveal h6 {
    -webkit-hyphens: auto;
    -moz-hyphens: auto;
    hyphens: auto;
    word-wrap:break-word
}

.reveal h1 {
    font-size:3.77em
}

.reveal h2 {
    font-size:2.11em
}

.reveal h3 {
    font-size:1.55em
}

.reveal h4 {
    font-size:1em
}

.reveal .slides section .fragment {
    opacity: 0;
    -webkit-transition: all .2s ease;
    -moz-transition: all .2s ease;
    -ms-transition: all .2s ease;
    -o-transition: all .2s ease;
    transition:all .2s ease
}

.reveal .slides section .fragment.visible {
    opacity:1
}

.reveal .slides section .fragment.grow {
    opacity:1
}

.reveal .slides section .fragment.grow.visible {
    -webkit-transform: scale(1.3);
    -moz-transform: scale(1.3);
    -ms-transform: scale(1.3);
    -o-transform: scale(1.3);
    transform:scale(1.3)
}

.reveal .slides section .fragment.shrink {
    opacity:1
}

.reveal .slides section .fragment.shrink.visible {
    -webkit-transform: scale(0.7);
    -moz-transform: scale(0.7);
    -ms-transform: scale(0.7);
    -o-transform: scale(0.7);
    transform:scale(0.7)
}

.reveal .slides section .fragment.roll-in {
    opacity: 0;
    -webkit-transform: rotateX(90deg);
    -moz-transform: rotateX(90deg);
    -ms-transform: rotateX(90deg);
    -o-transform: rotateX(90deg);
    transform:rotateX(90deg)
}

.reveal .slides section .fragment.roll-in.visible {
    opacity: 1;
    -webkit-transform: rotateX(0);
    -moz-transform: rotateX(0);
    -ms-transform: rotateX(0);
    -o-transform: rotateX(0);
    transform:rotateX(0)
}

.reveal .slides section .fragment.fade-out {
    opacity:1
}

.reveal .slides section .fragment.fade-out.visible {
    opacity:0
}

.reveal .slides section .fragment.highlight-red, .reveal .slides section .fragment.highlight-green, .reveal .slides section .fragment.highlight-blue {
    opacity:1
}

.reveal .slides section .fragment.highlight-red.visible {
    color:#ff2c2d
}

.reveal .slides section .fragment.highlight-green.visible {
    color:#17ff2e
}

.reveal .slides section .fragment.highlight-blue.visible {
    color:#1b91ff
}

.reveal:after {
    content: '';
    font-style:italic
}

.reveal img, .reveal video, .reveal iframe {
    max-width: 95%;
    max-height:95%
}

.reveal a {
    position:relative
}

.reveal strong, .reveal b {
    font-weight:700
}

.reveal em, .reveal i {
    font-style:italic
}

.reveal ol, .reveal ul {
    display: inline-block;
    text-align: left;
    margin:0 0 0 1em
}

.reveal ol {
    list-style-type:decimal
}

.reveal ul {
    list-style-type:disc
}

.reveal ul ul {
    list-style-type:square
}

.reveal ul ul ul {
    list-style-type:circle
}

.reveal ul ul, .reveal ul ol, .reveal ol ol, .reveal ol ul {
    display: block;
    margin-left:40px
}

.reveal p {
    margin-bottom: 10px;
    line-height:1.2em
}

.reveal q, .reveal blockquote {
    quotes:none
}

.reveal blockquote {
    display: block;
    position: relative;
    width: 70%;
    margin: 5px auto;
    padding: 5px;
    font-style: italic;
    background: rgba(255, 255, 255, .05);
    box-shadow:0 0 2px rgba(0, 0, 0, .2)
}

.reveal blockquote p:first-child, .reveal blockquote p:last-child {
    display:inline-block
}

.reveal blockquote:before {
    content: '\201C'
}

.reveal blockquote:after {
    content: '\201D'
}

.reveal q {
    font-style:italic
}

.reveal q:before {
    content: '\201C'
}

.reveal q:after {
    content: '\201D'
}

.reveal pre {
    display: block;
    position: relative;
    width: 90%;
    margin: 15px auto;
    text-align: left;
    font-size: .55em;
    font-family: monospace;
    line-height: 1.2em;
    word-wrap: break-word;
    box-shadow:0 0 6px rgba(0, 0, 0, .3)
}

.reveal code {
    font-family:monospace
}

.reveal pre code {
    padding: 5px;
    overflow: auto;
    max-height: 400px;
    word-wrap:normal
}

.reveal table th, .reveal table td {
    text-align: left;
    padding-right:.3em
}

.reveal table th {
    text-shadow:#fff 1px 1px 2px
}

.reveal sup {
    vertical-align:super
}

.reveal sub {
    vertical-align:sub
}

.reveal small {
    display: inline-block;
    font-size: .6em;
    line-height: 1.2em;
    vertical-align:top
}

.reveal small * {
    vertical-align:top
}

.reveal .controls {
    display: none;
    position: fixed;
    width: 110px;
    height: 110px;
    z-index: 30;
    right: 10px;
    bottom:10px
}

.reveal .controls div {
    position: absolute;
    opacity: .1;
    width: 0;
    height: 0;
    border: 12px solid transparent;
    -webkit-transition: opacity .2s ease;
    -moz-transition: opacity .2s ease;
    -ms-transition: opacity .2s ease;
    -o-transition: opacity .2s ease;
    transition:opacity .2s ease
}

.reveal .controls div.enabled {
    opacity: .6;
    cursor:pointer
}

.reveal .controls div.enabled:active {
    margin-top:1px
}

.reveal .controls div.navigate-left {
    top: 42px;
    border-right-width: 22px;
    border-right-color:#eee
}

.reveal .controls div.navigate-right {
    left: 74px;
    top: 42px;
    border-left-width: 22px;
    border-left-color:#eee
}

.reveal .controls div.navigate-up {
    left: 42px;
    border-bottom-width: 22px;
    border-bottom-color:#eee
}

.reveal .controls div.navigate-down {
    left: 42px;
    top: 74px;
    border-top-width: 22px;
    border-top-color:#eee
}

.reveal .progress {
    position: fixed;
    display: none;
    height: 3px;
    width: 100%;
    bottom: 0;
    left: 0;
    z-index:10
}

.reveal .progress:after {
    content: '';
    display: 'block';
    position: absolute;
    height: 20px;
    width: 100%;
    top:-20px
}

.reveal .progress span {
    display: block;
    height: 100%;
    width: 0;
    -webkit-transition: width 800ms cubic-bezier(0.26, .86, .44, .985);
    -moz-transition: width 800ms cubic-bezier(0.26, .86, .44, .985);
    -ms-transition: width 800ms cubic-bezier(0.26, .86, .44, .985);
    -o-transition: width 800ms cubic-bezier(0.26, .86, .44, .985);
    transition:width 800ms cubic-bezier(0.26, .86, .44, .985)
}

.reveal .roll {
    display: inline-block;
    line-height: 1.2;
    overflow: hidden;
    vertical-align: top;
    -webkit-perspective: 400px;
    -moz-perspective: 400px;
    -ms-perspective: 400px;
    perspective: 400px;
    -webkit-perspective-origin: 50% 50%;
    -moz-perspective-origin: 50% 50%;
    -ms-perspective-origin: 50% 50%;
    perspective-origin:50% 50%
}

.reveal .roll:hover {
    background: 0;
    text-shadow:none
}

.reveal .roll span {
    display: block;
    position: relative;
    padding: 0 2px;
    pointer-events: none;
    -webkit-transition: all 400ms ease;
    -moz-transition: all 400ms ease;
    -ms-transition: all 400ms ease;
    transition: all 400ms ease;
    -webkit-transform-origin: 50% 0;
    -moz-transform-origin: 50% 0;
    -ms-transform-origin: 50% 0;
    transform-origin: 50% 0;
    -webkit-transform-style: preserve-3d;
    -moz-transform-style: preserve-3d;
    -ms-transform-style: preserve-3d;
    transform-style: preserve-3d;
    -webkit-backface-visibility: hidden;
    -moz-backface-visibility: hidden;
    backface-visibility:hidden
}

.reveal .roll:hover span {
    background: rgba(0, 0, 0, .5);
    -webkit-transform: translate3d(0px, 0, -45px) rotateX(90deg);
    -moz-transform: translate3d(0px, 0, -45px) rotateX(90deg);
    -ms-transform: translate3d(0px, 0, -45px) rotateX(90deg);
    transform:translate3d(0px, 0, -45px) rotateX(90deg)
}

.reveal .roll span:after {
    content: attr(data-title);
    display: block;
    position: absolute;
    left: 0;
    top: 0;
    padding: 0 2px;
    -webkit-backface-visibility: hidden;
    -moz-backface-visibility: hidden;
    backface-visibility: hidden;
    -webkit-transform-origin: 50% 0;
    -moz-transform-origin: 50% 0;
    -ms-transform-origin: 50% 0;
    transform-origin: 50% 0;
    -webkit-transform: translate3d(0px, 110%, 0) rotateX(-90deg);
    -moz-transform: translate3d(0px, 110%, 0) rotateX(-90deg);
    -ms-transform: translate3d(0px, 110%, 0) rotateX(-90deg);
    transform:translate3d(0px, 110%, 0) rotateX(-90deg)
}

.reveal {
    position: relative;
    width: 100%;
    height:100%
}

.reveal .slides {
    position: absolute;
    width: 100%;
    height: 100%;
    left: 50%;
    top: 50%;
    overflow: visible;
    z-index: 1;
    text-align: center;
    -webkit-transition: -webkit-perspective .4s ease;
    -moz-transition: -moz-perspective .4s ease;
    -ms-transition: -ms-perspective .4s ease;
    -o-transition: -o-perspective .4s ease;
    transition: perspective .4s ease;
    -webkit-perspective: 600px;
    -moz-perspective: 600px;
    -ms-perspective: 600px;
    perspective: 600px;
    -webkit-perspective-origin: 0 -100px;
    -moz-perspective-origin: 0 -100px;
    -ms-perspective-origin: 0 -100px;
    perspective-origin:0 -100px
}

.reveal .slides > section, .reveal .slides > section > section {
    display: none;
    position: absolute;
    width: 100%;
    padding: 20px 0;
    z-index: 10;
    line-height: 1.2em;
    font-weight: 400;
    -webkit-transform-style: preserve-3d;
    -moz-transform-style: preserve-3d;
    -ms-transform-style: preserve-3d;
    transform-style: preserve-3d;
    -webkit-transition: -webkit-transform-origin 800ms cubic-bezier(0.26, .86, .44, .985), -webkit-transform 800ms cubic-bezier(0.26, .86, .44, .985), visibility 800ms cubic-bezier(0.26, .86, .44, .985), opacity 800ms cubic-bezier(0.26, .86, .44, .985);
    -moz-transition: -moz-transform-origin 800ms cubic-bezier(0.26, .86, .44, .985), -moz-transform 800ms cubic-bezier(0.26, .86, .44, .985), visibility 800ms cubic-bezier(0.26, .86, .44, .985), opacity 800ms cubic-bezier(0.26, .86, .44, .985);
    -ms-transition: -ms-transform-origin 800ms cubic-bezier(0.26, .86, .44, .985), -ms-transform 800ms cubic-bezier(0.26, .86, .44, .985), visibility 800ms cubic-bezier(0.26, .86, .44, .985), opacity 800ms cubic-bezier(0.26, .86, .44, .985);
    -o-transition: -o-transform-origin 800ms cubic-bezier(0.26, .86, .44, .985), -o-transform 800ms cubic-bezier(0.26, .86, .44, .985), visibility 800ms cubic-bezier(0.26, .86, .44, .985), opacity 800ms cubic-bezier(0.26, .86, .44, .985);
    transition:transform-origin 800ms cubic-bezier(0.26, .86, .44, .985), transform 800ms cubic-bezier(0.26, .86, .44, .985), visibility 800ms cubic-bezier(0.26, .86, .44, .985), opacity 800ms cubic-bezier(0.26, .86, .44, .985)
}

.reveal[data-transition-speed=fast] .slides section {
    -webkit-transition-duration: 400ms;
    -moz-transition-duration: 400ms;
    -ms-transition-duration: 400ms;
    transition-duration:400ms
}

.reveal[data-transition-speed=slow] .slides section {
    -webkit-transition-duration: 1200ms;
    -moz-transition-duration: 1200ms;
    -ms-transition-duration: 1200ms;
    transition-duration:1200ms
}

.reveal .slides section[data-transition-speed=fast] {
    -webkit-transition-duration: 400ms;
    -moz-transition-duration: 400ms;
    -ms-transition-duration: 400ms;
    transition-duration:400ms
}

.reveal .slides section[data-transition-speed=slow] {
    -webkit-transition-duration: 1200ms;
    -moz-transition-duration: 1200ms;
    -ms-transition-duration: 1200ms;
    transition-duration:1200ms
}

.reveal .slides > section {
    left: -50%;
    top:-50%
}

.reveal .slides > section.stack {
    padding-top: 0;
    padding-bottom:0
}

.reveal .slides > section.present, .reveal .slides > section > section.present {
    display: block;
    z-index: 11;
    opacity:1
}

.reveal.center, .reveal.center .slides, .reveal.center .slides section {
    min-height:auto !important
}

.reveal .slides > section[data-transition=default].past, .reveal .slides > section.past {
    display: block;
    opacity: 0;
    -webkit-transform: translate3d(-100%, 0, 0) rotateY(-90deg) translate3d(-100%, 0, 0);
    -moz-transform: translate3d(-100%, 0, 0) rotateY(-90deg) translate3d(-100%, 0, 0);
    -ms-transform: translate3d(-100%, 0, 0) rotateY(-90deg) translate3d(-100%, 0, 0);
    transform:translate3d(-100%, 0, 0) rotateY(-90deg) translate3d(-100%, 0, 0)
}

.reveal .slides > section[data-transition=default].future, .reveal .slides > section.future {
    display: block;
    opacity: 0;
    -webkit-transform: translate3d(100%, 0, 0) rotateY(90deg) translate3d(100%, 0, 0);
    -moz-transform: translate3d(100%, 0, 0) rotateY(90deg) translate3d(100%, 0, 0);
    -ms-transform: translate3d(100%, 0, 0) rotateY(90deg) translate3d(100%, 0, 0);
    transform:translate3d(100%, 0, 0) rotateY(90deg) translate3d(100%, 0, 0)
}

.reveal .slides > section > section[data-transition=default].past, .reveal .slides > section > section.past {
    display: block;
    opacity: 0;
    -webkit-transform: translate3d(0, -300px, 0) rotateX(70deg) translate3d(0, -300px, 0);
    -moz-transform: translate3d(0, -300px, 0) rotateX(70deg) translate3d(0, -300px, 0);
    -ms-transform: translate3d(0, -300px, 0) rotateX(70deg) translate3d(0, -300px, 0);
    transform:translate3d(0, -300px, 0) rotateX(70deg) translate3d(0, -300px, 0)
}

.reveal .slides > section > section[data-transition=default].future, .reveal .slides > section > section.future {
    display: block;
    opacity: 0;
    -webkit-transform: translate3d(0, 300px, 0) rotateX(-70deg) translate3d(0, 300px, 0);
    -moz-transform: translate3d(0, 300px, 0) rotateX(-70deg) translate3d(0, 300px, 0);
    -ms-transform: translate3d(0, 300px, 0) rotateX(-70deg) translate3d(0, 300px, 0);
    transform:translate3d(0, 300px, 0) rotateX(-70deg) translate3d(0, 300px, 0)
}

.reveal .slides > section[data-transition=concave].past, .reveal.concave .slides > section.past {
    -webkit-transform: translate3d(-100%, 0, 0) rotateY(90deg) translate3d(-100%, 0, 0);
    -moz-transform: translate3d(-100%, 0, 0) rotateY(90deg) translate3d(-100%, 0, 0);
    -ms-transform: translate3d(-100%, 0, 0) rotateY(90deg) translate3d(-100%, 0, 0);
    transform:translate3d(-100%, 0, 0) rotateY(90deg) translate3d(-100%, 0, 0)
}

.reveal .slides > section[data-transition=concave].future, .reveal.concave .slides > section.future {
    -webkit-transform: translate3d(100%, 0, 0) rotateY(-90deg) translate3d(100%, 0, 0);
    -moz-transform: translate3d(100%, 0, 0) rotateY(-90deg) translate3d(100%, 0, 0);
    -ms-transform: translate3d(100%, 0, 0) rotateY(-90deg) translate3d(100%, 0, 0);
    transform:translate3d(100%, 0, 0) rotateY(-90deg) translate3d(100%, 0, 0)
}

.reveal .slides > section > section[data-transition=concave].past, .reveal.concave .slides > section > section.past {
    -webkit-transform: translate3d(0, -80%, 0) rotateX(-70deg) translate3d(0, -80%, 0);
    -moz-transform: translate3d(0, -80%, 0) rotateX(-70deg) translate3d(0, -80%, 0);
    -ms-transform: translate3d(0, -80%, 0) rotateX(-70deg) translate3d(0, -80%, 0);
    transform:translate3d(0, -80%, 0) rotateX(-70deg) translate3d(0, -80%, 0)
}

.reveal .slides > section > section[data-transition=concave].future, .reveal.concave .slides > section > section.future {
    -webkit-transform: translate3d(0, 80%, 0) rotateX(70deg) translate3d(0, 80%, 0);
    -moz-transform: translate3d(0, 80%, 0) rotateX(70deg) translate3d(0, 80%, 0);
    -ms-transform: translate3d(0, 80%, 0) rotateX(70deg) translate3d(0, 80%, 0);
    transform:translate3d(0, 80%, 0) rotateX(70deg) translate3d(0, 80%, 0)
}

.reveal .slides > section[data-transition=zoom].past, .reveal.zoom .slides > section.past {
    opacity: 0;
    visibility: hidden;
    -webkit-transform: scale(16);
    -moz-transform: scale(16);
    -ms-transform: scale(16);
    -o-transform: scale(16);
    transform:scale(16)
}

.reveal .slides > section[data-transition=zoom].future, .reveal.zoom .slides > section.future {
    opacity: 0;
    visibility: hidden;
    -webkit-transform: scale(0.2);
    -moz-transform: scale(0.2);
    -ms-transform: scale(0.2);
    -o-transform: scale(0.2);
    transform:scale(0.2)
}

.reveal .slides > section > section[data-transition=zoom].past, .reveal.zoom .slides > section > section.past {
    -webkit-transform: translate(0, -150%);
    -moz-transform: translate(0, -150%);
    -ms-transform: translate(0, -150%);
    -o-transform: translate(0, -150%);
    transform:translate(0, -150%)
}

.reveal .slides > section > section[data-transition=zoom].future, .reveal.zoom .slides > section > section.future {
    -webkit-transform: translate(0, 150%);
    -moz-transform: translate(0, 150%);
    -ms-transform: translate(0, 150%);
    -o-transform: translate(0, 150%);
    transform:translate(0, 150%)
}

.reveal .slides > section[data-transition=linear].past, .reveal.linear .slides > section.past {
    -webkit-transform: translate(-150%, 0);
    -moz-transform: translate(-150%, 0);
    -ms-transform: translate(-150%, 0);
    -o-transform: translate(-150%, 0);
    transform:translate(-150%, 0)
}

.reveal .slides > section[data-transition=linear].future, .reveal.linear .slides > section.future {
    -webkit-transform: translate(150%, 0);
    -moz-transform: translate(150%, 0);
    -ms-transform: translate(150%, 0);
    -o-transform: translate(150%, 0);
    transform:translate(150%, 0)
}

.reveal .slides > section > section[data-transition=linear].past, .reveal.linear .slides > section > section.past {
    -webkit-transform: translate(0, -150%);
    -moz-transform: translate(0, -150%);
    -ms-transform: translate(0, -150%);
    -o-transform: translate(0, -150%);
    transform:translate(0, -150%)
}

.reveal .slides > section > section[data-transition=linear].future, .reveal.linear .slides > section > section.future {
    -webkit-transform: translate(0, 150%);
    -moz-transform: translate(0, 150%);
    -ms-transform: translate(0, 150%);
    -o-transform: translate(0, 150%);
    transform:translate(0, 150%)
}

.reveal.cube .slides {
    -webkit-perspective: 1300px;
    -moz-perspective: 1300px;
    -ms-perspective: 1300px;
    perspective:1300px
}

.reveal.cube .slides section {
    padding: 30px;
    min-height: 700px;
    -webkit-backface-visibility: hidden;
    -moz-backface-visibility: hidden;
    -ms-backface-visibility: hidden;
    backface-visibility: hidden;
    -webkit-box-sizing: border-box;
    -moz-box-sizing: border-box;
    box-sizing:border-box
}

.reveal.center.cube .slides section {
    min-height:auto
}

.reveal.cube .slides section:not(.stack):before {
    content: '';
    position: absolute;
    display: block;
    width: 100%;
    height: 100%;
    left: 0;
    top: 0;
    background: rgba(0, 0, 0, .1);
    border-radius: 4px;
    -webkit-transform: translateZ(-20px);
    -moz-transform: translateZ(-20px);
    -ms-transform: translateZ(-20px);
    -o-transform: translateZ(-20px);
    transform:translateZ(-20px)
}

.reveal.cube .slides section:not(.stack):after {
    content: '';
    position: absolute;
    display: block;
    width: 90%;
    height: 30px;
    left: 5%;
    bottom: 0;
    background: 0;
    z-index: 1;
    border-radius: 4px;
    box-shadow: 0 95px 25px rgba(0, 0, 0, .2);
    -webkit-transform: translateZ(-90px) rotateX(65deg);
    -moz-transform: translateZ(-90px) rotateX(65deg);
    -ms-transform: translateZ(-90px) rotateX(65deg);
    -o-transform: translateZ(-90px) rotateX(65deg);
    transform:translateZ(-90px) rotateX(65deg)
}

.reveal.cube .slides > section.stack {
    padding: 0;
    background:0
}

.reveal.cube .slides > section.past {
    -webkit-transform-origin: 100% 0;
    -moz-transform-origin: 100% 0;
    -ms-transform-origin: 100% 0;
    transform-origin: 100% 0;
    -webkit-transform: translate3d(-100%, 0, 0) rotateY(-90deg);
    -moz-transform: translate3d(-100%, 0, 0) rotateY(-90deg);
    -ms-transform: translate3d(-100%, 0, 0) rotateY(-90deg);
    transform:translate3d(-100%, 0, 0) rotateY(-90deg)
}

.reveal.cube .slides > section.future {
    -webkit-transform-origin: 0 0;
    -moz-transform-origin: 0 0;
    -ms-transform-origin: 0 0;
    transform-origin: 0 0;
    -webkit-transform: translate3d(100%, 0, 0) rotateY(90deg);
    -moz-transform: translate3d(100%, 0, 0) rotateY(90deg);
    -ms-transform: translate3d(100%, 0, 0) rotateY(90deg);
    transform:translate3d(100%, 0, 0) rotateY(90deg)
}

.reveal.cube .slides > section > section.past {
    -webkit-transform-origin: 0 100%;
    -moz-transform-origin: 0 100%;
    -ms-transform-origin: 0 100%;
    transform-origin: 0 100%;
    -webkit-transform: translate3d(0, -100%, 0) rotateX(90deg);
    -moz-transform: translate3d(0, -100%, 0) rotateX(90deg);
    -ms-transform: translate3d(0, -100%, 0) rotateX(90deg);
    transform:translate3d(0, -100%, 0) rotateX(90deg)
}

.reveal.cube .slides > section > section.future {
    -webkit-transform-origin: 0 0;
    -moz-transform-origin: 0 0;
    -ms-transform-origin: 0 0;
    transform-origin: 0 0;
    -webkit-transform: translate3d(0, 100%, 0) rotateX(-90deg);
    -moz-transform: translate3d(0, 100%, 0) rotateX(-90deg);
    -ms-transform: translate3d(0, 100%, 0) rotateX(-90deg);
    transform:translate3d(0, 100%, 0) rotateX(-90deg)
}

.reveal.page .slides {
    -webkit-perspective-origin: 0 50%;
    -moz-perspective-origin: 0 50%;
    -ms-perspective-origin: 0 50%;
    perspective-origin: 0 50%;
    -webkit-perspective: 3000px;
    -moz-perspective: 3000px;
    -ms-perspective: 3000px;
    perspective:3000px
}

.reveal.page .slides section {
    padding: 30px;
    min-height: 700px;
    -webkit-box-sizing: border-box;
    -moz-box-sizing: border-box;
    box-sizing:border-box
}

.reveal.page .slides section.past {
    z-index:12
}

.reveal.page .slides section:not(.stack):before {
    content: '';
    position: absolute;
    display: block;
    width: 100%;
    height: 100%;
    left: 0;
    top: 0;
    background: rgba(0, 0, 0, .1);
    -webkit-transform: translateZ(-20px);
    -moz-transform: translateZ(-20px);
    -ms-transform: translateZ(-20px);
    -o-transform: translateZ(-20px);
    transform:translateZ(-20px)
}

.reveal.page .slides section:not(.stack):after {
    content: '';
    position: absolute;
    display: block;
    width: 90%;
    height: 30px;
    left: 5%;
    bottom: 0;
    background: 0;
    z-index: 1;
    border-radius: 4px;
    box-shadow: 0 95px 25px rgba(0, 0, 0, .2);
    -webkit-transform:translateZ(-90px) rotateX(65deg)
}

.reveal.page .slides > section.stack {
    padding: 0;
    background:0
}

.reveal.page .slides > section.past {
    -webkit-transform-origin: 0 0;
    -moz-transform-origin: 0 0;
    -ms-transform-origin: 0 0;
    transform-origin: 0 0;
    -webkit-transform: translate3d(-40%, 0, 0) rotateY(-80deg);
    -moz-transform: translate3d(-40%, 0, 0) rotateY(-80deg);
    -ms-transform: translate3d(-40%, 0, 0) rotateY(-80deg);
    transform:translate3d(-40%, 0, 0) rotateY(-80deg)
}

.reveal.page .slides > section.future {
    -webkit-transform-origin: 100% 0;
    -moz-transform-origin: 100% 0;
    -ms-transform-origin: 100% 0;
    transform-origin: 100% 0;
    -webkit-transform: translate3d(0, 0, 0);
    -moz-transform: translate3d(0, 0, 0);
    -ms-transform: translate3d(0, 0, 0);
    transform:translate3d(0, 0, 0)
}

.reveal.page .slides > section > section.past {
    -webkit-transform-origin: 0 0;
    -moz-transform-origin: 0 0;
    -ms-transform-origin: 0 0;
    transform-origin: 0 0;
    -webkit-transform: translate3d(0, -40%, 0) rotateX(80deg);
    -moz-transform: translate3d(0, -40%, 0) rotateX(80deg);
    -ms-transform: translate3d(0, -40%, 0) rotateX(80deg);
    transform:translate3d(0, -40%, 0) rotateX(80deg)
}

.reveal.page .slides > section > section.future {
    -webkit-transform-origin: 0 100%;
    -moz-transform-origin: 0 100%;
    -ms-transform-origin: 0 100%;
    transform-origin: 0 100%;
    -webkit-transform: translate3d(0, 0, 0);
    -moz-transform: translate3d(0, 0, 0);
    -ms-transform: translate3d(0, 0, 0);
    transform:translate3d(0, 0, 0)
}

.reveal .slides section[data-transition=fade], .reveal.fade .slides section, .reveal.fade .slides > section > section {
    -webkit-transform: none;
    -moz-transform: none;
    -ms-transform: none;
    -o-transform: none;
    transform: none;
    -webkit-transition: opacity .5s;
    -moz-transition: opacity .5s;
    -ms-transition: opacity .5s;
    -o-transition: opacity .5s;
    transition:opacity .5s
}

.reveal.fade.overview .slides section, .reveal.fade.overview .slides > section > section, .reveal.fade.exit-overview .slides section, .reveal.fade.exit-overview .slides > section > section {
    -webkit-transition: none;
    -moz-transition: none;
    -ms-transition: none;
    -o-transition: none;
    transition:none
}

.reveal .slides section[data-transition=none], .reveal.none .slides section {
    -webkit-transform: none;
    -moz-transform: none;
    -ms-transform: none;
    -o-transform: none;
    transform: none;
    -webkit-transition: none;
    -moz-transition: none;
    -ms-transition: none;
    -o-transition: none;
    transition:none
}

.reveal.overview .slides {
    -webkit-perspective-origin: 0 0;
    -moz-perspective-origin: 0 0;
    -ms-perspective-origin: 0 0;
    perspective-origin: 0 0;
    -webkit-perspective: 700px;
    -moz-perspective: 700px;
    -ms-perspective: 700px;
    perspective:700px
}

.reveal.overview .slides section {
    height: 600px;
    overflow: hidden;
    opacity: 1 !important;
    visibility: visible !important;
    cursor: pointer;
    background:rgba(0, 0, 0, .1)
}

.reveal.overview .slides section .fragment {
    opacity:1
}

.reveal.overview .slides section:after, .reveal.overview .slides section:before {
    display:none !important
}

.reveal.overview .slides section > section {
    opacity: 1;
    cursor:pointer
}

.reveal.overview .slides section:hover {
    background:rgba(0, 0, 0, .3)
}

.reveal.overview .slides section.present {
    background:rgba(0, 0, 0, .3)
}

.reveal.overview .slides > section.stack {
    padding: 0;
    background: 0;
    overflow:visible
}

.reveal .pause-overlay {
    position: absolute;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    background: #000;
    visibility: hidden;
    opacity: 0;
    z-index: 100;
    -webkit-transition: all 1s ease;
    -moz-transition: all 1s ease;
    -ms-transition: all 1s ease;
    -o-transition: all 1s ease;
    transition:all 1s ease
}

.reveal.paused .pause-overlay {
    visibility: visible;
    opacity:1
}

.no-transforms {
    overflow-y:auto
}

.no-transforms .reveal .slides {
    position: relative;
    width: 80%;
    height: auto !important;
    top: 0;
    left: 50%;
    margin: 0;
    text-align:center
}

.no-transforms .reveal .controls, .no-transforms .reveal .progress {
    display:none !important
}

.no-transforms .reveal .slides section {
    display: block !important;
    opacity: 1 !important;
    position: relative !important;
    height: auto;
    min-height: auto;
    top: 0;
    left: -50%;
    margin: 70px 0;
    -webkit-transform: none;
    -moz-transform: none;
    -ms-transform: none;
    -o-transform: none;
    transform:none
}

.no-transforms .reveal .slides section section {
    left:0
}

.no-transition {
    -webkit-transition: none;
    -moz-transition: none;
    -ms-transition: none;
    -o-transition: none;
    transition:none
}

.reveal .state-background {
    position: absolute;
    width: 100%;
    height: 100%;
    background: rgba(0, 0, 0, 0);
    -webkit-transition: background 800ms ease;
    -moz-transition: background 800ms ease;
    -ms-transition: background 800ms ease;
    -o-transition: background 800ms ease;
    transition:background 800ms ease
}

.alert .reveal .state-background {
    background:rgba(200, 50, 30, .6)
}

.soothe .reveal .state-background {
    background:rgba(50, 200, 90, .4)
}

.blackout .reveal .state-background {
    background:rgba(0, 0, 0, .6)
}

.whiteout .reveal .state-background {
    background:rgba(255, 255, 255, .6)
}

.cobalt .reveal .state-background {
    background:rgba(22, 152, 213, .6)
}

.mint .reveal .state-background {
    background:rgba(22, 213, 75, .6)
}

.submerge .reveal .state-background {
    background:rgba(12, 25, 77, .6)
}

.lila .reveal .state-background {
    background:rgba(180, 50, 140, .6)
}

.sunset .reveal .state-background {
    background:rgba(255, 122, 0, .6)
}

.reveal.rtl .slides, .reveal.rtl .slides h1, .reveal.rtl .slides h2, .reveal.rtl .slides h3, .reveal.rtl .slides h4, .reveal.rtl .slides h5, .reveal.rtl .slides h6 {
    direction: rtl;
    font-family:sans-serif
}

.reveal.rtl pre, .reveal.rtl code {
    direction:ltr
}

.reveal.rtl ol, .reveal.rtl ul {
    text-align:right
}

.reveal aside.notes {
    display:none
}

.zoomed .reveal *, .zoomed .reveal :before, .zoomed .reveal :after {
    -webkit-transform: none !important;
    -moz-transform: none !important;
    -ms-transform: none !important;
    transform: none !important;
    -webkit-backface-visibility: visible !important;
    -moz-backface-visibility: visible !important;
    -ms-backface-visibility: visible !important;
    backface-visibility:visible !important
}

.zoomed .reveal .progress, .zoomed .reveal .controls {
    opacity:0
}

.zoomed .reveal .roll span {
    background:0
}

.zoomed .reveal .roll span:after {
    visibility: hidden
}

/*Github Ribbon Test*/
/* Source: https://github.com/dciccale/css3-github-ribbon */
/* Define classes for example, definition, problem etc. */
/* Choose meaningful colors for background and text */

.example {
  background-color: #121621;
  top: 1.2em;
  right: -3.2em;
  -webkit-transform: rotate(45deg);
  -moz-transform: rotate(45deg);
  transform: rotate(45deg);
  -webkit-box-shadow: 0 0 0 1px #1d212e inset,0 0 2px 1px #fff inset,0 0 1em #888;
  -moz-box-shadow: 0 0 0 1px #1d212e inset,0 0 2px 1px #fff inset,0 0 1em #888;
  box-shadow: 0 0 0 1px #1d212e inset,0 0 2px 1px #fff inset,0 0 1em #888;
  color: #FF0;
  display: block;
  padding: .6em 3.5em;
  position: absolute;
  font: bold .82em sans-serif;
  text-align: center;
  text-decoration: none;
  text-shadow: 1px -1px 8px rgba(0,0,0,0.60);
  -webkit-user-select: none;
  -moz-user-select: none;
  user-select: none;
}

.definition {
  background-color: #a00;
  top: 1.2em;
  right: -3.2em;
  -webkit-transform: rotate(45deg);
  -moz-transform: rotate(45deg);
  transform: rotate(45deg);
  -webkit-box-shadow: 0 0 0 1px #1d212e inset,0 0 2px 1px #fff inset,0 0 1em #888;
  -moz-box-shadow: 0 0 0 1px #1d212e inset,0 0 2px 1px #fff inset,0 0 1em #888;
  box-shadow: 0 0 0 1px #1d212e inset,0 0 2px 1px #fff inset,0 0 1em #888;
  color: #FFF;
  display: block;
  padding: .6em 3.5em;
  position: absolute;
  font: bold .82em sans-serif;
  text-align: center;
  text-decoration: none;
  text-shadow: 1px -1px 8px rgba(0,0,0,0.60);
  -webkit-user-select: none;
  -moz-user-select: none;
  user-select: none;
}

.ninety {
  font-size: 0.2em;
   }
   
p li {
  font-size: 10px;
}

.stack .present {
  text-align: left;
  font: italic 1em "Fira Sans", serif;
  color :white;
}

.reveal section img {
  display: block;
  left :0;
  right: 0;
  margin:auto;
}
/* Tomorrow Theme */
	/* http://jmblog.github.com/color-themes-for-google-code-highlightjs */
	/* Original theme - https://github.com/chriskempson/tomorrow-theme */
	/* http://jmblog.github.com/color-themes-for-google-code-highlightjs */
	.tomorrow-comment, pre .comment, pre .title {
		color: #8e908c;
	}

.tomorrow-red, pre .variable, pre .attribute, pre .tag, pre .regexp, pre .ruby .constant, pre .xml .tag .title, pre .xml .pi, pre .xml .doctype, pre .html .doctype, pre .css .id, pre .css .class, pre .css .pseudo {
	color: #c82829;
}

.tomorrow-orange, pre .number, pre .preprocessor, pre .built_in, pre .literal, pre .params, pre .constant {
	color: #f5871f;
}

.tomorrow-yellow, pre .class, pre .ruby .class .title, pre .css .rules .attribute {
	color: #eab700;
}

.tomorrow-green, pre .string, pre .value, pre .inheritance, pre .header, pre .ruby .symbol, pre .xml .cdata {
	color: #718c00;
}

.tomorrow-aqua, pre .css .hexcolor {
	color: #3e999f;
}

.tomorrow-blue, pre .function, pre .python .decorator, pre .python .title, pre .ruby .function .title, pre .ruby .title .keyword, pre .perl .sub, pre .javascript .title, pre .coffeescript .title {
	color: #4271ae;
}

.tomorrow-purple, pre .keyword, pre .javascript .function {
	color: #8959a8;
}

pre code {
	display: block;
	background: white;
	color: #4d4d4c;
		font-family: Menlo, Monaco, Consolas, monospace;
	line-height: 1.5;
	border: 1px solid #ccc;
	padding: 10px;
}
/* latin-ext */
@font-face {
  font-family: 'Lato';
  font-style: italic;
  font-weight: 400;
  src: url(https://fonts.gstatic.com/s/lato/v22/S6u8w4BMUTPHjxsAUi-qNiXg7eU0.woff2) format('woff2');
  unicode-range: U+0100-024F, U+0259, U+1E00-1EFF, U+2020, U+20A0-20AB, U+20AD-20CF, U+2113, U+2C60-2C7F, U+A720-A7FF;
}
/* latin */
@font-face {
  font-family: 'Lato';
  font-style: italic;
  font-weight: 400;
  src: url(https://fonts.gstatic.com/s/lato/v22/S6u8w4BMUTPHjxsAXC-qNiXg7Q.woff2) format('woff2');
  unicode-range: U+0000-00FF, U+0131, U+0152-0153, U+02BB-02BC, U+02C6, U+02DA, U+02DC, U+2000-206F, U+2074, U+20AC, U+2122, U+2191, U+2193, U+2212, U+2215, U+FEFF, U+FFFD;
}
/* latin-ext */
@font-face {
  font-family: 'Lato';
  font-style: italic;
  font-weight: 700;
  src: url(https://fonts.gstatic.com/s/lato/v22/S6u_w4BMUTPHjxsI5wq_FQftx9897sxZ.woff2) format('woff2');
  unicode-range: U+0100-024F, U+0259, U+1E00-1EFF, U+2020, U+20A0-20AB, U+20AD-20CF, U+2113, U+2C60-2C7F, U+A720-A7FF;
}
/* latin */
@font-face {
  font-family: 'Lato';
  font-style: italic;
  font-weight: 700;
  src: url(https://fonts.gstatic.com/s/lato/v22/S6u_w4BMUTPHjxsI5wq_Gwftx9897g.woff2) format('woff2');
  unicode-range: U+0000-00FF, U+0131, U+0152-0153, U+02BB-02BC, U+02C6, U+02DA, U+02DC, U+2000-206F, U+2074, U+20AC, U+2122, U+2191, U+2193, U+2212, U+2215, U+FEFF, U+FFFD;
}
/* latin-ext */
@font-face {
  font-family: 'Lato';
  font-style: normal;
  font-weight: 400;
  src: url(https://fonts.gstatic.com/s/lato/v22/S6uyw4BMUTPHjxAwXiWtFCfQ7A.woff2) format('woff2');
  unicode-range: U+0100-024F, U+0259, U+1E00-1EFF, U+2020, U+20A0-20AB, U+20AD-20CF, U+2113, U+2C60-2C7F, U+A720-A7FF;
}
/* latin */
@font-face {
  font-family: 'Lato';
  font-style: normal;
  font-weight: 400;
  src: url(https://fonts.gstatic.com/s/lato/v22/S6uyw4BMUTPHjx4wXiWtFCc.woff2) format('woff2');
  unicode-range: U+0000-00FF, U+0131, U+0152-0153, U+02BB-02BC, U+02C6, U+02DA, U+02DC, U+2000-206F, U+2074, U+20AC, U+2122, U+2191, U+2193, U+2212, U+2215, U+FEFF, U+FFFD;
}
/* latin-ext */
@font-face {
  font-family: 'Lato';
  font-style: normal;
  font-weight: 700;
  src: url(https://fonts.gstatic.com/s/lato/v22/S6u9w4BMUTPHh6UVSwaPGQ3q5d0N7w.woff2) format('woff2');
  unicode-range: U+0100-024F, U+0259, U+1E00-1EFF, U+2020, U+20A0-20AB, U+20AD-20CF, U+2113, U+2C60-2C7F, U+A720-A7FF;
}
/* latin */
@font-face {
  font-family: 'Lato';
  font-style: normal;
  font-weight: 700;
  src: url(https://fonts.gstatic.com/s/lato/v22/S6u9w4BMUTPHh6UVSwiPGQ3q5d0.woff2) format('woff2');
  unicode-range: U+0000-00FF, U+0131, U+0152-0153, U+02BB-02BC, U+02C6, U+02DA, U+02DC, U+2000-206F, U+2074, U+20AC, U+2122, U+2191, U+2193, U+2212, U+2215, U+FEFF, U+FFFD;
}


</style>

</br></br></br>

# Reproducible Pitch
## Production for Coursera assignement (Course 9 - Week 4)
#### Author : Idriss .S
#### Date : 12 april 2022

--- &vertical

### Introduction

This presentation aims to study a data set from R : `ToothGrowth`.

It's cover :

1. only data exploration (due to the limitation of 5 slides) ;

</br>

This work could be completed (of more slides are allowed) by :

2. fitting model responding to the question that try to explain the relationship 
between miles per gallon consmption and weight for each car.
3. discussions

</br>

### Exploratory data analysis

#### Data structure


```r
data(ToothGrowth)
str(ToothGrowth)
```

```
## 'data.frame':	60 obs. of  3 variables:
##  $ len : num  4.2 11.5 7.3 5.8 6.4 10 11.2 11.2 5.2 7 ...
##  $ supp: Factor w/ 2 levels "OJ","VC": 2 2 2 2 2 2 2 2 2 2 ...
##  $ dose: num  0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ...
```

***

#### Data structure - 2

```r
library(tidyr)
library(rstatix)
ToothGrowth %>% sample_n_by(dose, size = 3)
```

```
## # A tibble: 9 × 3
##     len supp   dose
##   <dbl> <fct> <dbl>
## 1  14.5 OJ      0.5
## 2   4.2 VC      0.5
## 3   7.3 VC      0.5
## 4  19.7 OJ      1  
## 5  14.5 VC      1  
## 6  22.5 VC      1  
## 7  27.3 OJ      2  
## 8  26.4 OJ      2  
## 9  33.9 VC      2
```

```r
# Quick summary of the data
summary(ToothGrowth)
```

```
##       len        supp         dose      
##  Min.   : 4.20   OJ:30   Min.   :0.500  
##  1st Qu.:13.07   VC:30   1st Qu.:0.500  
##  Median :19.25           Median :1.000  
##  Mean   :18.81           Mean   :1.167  
##  3rd Qu.:25.27           3rd Qu.:2.000  
##  Max.   :33.90           Max.   :2.000
```

```r
# Exploration of ToothGrowth$dose and ToothGrowth$len, since we know exactly what ToothGrowth$supp contains
unique(ToothGrowth$dose)
```

```
## [1] 0.5 1.0 2.0
```

```r
unique(ToothGrowth$len)
```

```
##  [1]  4.2 11.5  7.3  5.8  6.4 10.0 11.2  5.2  7.0 16.5 15.2 17.3 22.5 13.6 14.5
## [16] 18.8 15.5 23.6 18.5 33.9 25.5 26.4 32.5 26.7 21.5 23.3 29.5 17.6  9.7  8.2
## [31]  9.4 19.7 20.0 25.2 25.8 21.2 27.3 22.4 24.5 24.8 30.9 29.4 23.0
```

***

#### Box plot - code



```r
library(ggplot2)
my_plot_fun <- function(my_data,my_x) {
  my_data <- as.data.frame(my_data)
  my_data %>%
  ggplot(aes(x=my_x, y=len)) +
theme(plot.title = element_text(color="#ADA717", size=18, face="bold.italic",hjust=0.5),
 axis.title.x = element_text(color="#993333", size=18, face="bold"),
 axis.text.x = element_text(color="#993333", size=14,vjust = 0),
 axis.title.y = element_text(color="darkgreen", size=18, face="bold"),
 axis.text.y = element_text(face="bold", color="darkgreen", size=14),
 legend.text = element_text(size=12),
 legend.title = element_text(size=16))}

# Conversion of ToothGrowth$dose into a factor
ToothGrowth$dose<-as.factor(ToothGrowth$dose)

mpt <-my_plot_fun(ToothGrowth,my_x = ToothGrowth$dose) + 
  geom_boxplot(aes(color = supp,group=dose), width = 0.6) +
  geom_dotplot(aes(fill = as.factor(dose), color = supp,group=dose), binaxis='y', stackdir='center', 
               dotsize = 0.8,position = position_dodge(0.8),binwidth=1)+
  scale_fill_manual( values = c("#00AFBB", "#E3B166","#A1A861"))+
  scale_color_manual(values = c("red", "black")) + facet_grid(~ supp)+
  labs(x = "Doses", y ="Toothe Length", fill = "Doses", color="Supplement delivery",
    title = "Tooth length \n by dose amount",
  caption = "Plot tooth length ('len') by the dose amount ('dose'), \n grouped by supplement delivery method ('supp')") +
  theme(plot.caption = element_text(color = "darkblue", face = "italic", size = 12))
```

***
#### Box plot - plot


```r
mpt
```

![plot of chunk unnamed-chunk-4](assets/fig/unnamed-chunk-4-1.png)

</br>

#### Thanks for watching. Regards.
##### Idriss .S