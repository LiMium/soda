## Goal
To build a legible layout engine that is close to the theoretical model defined in CSS specs.

## Scope

Only static layout and rendering, which implies:
  * No scrolling, hovering, selecting, resizing and other user interactions
  * No form controls
  * No cursors
  * No animations, HTML5 Canvas
  * No DOM manipulations through Javascript, etc
  * No CSS DOM
  * Synchronous operations for loading resources like images, stylesheets, etc

#### Features which are within scope but low priority:
  * Border styles
  * Border radius
  * Box Shadows

## Status
As of 11 June 2020, about 57% tests from the W3C CSS2.1 test suite are passing.

Major features yet to be implemented:

  * floated elements
  * counters
  * margin collapse (partially done, more variations TBD)
  * RTL language support
