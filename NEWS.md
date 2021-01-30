# ggchromatic 0.0.0.9000

## General

* Added a `NEWS.md` file to track changes to the package.

## Functions

* Added `chromatic_scale()` constructor for chromatic scales.
* Added `colour_spec` vctrs classes for colour spaces.
* Added `void_channel` vctrs class as placeholder for missing colour channels.
* Added `vexpression` vctrs class to wrap expressions inside vctrs framework.
* Added `scale_fill/colour_*()` for supported colour spaces.
* Added `*_palette()` palettes for supported colour spaces.

## Guides

* Added `guide_colourcube()` guide for displaying three channels in a isometric
  perspective cube.
* Added `guide_colourrect()` guide for displaying two channels as a coloured 
  rectangle.
* Added `guide_colourbar2()` guide for adapting chromatic scales to the 
  colourbar guide.
* Added `guide_chromatic()` 'guide' for choosing an appropriate guide given the
  scale.

## Colour spaces

These colour spaces have `*_spec()`, `*_palette()`, `scale_colour/fill_*()` 
functions.

* Support for RGB colour space.
* Support for HSV colour space.
* Support for HSL colour space.
* Support for HCL colour space.
* Support for CMYK colour space.
* Support for CMY colour space.
* Support for CIE-Lab* colour space.
* Support for CIE-LCh colour space.
* Provisional support for OKLab and OKLch colour spaces (needs development 
  version of the farver package >2.0.3).
