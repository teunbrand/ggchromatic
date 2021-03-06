% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/palettes.R
\name{colourspace_palettes}
\alias{colourspace_palettes}
\alias{rgb_palette}
\alias{hsv_palette}
\alias{hsl_palette}
\alias{hcl_palette}
\alias{cmyk_palette}
\alias{cmy_palette}
\alias{lab_palette}
\alias{lch_palette}
\alias{oklab_palette}
\alias{oklch_palette}
\title{Colour space palettes}
\usage{
rgb_palette(x, min = 0, max = 1)

hsv_palette(x, min = 0, max = 1)

hsl_palette(x, min = 0, max = 1)

hcl_palette(x, min = 0, max = 1)

cmyk_palette(x, min = 0, max = 1)

cmy_palette(x, min = 0, max = 1)

lab_palette(x, min = 0, max = 1)

lch_palette(x, min = 0, max = 1)

oklab_palette(x, min = 0, max = 1)

oklch_palette(x, min = 0, max = 1)
}
\arguments{
\item{x}{An \code{colour_spec} vector with a \code{numeric} vector in every channel.}

\item{min, max}{A \code{numeric(1)} specifying the lower and upper limits
respectively.}
}
\value{
A \code{character} vector of the same length as the \code{x} argument with
hexadecimal colour notation.
}
\description{
These palette functions transform all-numeric \code{colour_spec} vectors into
colours.
}
\details{
The conversion from \code{colour_spec} vectors to colour is powered by
the \code{\link[farver:encode_colour]{farver::encode_colour()}} function.
}
\section{Functions}{
\itemize{
\item \code{rgb_palette}: Red, Green and Blue colour space.

\item \code{hsv_palette}: Hue, Saturation and Value colour space.

\item \code{hsl_palette}: Hue, Saturation and Lightness colour space.

\item \code{hcl_palette}: Hue, Chroma and Luminance colour space.

\item \code{cmyk_palette}: Cyan, Magenta, Yellow and Key (black) colour
space.

\item \code{cmy_palette}: Cyan, Magenta and Yellow colour space.

\item \code{lab_palette}: Lightness*, a* (green-red axis), b*
(blue-yellow axis) colour space. Also known as CIE L*ab.

\item \code{lch_palette}: Lightness*, Chroma*, Hue-angle  colour
space. Also known as CIE LCh.

\item \code{oklab_palette}: An OK Lightness, a (green-red axis), b
(blue-yellow axis) colour space. Also known as OKLab. Needs farver package
version \>2.0.3.

\item \code{oklch_palette}: An OK Lightness*, Chroma*, Hue-angle colour
space. Also known as OKLch. Needs farver package version \>2.0.3.
}}

\examples{
# Getting the colour "red" in different colour spaces
rgb_palette(rgb_spec(1, 0, 0))
hsv_palette(hsv_spec(0, 1, 1))
hsl_palette(hsl_spec(0, 1, 0.5))
hcl_palette(hcl_spec(0.034, 0.995, 0.532))
cmyk_palette(cmyk_spec(0, 1, 1, 0))
cmy_palette(cmy_spec(0, 1, 1))
lab_palette(lab_spec(0.5, 1, 1))
lch_palette(lch_spec(0.53, 0.79, 0.11))
}
\seealso{
The \link{colour_spec} page for building colour space vectors. The
\link{scale_chromatic} page for the scales wherein these palettes are used.
}
