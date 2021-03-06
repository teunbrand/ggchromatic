% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale_colour_spaces.R
\name{scale_chromatic}
\alias{scale_chromatic}
\alias{scale_colour_rgb}
\alias{scale_fill_rgb}
\alias{scale_color_rgb}
\alias{scale_colour_hsv}
\alias{scale_fill_hsv}
\alias{scale_color_hsv}
\alias{scale_colour_hsl}
\alias{scale_fill_hsl}
\alias{scale_color_hsl}
\alias{scale_colour_hcl}
\alias{scale_fill_hcl}
\alias{scale_color_hcl}
\alias{scale_colour_cmyk}
\alias{scale_fill_cmyk}
\alias{scale_color_cmyk}
\alias{scale_colour_cmy}
\alias{scale_fill_cmy}
\alias{scale_color_cmy}
\alias{scale_colour_lab}
\alias{scale_fill_lab}
\alias{scale_color_lab}
\alias{scale_colour_lch}
\alias{scale_fill_lch}
\alias{scale_color_lch}
\alias{scale_colour_oklab}
\alias{scale_fill_oklab}
\alias{scale_color_oklab}
\alias{scale_colour_oklch}
\alias{scale_fill_oklch}
\alias{scale_color_oklch}
\title{Colour space scales}
\usage{
scale_colour_rgb(..., aesthetics = "colour")

scale_fill_rgb(..., aesthetics = "fill")

scale_colour_hsv(..., aesthetics = "colour")

scale_fill_hsv(..., aesthetics = "fill")

scale_colour_hsl(..., aesthetics = "colour")

scale_fill_hsl(..., aesthetics = "fill")

scale_colour_hcl(..., aesthetics = "colour")

scale_fill_hcl(..., aesthetics = "fill")

scale_colour_cmyk(..., aesthetics = "colour")

scale_fill_cmyk(..., aesthetics = "fill")

scale_colour_cmy(..., aesthetics = "colour")

scale_fill_cmy(..., aesthetics = "fill")

scale_colour_lab(..., aesthetics = "colour")

scale_fill_lab(..., aesthetics = "fill")

scale_colour_lch(..., aesthetics = "colour")

scale_fill_lch(..., aesthetics = "fill")

scale_colour_oklab(..., aesthetics = "colour")

scale_fill_oklab(..., aesthetics = "fill")

scale_colour_oklch(..., aesthetics = "colour")

scale_fill_oklch(..., aesthetics = "fill")
}
\arguments{
\item{...}{
  Arguments passed on to \code{\link[=chromatic_scale]{chromatic_scale}}
  \describe{
    \item{\code{palette}}{A palette function that when called with a \code{colour_spec}
vector should return a vector of colours.}
    \item{\code{breaks}}{One of \itemize{
\item \code{NULL} for no breaks.
\item \code{waiver()} for the default breaks computed by the
\href{scales::trans_new()}{transformation object}.
\item A \code{colour_spec} vector. For continuous channels, must be a numeric
channel. For discrete channels, a character channel. Channels can be padded
with \code{NA}s if the desired breaks are of unequal length.
\item A \code{function} that uses the limits as input and returns breaks. Note
that this is used for both continuous and discrete channels.
\item A named \code{list} with the names of channels with (1) a \code{character}
or \code{numeric} vector giving the breaks for that channel or (2) a function to
be applied to the limits of that channel or (3) \code{NULL} for no breaks in
that channel. Channels whose names are absent in the \code{list}'s names are
treated with the \code{waiver()} option above.
}}
    \item{\code{labels}}{One of \itemize{
\item \code{NULL} for no labels.
\item \code{waiver()} for the default labels. In case of continuous channels,
these are passed through the format function of the
\href{scales::trans_new()}{transformation object}.
\item A \code{colour_spec} vector with character vectors in the channels. The
channels can be padded with \code{NA}s to match the length of channels with the
most breaks.
\item A \code{function} that uses the breaks as input and returns labels. Note
that this is used for both continuous and discrete channels.
\item A named \code{list} with the names of channels with (1) a \code{character}
vector giving the labels for that channel or (2) a function to be applied to
the breaks of that channel or (3) \code{NULL} for no labels in that channel.
Channels whose names are absent in the \code{list}'s names are treated with the
\code{waiver()} option above.
}}
    \item{\code{limits}}{One of \itemize{
\item \code{NULL} to use the default scale range.
\item A \code{colour_spec} vector. For continuous channels, must be a length 2
vector giving the minimum and maximum. For discrete channels, the relevant
channel should define possible values. For mixed usage, the continuous
limits can be padded with \code{NA}s.
\item A \code{function} that accepts the existing (automatic) limits and returns
new limits. Note that this is used for both continuous and discrete
channels.
\item A named \code{list} with names of channels with (1) a vector defining
the limits or (2) a function to be applied to the natural limits. Channels
whose names are absent in the \code{list}'s names are treated with the \code{NULL}
option above.
}}
    \item{\code{prototype}}{A \code{function} that serves as constructor for the specific
\code{colour_spec} class.}
    \item{\code{channel_limits}}{One of: \itemize{
\item A \code{colour_spec} vector of length 2 containing \code{numeric} channels that
indicating the limits for each channel between 0-1.
\item A named \code{list} with channel names and length 1 or 2 \code{numeric} vectors
that indicate the limits for that channel between 0-1.
}}
    \item{\code{scale_name}}{The name of the scale that should be used for error messages
associated with this scale.}
    \item{\code{name}}{The name of the scale. Used as the axis or legend title. If
\code{waiver()}, the default, the name of the scale is taken from the first
mapping used for that aesthetic. If \code{NULL}, the legend title will be
omitted.}
    \item{\code{n.breaks}}{An integer guiding the number of major breaks. The algorithm
may choose a slightly different number to ensure nice break labels. Will
only have an effect if \code{breaks = waiver()}. Use \code{NULL} to use the default
number of breaks given by the transformation.}
    \item{\code{rescaler}}{A function used to scale the input values to the
range [0, 1]. This is always \code{\link[scales:rescale]{scales::rescale()}}, except for
diverging and n colour gradients (i.e., \code{\link[ggplot2:scale_gradient]{scale_colour_gradient2()}},
\code{\link[ggplot2:scale_gradient]{scale_colour_gradientn()}}). The \code{rescaler} is ignored by position
scales, which always use \code{\link[scales:rescale]{scales::rescale()}}.}
    \item{\code{oob}}{One of:
\itemize{
\item Function that handles limits outside of the scale limits
(out of bounds).
\item The default (\code{\link[scales:oob]{scales::censor()}}) replaces out of
bounds values with \code{NA}.
\item \code{\link[scales:oob]{scales::squish()}} for squishing out of bounds values into range.
\item \code{\link[scales:oob]{scales::squish_infinite()}} for squishing infinite values into range.
}}
    \item{\code{expand}}{For position scales, a vector of range expansion constants used to add some
padding around the data to ensure that they are placed some distance
away from the axes. Use the convenience function \code{\link[ggplot2:expansion]{expansion()}}
to generate the values for the \code{expand} argument. The defaults are to
expand the scale by 5\% on each side for continuous variables, and by
0.6 units on each side for discrete variables.}
    \item{\code{na.value}}{Missing values will be replaced with this value.}
    \item{\code{trans}}{For continuous scales, the name of a transformation object
or the object itself. Built-in transformations include "asn", "atanh",
"boxcox", "date", "exp", "hms", "identity", "log", "log10", "log1p", "log2",
"logit", "modulus", "probability", "probit", "pseudo_log", "reciprocal",
"reverse", "sqrt" and "time".

A transformation object bundles together a transform, its inverse,
and methods for generating breaks and labels. Transformation objects
are defined in the scales package, and are called \verb{<name>_trans} (e.g.,
\code{\link[scales:boxcox_trans]{scales::boxcox_trans()}}). You can create your own
transformation with \code{\link[scales:trans_new]{scales::trans_new()}}.}
    \item{\code{guide}}{A function used to create a guide or its name. See
\code{\link[ggplot2:guides]{guides()}} for more information.}
    \item{\code{super}}{The super class to use for the constructed scale}
  }}

\item{aesthetics}{The names of the aesthetics that this scale works with.}
}
\value{
A \code{ScaleChromatic} ggproto object that can be added to a plot.
}
\description{
These scales map the values of \code{colour_spec} vectors to coordinates in a
colour space.
}
\note{
Specifying \code{limits}, \code{breaks} and \code{labels} works slightly differently
compared to ggplot2, as these are needed for every channel in a colour
space. When providing these arguments as a vector, a \code{colour_spec} vector
describing the arguments for all channels is expected. If provided a
function, the function is applied to every channel. To make it easier to set
these arguments for channels individually, the preferred way is to provide
these arguments with a named list, wherein the names are the first letters
of the channels. For example, you can set the following in
\code{scale_colour_hsv()}:\if{html}{\out{<div class="r">}}\preformatted{labels = list(h = scales::percent, v = c("First", "Second", "Third"))
}\if{html}{\out{</div>}}

To give the \emph{hue} percent labels, use the default labels for \emph{saturation}
by omission, and set the literal labels for \emph{value}.
}
\section{Functions}{

\itemize{
\item \verb{scale_*_rgb()}: Red, Green and Blue colour space.
\item \verb{scale_*_hsv()}: Hue, Saturation and Value colour space.
\item \verb{scale_*_hsl()}: Hue, Saturation and Lightness colour space.
\item \verb{scale_*_hcl()}: Hue, Chroma and Luminance colour space.
\item \verb{scale_*_cmyk()}: Cyan, Magenta, Yellow and Key (black) colour space.
\item \verb{scale_*_cmy()}: Cyan, Magenta and Yellow colour space.
\item \verb{scale_*_lab()}: Lightness*, a* (green-red axis), b* (blue-yellow axis)
colour space. Also known as CIE-L\emph{a}b*.
\item \verb{scale_*_lch()}: Lightness*, Chroma*, Hue-angle colour space. Also known
as CIE LCh.
}
}

\examples{
# Empty channels will take the midpoint of the channel limits.
# Note that the 'luminance' channel is missing below.
p <- ggplot(economics, aes(date, unemploy)) +
  geom_point(aes(colour = hcl_spec(pop, psavert)))
p

# You can set the output of missing channels through the channel limits.
# Setting 1 value fixes the output for that channel.
p + scale_colour_hcl(channel_limits = list(l = 0.8))

# Alternatively, you can constrain the output of particular channels.
# Setting 2 values between 0-1 restricts the channel output range.
p <- ggplot(economics, aes(date, unemploy)) +
  geom_point(aes(colour = hcl_spec(pop, psavert, pce)))
p + scale_colour_hcl(channel_limits = list(l = c(0.5, 1), c = c(0.2, 0.8)))

# Setting breaks, labels and limits through named lists
p + scale_colour_hcl(
  breaks = list(c = c(5, 10, 15)),
  labels = list(h = scales::number_format(scale = 1e-3, suffix = "K")),
  limits = list(h = c(200e3, 300e3), c = c(5, 15), l = c(3000, 9000)),
  oob = scales::oob_squish
)

# Scale can have names for every channel to be displayed in the guide
p + scale_colour_hcl(name = c("Hue", "Chroma", "Luminance"))

# Scale can handle all-discrete variables
p <- ggplot(iris, aes(Sepal.Width, Sepal.Length))
p + geom_point(aes(colour = rgb_spec(Species, sample(Species), sample(Species))))

# Or can handle a mix between discrete and continuous
p + geom_point(aes(colour = cmy_spec(Petal.Length, Species, Petal.Width)))
}
\seealso{
The \link{colour_spec} page for creating \code{colour_spec} vectors.
}
