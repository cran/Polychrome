\name{palette.viewers}
\alias{palette.viewers}
\alias{Palette Viewers}
\alias{rancurves}
\alias{ranpoints}
\alias{swatch}
\alias{swatchHue}
\alias{swatchLuminance}
\alias{ranswatch}
\alias{uvscatter}
\alias{luminance}
\alias{plothc}
\alias{plotpc}
\alias{p3d}

\title{Visualizing Color Palettes}
\description{
  Functions that provide visualization of palettes to help determine
  appropriate contexts where thay can be used. 
}
\usage{
rancurves(colorset, ...)
ranpoints(colorset, N=10, ...)
swatch(colorset, main=deparse(substitute(colorset)))
swatchHue(colorset, main=paste(deparse(substitute(colorset)),
                           ", by Hue", sep=""))
swatchLuminance(colorset, main=paste(deparse(substitute(colorset)),
                           ", by Luminance", sep=""))
ranswatch(colorset, main=deparse(substitute(colorset)))
uvscatter(colorset, main=deparse(substitute(colorset)), ...)
luminance(colorset, main=deparse(substitute(colorset)), ...)
plothc(colorset, main=deparse(substitute(colorset)), ...)
plotpc(colorset, main=deparse(substitute(colorset)), ...)
p3d(colorset, main=deparse(substitute(colorset)), ...)
}
\arguments{
  \item{colorset}{a character vector containing hexadecimal color
    values.}
  \item{main}{a character string, the main title for a plot}
  \item{N}{an integer; the number of points to plot in each color.}
  \item{\dots}{additional graphical parameters.}
}
\details{
  Different palettes are useful in different contexts.  For example,
  high luminance colors may work well in barplots but provide low
  contrast when used to color points in scatter plots. The best way to
  decide if a palette is right for any particular application is
  probably to create a sample plot using the palette.  The functions
  described here provide sample plots that display colors.

  The function \code{rancurves} produces a set of sine curves with
  different phases and amplitudes, with each curve shown in a different
  color. The function \code{ranpts} produces a scatter plot showing
  \code{N} clustered points in each of the palette colors.

  There are four functions that use barplots to display the palette. The
  simplest one, \code{swatch}, simply produces one bar of height one for
  each color, in the order that they are listed in the palette.  The
  next two, \code{swatchHue} and \code{swatchLuminance}, first sort the palette
  (by hue or by luminance, respectively), before producing the
  barplot.  The goal of these functions is to make sure that similar
  colors can be distinguished by placing them close together. The final
  function, \code{ranswatch}, randomly sorts the colors, to help decide if
  similar colors are identifiable when they are relatively far apart.

  The \code{p3d} function plots the palette colors as spheres in
  three-dimensional CIE L*u*v* color space.  It has been shown that
  perceptual distance is closely related to Euclidean distance in L*u*v*
  space. The \code{uvscatter} function produces a scatter plot of the
  palette colors using their projected u-v coordinates. The
  \code{luminance} function sorts the colors by luminance and produces a
  scatter plot showing the luminance.

  The \code{plothc} function performs hierarchical clustering on the
  colors (using Euclidan distance in CIE L*u*v* color space and Ward's
  linkage) and displays the resulting dendrogram. The \code{plotpc}
  function uses the same distance metric to compute and plot principal
  components.

}
\value{
  In general, these functions are used for their side-effect (producing
  plots) rather than for their return values. In most cases, they
  invisibly return the color set with which they were invoked.  The
  barplot-based functions (\code{swatch}, \code{ranswatch}, \code{swatchHue}, and
  \code{swatchLuminance}), however, return the vector of bar-centers, which
  can be used to add other information to the plot. The \code{plothc}
  function returns the dendrogram, and the \code{plotpc} function
  returns the principal components object.
}
\author{Kevin R. Coombes <krc@silicovore.com>}
\seealso{
  \code{\link{palette36.colors}}
  }
\examples{
data(alphabet)
rancurves(alphabet)
ranpoints(alphabet)
uvscatter(alphabet)
luminance(alphabet)
plothc(alphabet)
p3d(alphabet, cex.symbols = 2)
swatch(alphabet)
swatchHue(alphabet)
swatchLuminance(alphabet)
ranswatch(alphabet)
}
\keyword{color}
