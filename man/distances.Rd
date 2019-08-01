\name{distances}
\alias{distances}
\alias{plotDistances}
\alias{computeDistances}

\title{Visualizing Color Palettes}
\description{
  Functions that provide visualization of palettes to help determine
  appropriate contexts where thay can be used. 
}
\usage{
computeDistances(colorset)
plotDistances(colorset, main=deparse(substitute(colorset)), pch=16, ...)
}
\arguments{
  \item{colorset}{a character vector containing hexadecimal color
    values.}
  \item{main}{a character string, the main title for a plot}
  \item{pch}{Plotting character to use.}
  \item{\dots}{additional graphical parameters.}
}
\details{
  Carter and Carter established the fact that, for two colors to be
  reliably distinguished, the Euclidean distance between their
  representations in CIE L*u*v* color space should be at least 40
  units. The  \code{computeDistances} function reorders the colors by
  maximal separation in L\*u\*v\*  space, and computes the minimum
  distance of the next color to all the preceeding colors.  The
  \code{plotDistances} function computes distances and immediately plots
  the result.
}
\value{
  The \code{plotDistances} function returns a list with two vector
  components: the \code{colors} in sorted order, and the minimum
  \code{distances} from  each color to the set of preceeding colors. The
  \code{computeDistances} function returns the vector of minimum
  distances.
}
\author{Kevin R. Coombes <krc@silicovore.com>}
\references{
  Carter RC, Carter EC. High-contrast sets of colors. Applied Optics,
  1982; 21(16):2936--9.
  
  Coombes KR, Brock G, Abrams ZB, Abruzzo LV. Polychrome: Creating and
  Assessing Qualitative Palettes with Many Colors. Journal of
  Statistical Software. 2019; 90(1):1--23.

}
\seealso{
  \code{\link{palette.viewers}}
  }
\examples{
data(alphabet)
plotDistances(alphabet)
luvd <- computeDistances(alphabet)
}
\keyword{color}
