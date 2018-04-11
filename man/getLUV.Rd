\name{getLUV}
\alias{getLUV}

\title{Get the L*u*v* coordinates of the colors}
\description{
  Given a character vector of colors in any format acceptable to R, this
  function computes their coordinates in L*u*v* color space.
}
\usage{
getLUV(colorset)
}
\arguments{
  \item{colorset}{a vector containing color values in any format
    recognized by R.}
}
\details{
  The real point of this function is to allow users to plot the colors
  in three-dimensional space using the \code{rgl} package. Because
  \code{rgl} depends on an external installation of XQuartz on Macintosh
  computers (and because we have found that some students in courses
  that we teach are apparently unable to install XQuartz, especially if
  they are using institutional computers without adminstrative
  privileges), \code{Polychrome} no longer imports or depends upon the
  \code{rgl} package, instead relying on \code{scatterplot3d} for its 3D
  plots. The example below shows how to plot a color set using
  \code{rgl}.
}
\value{
  Returns a list containing two components: (1) a three-column matrix
  named \code{coords} containing the luminance (L) and hue coordinates
  (U, V)   of each color provided in the input colorset; and (2) a
  character vector named \code{cset} containng a hexadecimal
  representation of the colorset.
}
\author{Kevin R. Coombes <krc@silicovore.com>}
\examples{
data(alphabet)
luv <- getLUV(alphabet)
scatterplot3d::scatterplot3d(luv$coords, color = luv$cset,
    pch = 16, cex.symbol = 3)
\dontrun{
library(rgl)
x <- luv$coords
cset <- luv$cset
open3d(windowRect=c(40, 40, 840, 840))
plot3d(x, main="Alphabet Colors")
spheres3d(x, radius=10, col=cset, shininess=100)
}
}
\keyword{color}
