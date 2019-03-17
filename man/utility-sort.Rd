\name{sortByHue}
\alias{sortByHue}
\alias{sortByLuminance}

\title{Sorting Palettes}
\description{
  Functions to sort palettes; potentially useful for combining existing
  palettes to create new ones.
}
\usage{
sortByHue(colorset)
sortByLuminance(colorset)
}
\arguments{
  \item{colorset}{a character vector containing hexadecimal color
    values.}
}
\details{
  These functions take a palette as input, sort it either by the hue or
  by the luminance, and return the result. One possibnle aplication
  would be to combine "dark" and "light" palettes to generate larger
  version of the RColorBrewer "Paired" palette.
}
\value{
  Returns a new color set (i.e., a palette, implemented as a character
  string containing the hex values of color), after sorting.
}
\author{Kevin R. Coombes <krc@silicovore.com>}
\examples{
D <- dark.colors(24)
L <- light.colors(24)
X <- sortByHue(c(D,L))
names(X) <- colorNames(X)
X <- X[!duplicated(names(X))]
swatch(X)
Y <- sortByLuminance(X)
swatch(Y)
}
\keyword{color}
