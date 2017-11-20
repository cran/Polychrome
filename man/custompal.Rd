\name{Dark24}
\alias{Dark24}
\alias{Light24}
\docType{data}
\title{Light and Dark 24-Color Palettes}
\description{
  Two palettes, each composed of 24 distinctive colors, optimized for
  either a light background (Dark24) or a dark background (Light24). 
}
\usage{
data(Dark24)
data(Light24)
}
\format{
  A character vector of length 24.
}
\details{
  A character vector containing hexadecimal color representations of 24
  distinctive colors that are well separated in the CIE L*u*v* color
  space.
}
\source{
  Both color palettes were generated using the
  \code{\link{createPalette}} function. In addition to specifing seed
  colors, the luminance range was restricted to produce either only
  light colors or only dark colors.
}
\seealso{
  \code{\link{createPalette}}
}
\examples{
data(Dark24)
Dark24
data(Light24)
swatch(Light24)
}
\keyword{datasets}
\keyword{color}
