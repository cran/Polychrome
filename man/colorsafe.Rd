\name{colorsafe}
\alias{colorsafe}
\docType{data}
\title{A 10-Color Palette Distinguishable By COlor-Deficit Individuals}
\description{
  A palette composed of 10 distinctive colors, selected to be
  distinguishable by a mojority of individuals who have some form mof
  color deficient vision.
}
\usage{data(colorsafe)}
\format{
  A character string of length 10.
}
\details{
  A character vector containing hexadecimal color representations of 10
  distinctive colors that are well separated in the CIE L*u*v* color
  space, chosen to be distinguishable by people with color deficient vision.
}
\source{
  Details of how te paklette was constructed can be found in the
  "color-deficit" vignette.
}
\seealso{
  \code{\link{createPalette}}
}
\examples{
data(colorsafe)
colorsafe
\dontrun{vignette("color-deficits")}
}
\keyword{datasets}
\keyword{color}
