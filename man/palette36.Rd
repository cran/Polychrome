\name{palette36}
\alias{palette36}
\docType{data}
\title{A 36-Color Palette}
\description{
  A palette composed of 36 distinctive colors.  
}
\usage{data(palette36)}
\format{
  A character string of length 36.
}
\details{
  A character vector containing hexadecimal color representations of 36
  distinctive colors that are well separated in the CIE L*u*v* color
  space.  Each color is assigned a name from the ISCC-NBS standard.
}
\source{
  The color palette was generated using the \code{\link{createPalette}}
  function with three seed colors: ebony ("#474747"), iron ("#E2E2E2"),
  and red ("#F70000"). 
}
\seealso{
  \code{\link{createPalette}}, \code{\link{isccNames}}.
}
\examples{
data(palette36)
palette36
}
\keyword{datasets}
\keyword{color}
