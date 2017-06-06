\name{alphabet}
\alias{alphabet}
\docType{data}
\title{A 26-Color Palette}
\description{
  A palette composed of 26 distinctive colors with names corresponding
  to letters of the alphabet.  
}
\usage{data(alphabet)}
\format{
  A character string of length 26.
}
\details{
  A character vector containing hexadecimal color representations of 26
  distinctive colors that are well separated in the CIE L*u*v* color
  space.
}
\source{
  The color palette was generated using the \code{\link{createPalette}}
  function with three seed colors: ebony ("#474747"), iron ("#E2E2E2"),
  and red ("#F70000"). The colors were then manually assigned names
  begining with different letters of the English alphabet.
}
\seealso{
  \code{\link{createPalette}}
}
\examples{
data(alphabet)
alphabet
}
\keyword{datasets}
\keyword{color}
