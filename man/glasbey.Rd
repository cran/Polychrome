\name{glasbey}
\alias{glasbey}
\docType{data}
\title{The 32-color Glasbye palette}
\description{
  A palette composed of 32 distinct colors.
}
\usage{data(glasbey)}
\format{
  A character string of length 32.
}
\details{
  A character vector containing hexadecimal color representations of 32
  distinctive colors that are well separated in the CIE L*u*v* color
  space.
}
\source{
  The color palette was created, using standard tools in the
  \code{colorspace} package from a manually transcribed matrix of
  RGB values copied from the paper by Glasbey and colleagues. 
}
\references{
  Glasbey CA, van der Heijden GWAM, Toh VFK, Gray AJ (2007). Colour Displays for
  Categorical Images. Color Research and Application, 32, 304-9.
}
\examples{
data(glasbey)
head(glasbey)
}
\keyword{datasets}
\keyword{color}
