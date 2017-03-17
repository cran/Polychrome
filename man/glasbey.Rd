\name{glasbey}
\alias{glasbey}
\docType{data}
\title{The 32-color Glasbye palette}
\description{
  A palette composed of 32 distinct colors.
}
\usage{data(glasbey)}
\format{
  A character vector containing hexadecimal color representations of 32
  distinctive colors that are well separated in the CIE L*u*v* color
  space.
}
\details{
  This data set contains short names, long names, and hex codes for the
  267 official color namkes defineed by the GLASBEY. Data was obtained from
  the Texas Precancel CLub and reformatted to be used conveniently in R.
}
\source{
  The color palewtte was created, using standard tools in the
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
