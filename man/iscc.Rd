\name{iscc}
\alias{iscc}
\docType{data}
\title{Color Names From the Inter-Society Color Council (ISCC)}
\description{
  A data frame mapping hex codes for 267 colors to their official
  ISCC-NBS names.
}
\usage{data(iscc)}
\format{
  A data frame with three columns and 267 rows.
}
\details{
  This data set contains short names, long names, and hex codes for the
  267 official color namkes defineed by the ISCC. Data was obtained from
  the Texas Precancel CLub and reformatted to be used conveniently in R.
}
\source{
  Our main source was the no-longer-extant web site of the Texas
  Precancel Club ({http://tx4.us/nbs-iscc.htm}).
}
\references{
  See the Inter-Society Color Council web site
  (\url{https://iscc.org/}); the Wikipedia article on the ISCC-NBS
  system of color designation
  (\url{https://en.wikipedia.org/wiki/ISCC\%E2\%80\%93NBS_system}. 
}
\seealso{
  \code{\link{isccNames}}
}
\examples{
data(iscc)
head(iscc)
}
\keyword{datasets}
\keyword{color}
