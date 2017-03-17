\name{turnGray}
\alias{turnGray}

\title{Convert Color Palette to Grayscale}
\description{
  Tools to convert colors to gray scale.
}
\usage{
turnGray(colorset)
}
\arguments{
  \item{colorset}{A character string containing hexadecimal values of colors.}
}
\details{
  Converting figures to grayscale can happen because of limitations of
  the medium (e.g., printing on a black-and-white printer).  It can also
  effectively happen when the viewer does not have normal color
  perception (e.g., some form of color blindness). To help test if a
  palette under consideration will work in this context, we provide the
  \code{turnGray} function, which can be combined with the various
  \code{\link{palette.viewers}} to visualize the results.
}
\value{
  A character string containing hexadecimal grayscale versions of the input colors.
}
\author{Kevin R. Coombes <krc@silicovore.com>}
\references{
  Kelly KL. Twenty-Two Colors of Maximum Contrast. Color Eng., 1965;
  3:26--7. 
}
\seealso{
  \code{\link{palette.viewers}}
}
\examples{
mycolors <- alphabet.colors(10)
turnGray(mycolors)
}
\keyword{color}
