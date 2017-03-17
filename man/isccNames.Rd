\name{isccNames}
\alias{isccNames}
\alias{colorNames}

\title{Standard Names for Colors}
\description{
  The Inter-Society Color Council, in cooperation with the United States
  National Bureau of Standards, developed a list of 267 standardized
  color names.  Many software tools (including R) also use a
  (non-standardized) list of color names derived from the original X11
  list on early UJNIX systems. We provide tools to convert hexadecimal
  colors to both sets of names.
}
\usage{
isccNames(colorset)
colorNames(colorset)
}
\arguments{
  \item{colorset}{A character vector containing hexadecimal
    representations of colors.}
}
\details{
  Each of the ISCC-NBS 267 standard color names is represented by the
  centroid of a region of CIE L*u*v* color space, all of whose points
  should be given the same name.  Each of the color names listed by the
  \code{\link{colors}} function has an associated RGB color that can
  also be converted to L*u*v* space. These functions take colors
  represented in the common hexadecimal notation, maps them into L*u*v*
  color space, and assigns the name of the nearest ISCC centroid or
  UNIX/X11/R color. 
}
\value{
  A character string containing the standard color name nearest (in CIE
  L*u*v* color space) to each input color.
}
\author{Kevin R. Coombes <krc@silicovore.com>}
\references{
  Kelly KL. Twenty-Two Colors of Maximum Contrast. Color Eng., 1965;
  3:26--7. 

  Also see the Inter-Society Color Council web site
  (\url{http://www.iscc.org/}) and the Texas
  Precancel Club (\url{http://tx4.us/nbs-iscc.htm}). 
}
\seealso{
  \code{\link{iscc}}, \code{\link{colors}}.
}
\examples{
data(alphabet)
isccNames(alphabet)
colorNames(alphabet)
}
\keyword{color}
