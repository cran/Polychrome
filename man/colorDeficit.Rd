\name{colorDeficit}
\alias{colorDeficit}

\title{Converting Colors to Illustrate Color Deficient Vision}
\description{
  Function to convert any palette to one that illustrates how it would
  appear to a person with a color deficit.
}
\usage{
colorDeficit(rgb, target = c("deuteranope", "protanope", "tritanope"))
}
\arguments{
  \item{rgb}{A color palette. Accepts hexademical representations, sRGB
class objects from the \code{colorspace} package, or three-column
sRGB matrices.}
  \item{target}{The kind of color deficit to simulate.}
}
\details{
  This function converts normal-vision color palettes into simulations
  that represent what is likely to be seen with one of the three kinds
  of color deficits.  Deuteranopes are red-blind, which is the most
common form of color deficit leading to an inability to ditinguish red
and green. Protanopes are geen-blind; this is the second most common
form of color-blindness and also leads to an inability to distinguiohg
red and green. Tritanopes are blue blind Thisn is the rarest fomr of
color blindness and leads to an inability to distinguish blue and yellow. 
}
\value{
  Returns a color palette in the same form as its input argument.
}
\author{Kevin R. Coombes <krc@silicovore.com>}
\seealso{
  \code{\link[colorspace]{color-class}}
}
\references{
  [1] http://www.vischeck.com/

  [2] Brettel H, Vienot F, Mollon JD. Computerized simulation of color appearance
for dichromats. J Opt Soc Am A Opt Image Sci Vis. 1997 Oct;14(10):2647-55. PubMed
PMID: 9316278.

  [3] Vienot F, Brettel H, Ott L, Ben M\'Barek A, Mollon JD. What do colour-blind
people see? Nature. 1995 Jul 13;376(6536):127-8. PubMed PMID: 7603561.
}
\examples{
alfa <- alphabet.colors(26)
def <- colorDeficit(alfa)
swatch(def)
}
\keyword{color}
