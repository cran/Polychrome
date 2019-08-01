\name{createPalette}
\alias{createPalette}

\title{Creating New Color Palettes}
\description{
  Tool to create new palettes that are well separated in CIE L*u*v*
  color space.
}
\usage{
createPalette(N, seedcolors, prefix = "NC", range = c(30, 90),
              target = c("normal", "protanope", "deuteranope", "tritanope"),
              M = 50000)
}
\arguments{
  \item{N}{An integer, the size of the palette to create.}
  \item{seedcolors}{A character vector containing the hexadecimal
    representations of one or more colors.}
  \item{prefix}{A character string to be used as a prefix to numeric
    names of the colors.}
  \item{range}{A numeric vector limitng the range of allowed luminance
    values.}
  \item{target}{A character string indicating the kind of color vision
    for which the palette is intended.}
  \item{M}{An integer; the number of random colors to generate while
    creating palettes.}
}
\details{
  Carter and Carter showed that "perceptual distinguishability" of
  colors was related to their Euclidean distance in the L*u*v* color
  space coordinates, as defined by the International Commisision on
  Illumination (CIE). The \code{createPalette} function implements a
  greedy algorithm to find colors that are well-spread-out in L*u*v*
  space.  The algorithm begins by generating a random set of 50,000
  colors; these colors are restricted to those whose luminance lies
  between 30 and 90.  Then, given one or more starting colors, the
  algorithm finds the random color that maximizes the distance to the
  closest existing color point.  This process continues until \code{N}
  colors have been selected.
}
\value{
  A character string containing the hexadecimal representations of
  \code{N} colors that are well spread out in CIE L*u*v* color space.
}
\author{Kevin R. Coombes <krc@silicovore.com>}
\references{
  Carter RC, Carter EC. High-contrast sets of colors. Applied Optics,
  1982; 21(16):2936--9.

  Coombes KR, Brock G, Abrams ZB, Abruzzo LV. Polychrome: Creating and
  Assessing Qualitative Palettes with Many Colors. Journal of
  Statistical Software. 2019; 90(1):1--23.
}
\seealso{
  \code{\link{Color Palettes}}, \code{\link{colorDeficit}}.
}
\examples{
seed <- c("#ff0000", "#00ff00", "#0000ff")
mycolors <- createPalette(15, seed, prefix="mine")
swatch(mycolors)
}
\keyword{color}
