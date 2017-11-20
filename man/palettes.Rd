\name{palettes}
\alias{palettes}
\alias{Color Palettes}
\alias{alphabet.colors}
\alias{kelly.colors}
\alias{green.armytage.colors}
\alias{palette36.colors}
\alias{glasbey.colors}
\alias{light.colors}
\alias{dark.colors}

\title{Polychrome Color Palettes}
\description{
  Five color palettes each containing at least 22 different,
  distinguishable colors.
}
\usage{
kelly.colors(n = 22)
glasbey.colors(n = 32)
green.armytage.colors(n = 26)
palette36.colors(n = 36)
alphabet.colors(n = 26)
light.colors(n = 24)
dark.colors(n = 24)
}
\arguments{
  \item{n}{An integer; the number of colors desired.}
}
\details{
  Kenneth Kelly, a physicist who worked at the United States National
  Bureau of Standards and chaired the Inter-Society Color Council
  Subcommittee on Color Names, made one of the earliest attempts to find
  a set of colors that could be easily distinguished when used in
  graphs. The \code{kelly.colors} function produces a palette from the
  22 colors that he produced, using his color names.  These are ordered
  so that the optimal contrast for any palette with fewer than 22 colors
  can be selected from the top of his list.

  Glasbey and colleagues used a sequential search algorithm in CIE LAB
  color space to create a palette of 32 well-separated colors.

  Paul Green-Armytage described a study growing out of a workshop held
  by the Colour Society of Australia in 2007 to test whether an alphabet
  composed of 26 distinguishable colors would serve in place of the usual
  symbols of the English alphabet. Each color is given a name starting
  with a different letter of the alphabet, which was found to make it
  easier for people to learn the association and read sentences written
  in color. The \code{green.armytage.colors} function produces palettes
  from his final color set, arranged in "alphabetical" order rather than
  by maximum contrast.

  Carter and Carter followed Kelly's article with a study that showed
  that "perceptual distinguishability" of colors was related to their
  Euclidean distance in the L*u*v* color space coordinates, as defined
  by the International Commisision on Illumination (CIE).  They also
  found that distinguishability falls off rapidly when the distance is
  less than about 40 L*u*v* units. We implemented a palette-construction
  algorithm based on this idea.  The \code{palette36.colors} function
  returns palettes from the resulting list of 36 colors, with names
  assigned using the ISCC-NSB standard.

  The \code{alphabet.colors} function uses the first 26 colors from
  "\code{palette36}" but assigns them names beginning with different
  letters of the English alphabet and reorders them accordingly.

  The \code{light.colors} and \code{dark.colors} functions use one of
  the two 24-color palettes (\code{Light24} or \code{Dark24}) customized
  to limit the luminance range.
}
\value{
  Each function returns a character vector of hexadecimal color values
  (such as "#EA9399"). Each color is assigned a name (such as
  "Strong_Pink").  The default value is the maximum number of colors
  available from the individual palette.
}
\author{Kevin R. Coombes <krc@silicovore.com>}
\references{
  Kelly KL. Twenty-Two Colors of Maximum Contrast. Color Eng., 1965;
  3:26--7. 

  Green-Armytage, P. A Colour Alphabet and the Limits of Colour
  Coding. Colour: Design and Creativity, 2010; 10:1--23.
  
  Carter RC, Carter EC. High-contrast sets of colors. Applied Optics,
  1982; 21(16):2936--9.
}
\seealso{
  \code{\link{createPalette}}
}
\examples{
palette36.colors(5)
kelly.colors(5)
alphabet.colors(7)
glasbey.colors(9)
green.armytage.colors(3)
light.colors(6)
dark.colors(11)
}
\keyword{color}
