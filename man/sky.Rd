\name{sky-colors}
\alias{sky-colors}
\alias{Spectral Karyotyping}
\docType{data}
\title{The 32-color Glasbye palette}
\description{
  A palette composed of 32 distinct colors.
}
\usage{data("sky-colors")}
\format{
  A character string of length 24.
}
\details{
  A character vector containing hexadecimal color representations of 24
  distinctive colors that are well separated in the CIE L*u*v* color
  space.
}
\source{
  Spectral karyotyping is a standard cytogenetic technology to map
  chromosomes using multiple different colored fluorescent probes. The
  probes for each individual human chromosome use different combinations
  of one or more probes. The standard software to display the result
  uses a false-color mapping to a 24-color paletee. Coombes et al. (see
  references) showed that the common colorm palette includes several
  colors that are hard to distinguish, then genrated this palette as a
  suggested replacement. The copde to generate the palette (and to match
  it as closely as possible with the older standard) can be found inth e
  appendix to that paper
}
\references{
  Coombes KR, Brock G, Abrams ZB, Abruzzo LV. Polychrome: Creating and
  Assessing Qualitative Palettes with Many Colors. Journal of
  Statistical Software. 2019; 90(1):1--23.
}
\examples{
data("sky-colors")
sky.colors
}
\keyword{datasets}
\keyword{color}
