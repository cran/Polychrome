\name{invertColors}
\alias{invertColors}

\title{Inverting the Plot Device Color Scheme}
\description{
  Function to convert the default plot color scheme to white-on-black.
}
\usage{
invertColors(...)
}
\arguments{
  \item{\dots}{Other graphical parameters to be given to \code{par}.}
}
\details{
  This function changes the default color scheme of the current graphics
  device to white on black. Note that since \code{invertColors} resets
  the \code{bg} parameter, you should avoid passing in a new default
  value for the \code{col} parameter.
}
\value{
  It returns the original color scheme, which can be passed to the
  \code{par} command to restore the original values.
}
\author{Kevin R. Coombes <krc@silicovore.com>}
\seealso{
  \code{\link[graphics]{par}}
}
\examples{
opar <- invertColors()
plot(1:3, 4:6, pch=16)
par(opar)
}
\keyword{color}
