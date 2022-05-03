\name{memberPlot}
\alias{memberPlot}

\title{Membership Plots}
\description{
  Membership plots are a graphical visualization of cross-membership of
  many individuals in many categories. Although Venn diagrams work well
  for this purpose when there are three or four categories, they can be
  difficiult to interpet, or even impossible to draw, with more
  categories. In this case, membership plots are more useful.
}
\usage{
memberPlot(bindat, features = NULL, pal = NULL, xlab = "Members",
           ylab = "Categories", \dots)
}
\arguments{
  \item{bindat}{A binary matrix where rows are categories, columns are
    members, 1 debnotes membership and 0 denotes nonmembership. Muissing
    data is not permitted.}
  \item{pal}{A character vector of colors. The first color is used for
    non-members; all other colors are used to denote different categories.}
  \item{features}{A numeric vector listing the number of features
    measured in each membership data set.}
  \item{xlab}{The usual graphical parameter.}
  \item{ylab}{The usual graphical parameter.}
  \item{\dots}{Additional plot parameters, especially "main" or "sub".}
}
\details{
  Membership plots are implemented as an image, where each row
  represents a different category and is shown in a different
  color. Non-membership is indicated by the same color regardless fo
  category; by dafault, we use "gray" for non-members. The data are
  sorted so that the number of members per category decreases from the
  bottom tot he top of the plot. They are also sorted so that membership
  in larger categories is prioritized from left to right.
}
\value{
  The transformed input matrix is returned invisibly. The main purpose
  of the function is teh "side-effect" or producing a plot.
}
\author{Kevin R. Coombes <krc@silicovore.com>}
\examples{
set.seed(98765)
categ <- 6
member <- 500
M <- matrix(rbinom(categ*member, 1, 0.5), nrow = categ)
rownames(M) <- LETTERS[1:categ]
memberPlot(M)
}
\keyword{color}
