library(Polychrome)

try( memberPlot( rep(0:1, 20)))                       # must be a matrix
try( memberPlot(matrix(LETTERS, ncol = 2 )) )         # must be numeric
try( memberPlot(matrix(1:30, ncol = 5), pal = palette()) ) # and binary

temp <- t(data.frame(
  A = c(rep(1, 500), rep(0, 100)),
  B = c(rep(1, 500), rep(0, 100)),
  C = c(rep(1, 300), rep(0, 300)),
  D = c(rep(1, 300), rep(0, 300)),
  E = c(rep(1, 300), rep(0, 300)),
  F = c(rep(1, 180), rep(0, 320), rep(1, 40), rep(0, 60)),
  G = c(rep(1, 180), rep(0, 320), rep(1, 40), rep(0, 60)),
  H = c(rep(1, 180), rep(0, 320), rep(1, 40), rep(0, 60))
))

set.seed(54321)
noise <- matrix(rbinom(prod(dim(temp)), 1, 0.05),
                ncol = ncol(temp))
meme <- 1*(temp | noise)
memberPlot(meme)
memberPlot(meme, ylab = "",
           features = c(123, 45, 18, 87, 75, 1234, 88, 999)) 
