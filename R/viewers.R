# Copyright (C) Kevin R. Coombes, 2016

invertColors <- function(...) {
  par(bg="black", fg="white",
      col.axis="white", col.lab="white",
      col.main="white", col.sub="white", ...)
}

rancurves <- function(colorset, ...) {
  plot(c(-2*pi, 2*pi), c(-5,5), type="n", xlab="Angle", ylab="Intensity", ...)
  x <- seq(-2*pi, 2*pi, length=500)
  L <- length(colorset)
  phase <- runif(L, 0, 2*pi)
  ampl <- rnorm(L, 0, 2)
  for (i in 1:L) {
    lines(x, ampl[i] * cos(phase[i] + x), col=colorset[i], lwd=2)
  }
  invisible(colorset)
}

ranpoints <- function(colorset, N=10, ...) {
  L <- length(colorset)
  mycols <- rep(colorset, each=N)
  x <- rep(rnorm(L, 0, 3), each=N) + rnorm(L*N)
  y <- rep(rnorm(L, 0, 3), each=N) + rnorm(L*N)
  plot(x, y, pch=16, col=mycols, ...)
  invisible(colorset)
}

uvscatter <- function(colorset, main=deparse(substitute(colorset)), ...) {
  luvmat <- as(hex2RGB(colorset), "LUV")
  x <- luvmat@coords
  plot(x[,2], x[,3], pch=16, col=colorset, cex=2,
       xlab="U", ylab="V", main=main, ...)
  invisible(colorset)
}

luminance <- function(colorset, main=deparse(substitute(colorset)), ...) {
  luvmat <- as(hex2RGB(colorset), "LUV")
  x <- luvmat@coords
  ox <- order(x[,1])
  plot(x[ox,1], pch=16, col=colorset[ox], cex=2,
       ylab="Luminance", main=main, ...)
  invisible(colorset)
}

plothc <- function(colorset, main=deparse(substitute(colorset)), ...) {
  luvmat <- as(hex2RGB(colorset), "LUV")
  x <- luvmat@coords
  euc <- dist(x, "euclid")
  hd <- hclust(euc, "ward.D2")
  plot(hd, hang = -1, labels = rep("", length(colorset)), 
       main = main, ...)
  mtext(names(colorset), side = 1, line = 0, at = order(hd$order), 
        col = colorset, las = 2)
  invisible(hd)
}

plotpc <- function(colorset, main=deparse(substitute(colorset)), ...) {
  luvmat <- as(hex2RGB(colorset), "LUV")
  x <- luvmat@coords
  pc <- princomp(x)
  plot(pc$scores[,1], pc$scores[,2],
       pch=16, col=colorset, cex=2,
       xlab="PC1", ylab="PC2", main=main, ...)
  invisible(pc)
}

p3d <- function(colorset, main=deparse(substitute(colorset))) {
  if (rgl.cur() == 0)  open3d(windowRect=c(40, 40, 840, 840))
  luvmat <- as(hex2RGB(colorset), "LUV")
  x <- luvmat@coords
  plot3d(x)
  spheres3d(x, radius=10, col=colorset, shininess=100)
  invisible(colorset)
}

swatch <- function(colorset, main=deparse(substitute(colorset))) {
  L <- length(colorset)
  pts <- barplot(rep(1, L), col=colorset, main=main, yaxt='n')
  text(pts, 0.5, names(colorset), srt=90)
  invisible(pts)
}

swatchHue <- function(colorset,
                        main=paste(deparse(substitute(colorset)),
                                   ", by Hue", sep="")) {
  luv <- as(hex2RGB(colorset), "HSV")
  tink <- luv@coords[,1]
  hh <- colorset[order(tink)]
  swatch(hh)
}

swatchLuminance <- function(colorset,
                        main=paste(deparse(substitute(colorset)),
                                   ", by Luminance", sep="")) {
  luv <- as(hex2RGB(colorset), "LUV")
  tink <- luv@coords[,1]
  hh <- colorset[order(tink)]
  swatch(hh)
}

ranswatch  <- function(colorset,
                        main=deparse(substitute(colorset))) {
  L <- length(colorset)
  scramble <- sample(colorset)
  swatch(scramble)
}

turnGray <- function(colorset) {
  temp <- as(hex2RGB(colorset), "LUV")
  tc <- temp@coords
  luv <- LUV(tc[,1], 0, 0)
  grayed <- hex(luv)
  names(grayed) <- names(colorset)
  grayed
}

computeDistances <- function(colorset) {
  # work in LUV space
  luvmat <- as(hex2RGB(colorset), "LUV")
  # take the fist color as the starting point
  selected <- 1
  mind <- d2(selected, luvmat)
  dd <- NULL
  # loop through, finding the most well-seaprated color at each iteration
  for (i in 2:length(colorset)) {
    idx <- which(mind==max(mind))[1]
    dd <- c(dd, sqrt(mind[idx]))
    mind <- upd(idx, luvmat, mind)
    selected <- c(selected, idx)
  }
  list(colors=colorset[selected], distances=dd)
}

plotDistances <- function(colorset, main=deparse(substitute(colorset)), ...) {
  luvd <- computeDistances(colorset)
  dd <- luvd$distances
  plot(dd, main=main,
       xlab="Index", ylab="Distance in L*u*v* space", ...)
  abline(h=40)
  invisible(luvd)
}
