# Copyright (C) Kevin R. Coombes, 2016

### Internal
xform <- function(colset, alpha=FALSE) {
  mat <- t(col2rgb(colset, alpha=alpha)/255)
  if (alpha) {
    value <- rgb(mat[, 1:3], alpha=mat[,4])
  } else {
    value <- rgb(mat[, 1:3])
  }
  if (is.null(names(colset))) {
    names(value) <- paste("X", 1:length(value), sep='')
  } else {
    names(value) <- names(colset)
  }
  value
}


invertColors <- function(...) {
  par(bg="black", fg="white",
      col.axis="white", col.lab="white",
      col.main="white", col.sub="white", ...)
}

rancurves <- function(colorset, ...) {
  colorset <- xform(colorset)
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
  colorset <- xform(colorset)
  L <- length(colorset)
  mycols <- rep(colorset, each=N)
  x <- rep(rnorm(L, 0, 3), each=N) + rnorm(L*N)
  y <- rep(rnorm(L, 0, 3), each=N) + rnorm(L*N)
  plot(x, y, pch=16, col=mycols, ...)
  invisible(colorset)
}

uvscatter <- function(colorset, main=deparse(substitute(colorset)), ...) {
  cset <- xform(colorset)
  luvmat <- as(hex2RGB(cset), "LUV")
  x <- luvmat@coords
  plot(x[,2], x[,3], pch=16, col=cset, cex=2,
       xlab="U", ylab="V", main=main, ...)
  invisible(cset)
}

luminance <- function(colorset, main=deparse(substitute(colorset)), ...) {
  cset <- xform(colorset)
  luvmat <- as(hex2RGB(cset), "LUV")
  x <- luvmat@coords
  ox <- order(x[,1])
  plot(x[ox,1], pch=16, col=cset[ox], cex=2,
       ylab="Luminance", main=main, ...)
  invisible(cset)
}

plothc <- function(colorset, main=deparse(substitute(colorset)), ...) {
  cset <- xform(colorset)
  luvmat <- as(hex2RGB(cset), "LUV")
  x <- luvmat@coords
  euc <- dist(x, "euclid")
  hd <- hclust(euc, "ward.D2")
  plot(hd, hang = -1, labels = rep("", length(cset)), 
       main = main, ...)
  mtext(names(cset), side = 1, line = 0, at = order(hd$order), 
        col = cset, las = 2)
  invisible(hd)
}

plotpc <- function(colorset, main=deparse(substitute(colorset)), ...) {
  cset <- xform(colorset)
  luvmat <- as(hex2RGB(cset), "LUV")
  x <- luvmat@coords
  pc <- princomp(x)
  plot(pc$scores[,1], pc$scores[,2],
       pch=16, col=cset, cex=2,
       xlab="PC1", ylab="PC2", main=main, ...)
  invisible(pc)
}

# added to allow external use of rgl
getLUV <- function(colorset) {
  cset <- xform(colorset)
  luvmat <- as(hex2RGB(cset), "LUV")
  list(coords = luvmat@coords, cset = cset)
}

p3d <- function(colorset, main=deparse(substitute(colorset)), ...) {
  y <- getLUV(colorset)
  x <- y$coords
  cset <- y$cset
  scatterplot3d(x, color = cset, pch = 16, ...)
  invisible(cset)
}

swatch <- function(colorset, main=deparse(substitute(colorset))) {
  cset <- xform(colorset)
  luvmat <- as(hex2RGB(cset), "LUV")
  x <- luvmat@coords
  labelcols <- c("white", "black")[1 + 1*(x[,1] > 50)]
  L <- length(cset)
  pts <- barplot(rep(1, L), col=cset, main=main, yaxt='n')
  text(pts, 0.5, names(cset), srt=90, col=labelcols)
  invisible(pts)
}

swatchHue <- function(colorset,
                        main=paste(deparse(substitute(colorset)),
                                   ", by Hue", sep="")) {
  cset <- xform(colorset)
  luv <- as(hex2RGB(cset), "HSV")
  tink <- luv@coords[,1]
  hh <- cset[order(tink)]
  swatch(hh, main=main)
}

swatchLuminance <- function(colorset,
                        main=paste(deparse(substitute(colorset)),
                                   ", by Luminance", sep="")) {
  cset <- xform(colorset)
  luv <- as(hex2RGB(cset), "LUV")
  tink <- luv@coords[,1]
  hh <- cset[order(tink)]
  swatch(hh, main=main)
}

ranswatch  <- function(colorset,
                        main=deparse(substitute(colorset))) {
  cset <- xform(colorset)
  L <- length(cset)
  scramble <- sample(cset)
  swatch(scramble, main=main)
}

luvDistances <- function(colorset) {
  colorset <- xform(colorset)
  if (any(dup <- duplicated(colorset))) {
    reserved <- colorset[dup]
    colorset <- colorset[!dup]
  }
  # work in LUV space
  luvmat <- as(hex2RGB(colorset), "LUV")
  if(any(is.na(luvmat@coords))) {
    luvmat@coords[is.na(luvmat@coords)] <- 0
  }
  # take the first color as the starting point
  selected <- 1
  names(selected) <- names(colorset)[1]
  mind <- d2(selected, luvmat)
  dd <- NULL
  # loop through, finding the most well-seaprated color at each iteration
  for (i in 2:length(colorset)) {
    idx <- which(mind==max(mind))[1]
    dd <- c(dd, sqrt(mind[idx]))
    mind <- upd(idx, luvmat, mind)
    selected <- c(selected, idx)
  }
  dd <- c(max(dd), dd)
  names(dd)[1] <- names(colorset)[1]
  colors <- colorset[selected]
  if (any(dup)) {
    colors <- c(colors, reserved)
    extra <- rep(0, length(reserved))
    names(extra) <- names(reserved)
    dd <- c(dd, extra)
  }
  list(colors=colors, distances=dd)
}

computeDistances <- function(colorset) {
  luvDistances(colorset)$distances
}

plotDistances <- function(colorset, main=deparse(substitute(colorset)), pch=16, ...) {
  cset <- xform(colorset)
  luvd <- luvDistances(cset)
  dd <- luvd$distances
  plot(dd, main=main,
       xlab="Index", ylab="Distance in L*u*v* space",
       col=luvd$colors, pch=pch, ...)
  abline(h=40)
  invisible(luvd)
}
