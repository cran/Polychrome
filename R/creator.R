# Copyright (C) Kevin R. Coombes, 2016

minOf2 <- function(a, b) (a + b - abs(a-b))/2

d1 <- function(y0, luv) {
  temp <- sweep(luv@coords, 2, y0, "-")
  apply(temp^2, 1, sum)
}

d2 <- function(idx, luv) {
  d1(luv@coords[idx,], luv)
}

upd <- function(idx, luv, mind) {
  temp <- d2(idx, luv)
  minOf2(mind, temp)
}

createPalette <- function(N, seedcolors, prefix="NC", range=c(30, 90), M=50000) {
  DARK <- min(range)
  LIGHT <- max(range)
  bigset <- as.matrix(data.frame(R=sample(0:255, M, replace=TRUE),
                                 G=sample(0:255, M, replace=TRUE),
                                 B=sample(0:255, M, replace=TRUE)))/255
  bigrgb <- RGB(bigset)
  bigluv <- as(bigrgb, "LUV")
  toodark <- bigluv@coords[,1] < DARK
  toolight <- bigluv@coords[,1] > LIGHT
  goldilocks <- !toodark & !toolight
  bigset <- bigset[goldilocks,]
  bigrgb <- RGB(bigset)
  bigluv <- as(bigrgb, "LUV")

  s <- as(hex2RGB(seedcolors[1]), "LUV")@coords
  seedcolors <- seedcolors[-1]
  distance <- d1(s, bigluv)
  selected <- which(distance == min(distance))
  mind <- d2(selected, bigluv)
  if (length(seedcolors) > 0) {
    repeat {
      s <- as(hex2RGB(seedcolors[1]), "LUV")@coords
      seedcolors <- seedcolors[-1]
      distance <- d1(s, bigluv)
      idx <- which(distance == min(distance))
      selected <- c(selected, idx)
      mind <- upd(idx, bigluv, mind)
      if (length(seedcolors) == 0) break
    }
  }
  for (i in (1+length(selected)):N) {
    idx <- which(mind == max(mind))[1]
    mind <- upd(idx, bigluv, mind)
    selected <- c(selected, idx)
  }
  nonsense <- bigluv[selected,]
  newcols <- hex(nonsense)
  names(newcols) <- paste(prefix, 1:N, sep="")
  newcols
}  
