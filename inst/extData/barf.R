library(Polychrome)
library(colorspace)
data(palette36)
names(palette36) <- paste("PC", 1:36)
p36 <- as(hex2RGB(palette36), "LUV")@coords

suppressWarnings( RNGversion("3.5.0") )
set.seed(191088)
set.seed(540238)
loopseeds <- sample(100000:999999, 65000) # first run

ebony <- hex(LUV(30,0,0))
iron <- hex(LUV(90, 0, 0))
red <- hex(sRGB(0.8, 0, 0))

  # set random number seed for reproducibility. 
  set.seed(S <- 714303) # second best so far
  set.seed(S <- 456614) # best so far
##  set.seed(S <- 991635)
##  set.seed(S  <-  38437)
  mytry <- createPalette(36, c(ebony, iron, red))
  m36 <- as(hex2RGB(mytry), "LUV")@coords
  dis  <- sqrt(sum((p36 - m36)^2))
  foo <- rbind(p36, m36)
  baffle <- data.frame(I=NA, J=NA)[-1,]
  D <- as.matrix(dist(foo))[1:36, 37:72]
  while(nrow(D) > 1) {
    I <- which.min(apply(D, 1, min))
    J  <- which.min(D[I,])
    baffle <- rbind(baffle,
                    data.frame(I=rownames(D)[I], J=colnames(D)[J]))
    D <- D[-I, -J, drop=FALSE]
  }
  baffle <- rbind(baffle,
                  data.frame(I=rownames(D)[1], J=colnames(D)[1]))
  rownames(baffle) <- baffle$I
  baffle  <- baffle[names(palette36),]
  baz <- mytry[as.character(baffle$J)]
  b36 <- as(hex2RGB(baz), "LUV")@coords
  bdis  <- sqrt(sum((dif  <- b36 - p36)^2))

dev.set(2)
opar <- par(mfrow=c(3,1))
swatch(palette36)
swatch(baz, main=paste("baz,", S))
swatch(mytry)
par(opar)

## windows()
if(FALSE) {
  dev.set(3)
  both <- c(palette36, mytry)
  uvscatter(both)
  swatchHue(both)
  p3d(both, cex.s=3)

  opar <- par(mfrow=c(1,2))
  plotDistances(palette36, cex=3)
  plotDistances(mytry, cex=3)
  par(opar)
}

data.frame(P = colorNames(palette36), B = colorNames(baz))
