# An alternative set of a dozen safe colors.
library(Polychrome)
set.seed(4528)
tempPal <- createPalette(40, c("#010101", "#FFFFFF"), range=c(25, 80))
tempPal <- tempPal[-(1:2)]
swatch(tempPal)

tempPal.deut <- colorDeficit(tempPal, "deut")
tempPal.prot <- colorDeficit(tempPal, "prot")
tempPal.trit <- colorDeficit(tempPal, "trit")

shift <- function(i, k=length(tempPal)) c(i, 1:(i-1), (1+i):k)
co <- shift(13)
pd <- computeDistances(tempPal.deut[co])
pp <- computeDistances(tempPal.prot[co])
pt <- computeDistances(tempPal.trit[co])

rd <- rank(pd)[order(names(pd))]
rp <- rank(pd)[order(names(pp))]
rt <- rank(pd)[order(names(pt))]
score <- 2*rd + 1.5*rp + rt

x <- tempPal[names(rev(sort(score)))][1:12]
plotDistances(x,cex=2)
y <- colorDeficit(x, "deut")
z <- colorDeficit(x, "prot")
w <- colorDeficit(x, "trit")

opar <- par(mfrow=c(2,2))
swatch(x, main="Normal")
swatch(y, main="Deuteranope")
swatch(z, main="Protanope")
swatch(w, main="Tritanope")
par(opar)

names(x) <- colorNames(x)
safeColors <- x

plotDistances(safeColors,cesafeColors=2)

opar <- par(mfrow=c(2,2))
swatch(safeColors)
swatchHue(safeColors)
swatchLuminance(safeColors)
ranswatch(safeColors)
par(opar)

rancurves(safeColors, lwd=3)
ranpoints(safeColors, cesafeColors=1.5)
uvscatter(safeColors)
luminance(safeColors)
plothc(safeColors)
plotpc(safeColors)
