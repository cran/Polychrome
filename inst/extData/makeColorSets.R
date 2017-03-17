library(Polychrome)
library(colorspace)

####################################
# palette36

## should not be called by user code
createnew36 <- function() {
  # three seed colors
  ebony <- hex(LUV(30,0,0))
  iron <- hex(LUV(90, 0, 0))
  red <- hex(sRGB(0.8, 0, 0))
  # set random number seed for reproducibility. 
  set.seed(567629)
  createPalette(36, c(ebony, iron, red))
}
palette36 <- createnew36()
names(palette36) <- isccNames(palette36)
save(palette36, file="palette36.rda")

####################################
# alphacolors

createAlphabet <- function() {
  alphabet <- palette36[1:26]
  # want to assign not-completely-stupid names
  # start with names from Green-Armytage
  colsch <- green.armytage.colors()
  ga <- as(hex2RGB(colsch), "LUV")
  alpha <- as(hex2RGB(alphabet), "LUV")
  d3 <- function(y0) {
    temp <- sweep(ga@coords, 2, y0, "-")
    dist <- apply(temp^2, 1, sum)
    which(dist==min(dist))
  }
  m <- apply(alpha@coords, 1, d3)
  odd <- which(!(1:26 %in% m))
  m[duplicated(m)] <- odd
  names(alphabet) <- names(colsch)[m]
  # now manually swap things around
  # I know; this is really dumb.
  b <- which(names(alphabet)=='blue')
  v <- which(names(alphabet)=='violet')
  tu <- which(names(alphabet)=='turquoise')
  names(alphabet)[b] <- "turquoise"
  names(alphabet)[tu] <- "violet"
  names(alphabet)[v] <- "blue"
  rm(b, v, tu)
  s <- which(names(alphabet) == "sky")
  g <- which(names(alphabet) == "green")
  j <- which(names(alphabet) == "jade")
  k <- which(names(alphabet) == "khaki")
  o <- which(names(alphabet) == "orpiment")
  w <- which(names(alphabet) == "wine")
  names(alphabet)[g] <- "sea"
  names(alphabet)[k] <- "jade"
  names(alphabet)[o] <- "green"
  names(alphabet)[s] <- "wine"
  names(alphabet)[j] <- "O"
  names(alphabet)[w] <- "kingcrab"
  rm(s,g,o,k, j)
  names(alphabet)[names(alphabet)=="zinnia"] <- "orange"
  names(alphabet)[names(alphabet)=="lime"] <- "zinnia"
  names(alphabet)[names(alphabet)=="amethyst"] <- "lavender"
  names(alphabet)[names(alphabet)=="O"] <- "amethyst"
  names(alphabet)[names(alphabet)=="mallow"] <- "magenta"
  names(alphabet)[names(alphabet)=="damson"] <- "ultraviolet"
  names(alphabet)[names(alphabet)=="uranium"] <- "damson"
  x <- which(names(alphabet) == "xanthin")
  h <- which(names(alphabet) == "honeydew")
  names(alphabet)[x] <- "honey"
  names(alphabet)[h] <- "xanthin"
  rm(x,h)
  alphabet["ebony"] <- "#565656"
  alphabet["iron"] <- "#E2E2E2"
  alphabet[order(names(alphabet))]
}
alphabet <- createAlphabet()

save(alphabet, file="alphabet.rda")
