# Copyright (C) Kevin R. Coombes, 2016

colorNames <- function (colorset)  {
  colorset <- xform(colorset)
  # note: should only use 'col2rgb' for color() names
  rigby <- t(col2rgb(colors())) # R-G-B matrix of
  rownames(rigby) <- colors()   #  known color names
  csrgb <- sRGB(rigby/255)      # convert to colorpace
  csluv <- as(csrgb, "LUV")     # then to L*u*v*
  cc <- csluv@coords
  cc[is.na(cc)] <- 0            # handle 'black'
  d3 <- function(y0) {
    # find the closest match in L*u*v* space
    temp <- sweep(cc, 2, y0, "-")
    dist <- apply(temp^2, 1, sum)
    which(dist == min(dist))[1]
  }
  alpha <- as(hex2RGB(colorset), "LUV")
  m <- apply(alpha@coords, 1, d3)
  rownames(cc)[m]
}

isccNames <- function(colorset) {
  colorset <- xform(colorset)
  data("iscc", package="Polychrome", envir=environment())
  iscc <- get("iscc", envir=environment())
  munsell <- as(hex2RGB(iscc$Hex), "LUV")
  d3 <- function(y0) {
    temp <- sweep(munsell@coords, 2, y0, "-")
    dist <- apply(temp^2, 1, sum)
    which(dist==min(dist))
  }
  alpha <- as(hex2RGB(colorset), "LUV")
  m <- apply(alpha@coords, 1, d3)
  iscc$longName[m]
}

makePalette <- function(n, colorset) {
  if (n < 3) {
    warning("minimal value for n is 3, returning requested palette with 3 levels\n")
    n <- 3
  }
  L <-  length(colorset)
  if (n > L) {
    name <- deparse(substitute(colorset))
    warning(paste("n too large, allowed maximum for palette", 
                  name, "is", L), 
            "\nReturning the palette you asked for with that many colors\n")
    n <- L
  }
  colorset[1:n]
}
####################################
palette36.colors <- function(n=36) {
  data("palette36", package="Polychrome", envir=environment())
  palette36 <- get("palette36", envir=environment())
  return (makePalette(n, palette36))
}

####################################
alphabet.colors <- function(n=26) {
  data("alphabet", package="Polychrome", envir=environment())
  alphabet <- get("alphabet", envir=environment())
  return (makePalette(n, alphabet))
}

####################################
glasbey.colors <- function(n=32) {
  data("glasbey", package="Polychrome", envir=environment())
  glasbey <- get("glasbey", envir=environment())
  return (makePalette(n, glasbey))
}

####################################
# Kelly
kelly.colors <- function(n=22) {
  kelly <- c(
    white = "#f2f3f4",
    black = "#222222",
    yellow = "#f3c300",
    purple = "#875692",
    orange = "#f38400",
    lightblue = "#a1caf1",
    red = "#be0032",
    buff = "#c2b280",
    gray = "#848482",
    green = "#008856",
    purplishpink = "#e68fac",
    blue = "#0067a5",
    yellowishpink = "#f99379",
    violet = "#604e97",
    orangeyellow = "#f6a600",
    purplishred = "#b3446c",
    greenishyellow = "#dcd300",
    reddishbrown = "#882d17",
    yellowgreen = "#8db600",
    yellowishbrown = "#654522",
    reddishorange = "#e25822",
    olivegreen = "#2b3d26"
  )
  return (makePalette(n, kelly))
}

####################################
# Green-Armytage

green.armytage.colors <- function(n=26) {
  colsch <- list(
    amethyst=c(240, 163, 255),
    blue=c(0, 117, 220),
    caramel=c(153,63,0),
    damson=c(76, 0, 92),
    ebony=c(25, 25, 25),
    forest=c(0, 92, 49),
    green=c(43, 206, 72),
    honeydew=c(255, 204, 153),
    iron=c(128, 128, 128),
    jade=c(148, 255, 181),
    khaki=c(143, 124, 0),
    lime=c(157, 204, 0),
    mallow=c(194, 0, 136),
    navy=c(0, 51, 128),
    orpiment=c(25, 164, 5),
    pink=c(255, 168, 187),
    quagmire=c(66, 102, 0),
    red=c(255, 0, 16),
    sky=c(94, 241, 242),
    turquoise=c(0, 153, 143),
    uranium=c(224, 255, 102),
    violet=c(16, 10, 255),
    wine=c(153, 0, 0),
    xanthin=c(255, 255, 128),
    yellow=c(255, 225, 0),
    zinnia=c(255, 80, 0))
  R <- unlist(lapply(colsch, function(x) x[1]/255))
  G <- unlist(lapply(colsch, function(x) x[2]/255))
  B <- unlist(lapply(colsch, function(x) x[3]/255))
  alpha <- as(sRGB(R, G, B), "LUV")
  green.armytage <- hex(alpha)
  names(green.armytage) <- names(colsch)
  makePalette(n, green.armytage)
}


