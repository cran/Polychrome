##########################################
## manual conversion between sRGB and RGB
srgb2rgb <- function(x) {
  a <- 0.055
  ifelse(x <= 0.04045,
         x/12.92,
         ((x + a)/(1+a))^2.4)
}

rgb2srgb <- function(x) {
  a <- 0.055
  ifelse(x <= 0.0031308,
         12.92*x,
         (1+a)*x^(1/2.4) - a)
}

##########################################
## Transformation matrices

## convert RGB to XYZ space (CIE uses 4-digit approxiomations)
RGB2XYZ <- matrix(c(0.4124, 0.3576, 0.1805,
              0.2126, 0.7152, 0.0722,
              0.0193, 0.1192, 0.9505),
            3, 3, byrow=TRUE)
XYZ2RGB <- round(solve(RGB2XYZ), 4)
## they also write (X, Y, Z)^t = M * (R, G, B)^t

## equal energy; rows sum to 1
eeXYZ2LMS <- matrix(c( 0.38971, 0.68898, -0.07868,
                      -0.22981, 1.18340,  0.04641,
                      0, 0, 1), 3, 3, byrow=TRUE)
eeLMS2XYZ <- round(solve(eeXYZ2LMS), 5)
## normalized to D65 daylight illumination
d65XYZ2LMS <- matrix(c( 0.4002, 0.7076, -0.0808,
                       -0.2263, 1.1653, 0.0457,
                      0, 0, 1), 3, 3, byrow=TRUE)
d65LMS2XYZ <- round(solve(d65XYZ2LMS), 5)


##########################################
## We assume that teh default behavior is to convert
## sRGB coordinates to (erqual energy) LMS coordinates
forward <- function(x) {
  srgb2rgb(x) %*% t(RGB2XYZ) %*% t(eeXYZ2LMS)
}

## the round trip isn't completely symmetric, since we
## have to truncate to get back into the 0-1 limitations
## of visible space
backward <- function(x) {
  y <- rgb2srgb(x %*% t(eeLMS2XYZ) %*% t(XYZ2RGB) )
  y[y < 0] <- 0
  y[y > 1] <- 1
  y
}

##########################################
## Color deficit transformation matrices

## Deuteranope (most common form of red/green color deficit)
lms2lmsd <- matrix(c(1,        0, 0,
                     0.494207, 0, 1.24827,
                     0,        0, 1), 3, 3, byrow=TRUE)

## Protanope (another form of red/green color deficit)
lms2lmsp <- matrix(c(0, 2.02344, -2.52581,
                     0, 1,        0,
                     0, 0,        1), 3, 3, byrow=TRUE)

## Transformation matrix for Tritanope (a blue/yellow deficit - very rare)
lms2lmst <- matrix(c(1,         0,        0,
                     0,         1,        0,
                     -0.395913, 0.801109, 0), 3, 3, byrow=TRUE)

##########################################


colorDeficit <- function(rgb,
                      target = c("deuteranope", "protanope", "tritanope")) {
### handle different kinds of inputs
  ## start with a class from 'colorspace'
  if (returnObject <- inherits(rgb, "sRGB")) {
    obj <- rgb
    rgb <- rgb@coords
  }
  ## character strings are assumed to be hexadecimnal colors
  if (returnHex <- is.character(rgb)) {
    obj <- hex2RGB(rgb)
    rgb <- obj@coords
  }

### What kind of transformation do we want?
  target <- match.arg(target)
  xform <- switch(target,
                  deuteranope = lms2lmsd,
                  protanope = lms2lmsp,
                  tritanope = lms2lmst
                  )
### MAIN ALGORITHM
  deficit <- backward(forward(rgb) %*% t(xform))

### Give the user what (s)he wants.
  if (returnHex | returnObject) {
    obj@coords <- deficit
    deficit <- obj
  }
  if (returnHex) deficit <- hex(deficit)
  deficit
}

