memberPlot <- function(bindat, pal = NULL,
                       xlab = "Members", ylab = "Categories", ...) {
  ## Ensure that it is really a binary matrix
  if(any(is.na(bindat))) stop("Data contains mssing values.")
  if(!all(bindat %in% 0:1)) stop("Matrix values must equal '0' or '1'.")
  if(is.null(pal)) {
    pal <- c("gray85",
             light.colors(24)[c(4, 3, 23, 9, 24,
                                7, 18, 11, 14, 5,
                                2, 15, 16, 21)],
             "purple3")
  }
  while(length(pal) < nrow(bindat)) pal <- c(pal, pal[-1])
  # rows are categories, columns are samples

  counts <- apply(bindat, 1, sum)  # number of members of each category
  M <- 1:nrow(bindat)
  bindat <- sweep(bindat, 1, M, "*")
  bindat <- bindat[rev(order(counts)),] # sort rows by membership
  
  # order columns successively by row membership
  bd <- as.list(as.data.frame(t(bindat))) # temporary, for ordering
  oo <- do.call(order, bd)
  bindat <- bindat[, rev(oo)]
  ocount <- apply(bindat, 2, sum)
  bindat <- bindat[, ocount > 0]
  image(1:ncol(bindat), 1:nrow(bindat), t(bindat),
        col = pal[1:(1+nrow(bindat))],
        xlab = xlab, ylab = ylab, yaxt = "n", ...)
  mtext(rownames(bindat), side =2, las = 2, at = 1:nrow(bindat), line = 1)
  text(ncol(bindat)/2, 1:nrow(bindat),
       paste("N =", apply(bindat > 0, 1, sum)),
       font = 2, cex=1.2)
  invisible(bindat)
}

