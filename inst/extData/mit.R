set.seed(9641)
dark24 <- createPalette(24, c("#2A95E8", "#E5629C" ), range = c(10, 60), M = 100000)
chrColors <- rev(dark24)
#set.seed(9641)
#chrColors <- dark24[c(sample(c(1:16, 18:23)), 24, 17)]
names(chrColors) <- paste("chr", c(1:22, "X", "Y"))

amlPC <- read.csv(system.file("extData/amlPC.csv", package = "Polychrome"), row.names = 1)

makeleg <- function(x, y, dx, dy = 0, chrColors, pch) {
  lab <- c(1:22, "X", "Y")
  legend(x, y, lab[1:8], col = chrColors[1:8], pch=15)
  legend(x + dx, y + dy, lab[9:16], col = chrColors[9:16], pch=pch)
  legend(x + 2*dx, y + 2*dy, lab[17:24], col = chrColors[17:24], pch=pch)
}

plot(amlPC[,2], amlPC[,3],
     pch=15, col=chrColors[amlPC[,1]],
     xlab="Loadings, PC1", ylab="Loadings, PC2")
makeleg(0.05, -0.07, 0.01, 0, chrColors, 15)

f <- function(i) {
  pick <- amlPC[,1] == i
  points(amlPC[pick,2], amlPC[pick,3], col=chrColors[i], cex=2)
}

f(1)
f(2)
f(3)

f(24)
