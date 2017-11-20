library(Polychrome)
set.seed(9641)
Dark24 <- createPalette(24, c("#2A95E8", "#E5629C"), range = c(10, 60),
   M = 100000)

set.seed(701138)
Light24 <- createPalette(24, "#ff0000", range = c(55, 95), M = 100000)

ddir <- file.path("..", "..", "data")
save(Dark24, file = file.path(ddir, "Dark24.rda"))
save(Light24, file = file.path(ddir, "Light24.rda"))
