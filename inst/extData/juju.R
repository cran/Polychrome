library(Polychrome)
library(colorspace)
data(palette36)
names(palette36) <- paste("PC", 1:36)
p36 <- as(hex2RGB(palette36), "LUV")@coords

suppressWarnings( RNGversion("3.5.0") )
set.seed(540238)
loopseeds <- sample(100000:999999, 65000) # first run
loopseeds <- sample(100000:999999, 65000) # second run
loopseeds <- 11001:99999 # third run
loopseeds <- sample(100000:999999, 65000) # fourth run
RN  <- 4

ebony <- hex(LUV(30,0,0))
iron <- hex(LUV(90, 0, 0))
red <- hex(sRGB(0.8, 0, 0))

counter  <- 0
currBS <- currS <- 0
currB <- currDist  <-  999999
flag <- FALSE

tf <- "tracker.Rda"
if(file.exists(tf)) {
  load(tf)
} else {
  tracker <- data.frame(Index=0, Seed = NA , Distance = NA,
                         Rearranged = NA, Run=NA)
}

for (S in loopseeds) {
  counter  <-  counter + 1
  if (counter %% 100 == 0) cat("tries = ", counter, "\n", file = stderr())
  # set random number seed for reproducibility. 
  set.seed(S)
  mytry <- createPalette(36, c(ebony, iron, red))
  m36 <- as(hex2RGB(mytry), "LUV")@coords
  dis  <- sqrt(sum((p36 - m36)^2))
  if (dis < currDist) {
    currS <- S
    currDist  <-  dis
    cat(S, "distance =", currDist, "\n", file=stderr())
    flag <- TRUE
  }
  if (dis < 30) stop("could be golden")
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
  bdis  <- sqrt(sum((b36 - p36)^2))
  if (bdis < currB) {
    currBS <- S
    currB  <-  bdis
    cat(S, "B distance =", currB, "\n", file=stderr())
    flag <- TRUE
  }
  if (flag) {
    tracker <- rbind(tracker,
                      data.frame(Index=counter, Seed = S ,
                                 Distance = dis, Rearranged = bdis, Run=RN))
    save(tracker, file = tf)
    write.csv(tracker, file="tracker.csv", row.names=FALSE)
    flag  <-  FALSE
    }
}
