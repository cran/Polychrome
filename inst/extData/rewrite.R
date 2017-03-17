temp <- read.table("temp.txt", sep="\t", comment.char='', as.is=TRUE)

foo <- strsplit(temp$V1, "\\s")
ping <- unlist(lapply(foo, function(f) which(!is.na(as.integer(f)))))
counter <- as.integer(sapply(1:length(foo), function(i) {
  foo[[i]][ping[i]]
}))
summary(diff(counter)) # all 1's

shortName <- unlist(sapply(1:length(foo), function(i) {
  paste(foo[[i]][1:(ping[i]-1)], collapse="_")
}))

pong <- unlist(lapply(foo, function(f) {
  w <- which(f %in% c('see', 'not'))
  ifelse(length(w) > 0, w-1, length(f))
}))

longName <- unlist(sapply(1:length(foo), function(i) {
  f <- foo[[i]]
  paste(f[(ping[i]+1):pong[i]], collapse="_")
}))
ln <- strsplit(longName, ',')
longName <- unlist(lapply(ln, function(x) x[1]))

iscc <- data.frame(shortName, longName, Hex=temp$V2)
write.table(iscc, file="iscc.txt", sep="\t", row.names=FALSE, quote=FALSE)
save(iscc, file="iscc.rda")
