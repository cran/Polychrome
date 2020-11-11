## -----------------------------------------------------------------------------
evalVignette <- requireNamespace("ggplot2", quietly = TRUE)
knitr::opts_chunk$set(eval = evalVignette)

## ----newpal, fig.cap="A new palette of 40 colors.", fig.width = 8, fig.height=4----
library(Polychrome)
set.seed(935234)
P40 <- createPalette(40, c("#FF0000", "#00FF00", "#0000FF"), range = c(30, 80))
swatch(P40)

## ----reorder, fig.cap="A sorted palette of 40 colors.", fig.width = 8, fig.height=4----
P40 <- sortByHue(P40)
P40 <- as.vector(t(matrix(P40, ncol=4)))
swatch(P40)

## ----dename-------------------------------------------------------------------
names(P40) <- NULL

## ----simdata------------------------------------------------------------------
## Nine groups
NG <- 9
gp <- paste("G", 1:NG, sep = "")
length(gp)

## Four Subjects per group
## 36 Subjects = 9 groups * 4 subjects/group 
sid <- paste(rep(LETTERS[1:2], each=26), c(LETTERS, LETTERS), sep="")[1:(4*NG)]
length(sid)

## Three Reps per subject
## 108 Experiments
reps = factor(rep(c("R1", "R2", "R3"), times = length(sid)))
length(reps)

## Each experiment with measurements on four Days, so 432 data rows
daft <- data.frame(Day = rep(1:4, each=length(reps)),
                   Group = factor(rep(rep(gp, each=12), times = 4)),
                   Subject = factor(rep(rep(sid, each = 3), times=4)),
                   Rep = factor(rep(reps, times = 4)))
dim(daft)
summary(daft)

## ----variable-----------------------------------------------------------------
## Linear model with noise, ignoring group
beta <- runif(length(sid), 0.5, 2)
## "Measured" variable
attach(daft)
daft$variable <- rnorm(nrow(daft), 0, 0.2) + 1  + beta[as.numeric(Subject)]*Day
detach()

## ----fig.cap="A faceted plot, colored by subject.", fig.width=8, fig.height=8----
library(ggplot2)
ggplot(daft, aes(x = Day, y = variable, colour = as.factor(Subject))) +
  geom_point(aes(shape = as.factor(Rep)), size = 3) +
  geom_line(aes(linetype = as.factor(Rep)), size = 0.8) +
  facet_wrap(. ~ Group, ncol = 3)+
  theme_bw() + theme(legend.position="none")+
  scale_color_manual(values = P40)

