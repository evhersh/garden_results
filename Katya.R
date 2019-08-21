##### Load Packages #####
library(tidyverse)

##### Load Data #####
flow.dat <- read.csv(file.choose(), header=TRUE, sep=",", strip.white=TRUE, na.string=c("NA", ""))

##### Factors/Levels/etc #####
flow.dat$ms <- factor(flow.dat$ms, levels=c("S", "A"))
flow.dat$pop <- factor(flow.dat$pop, levels=c())

##### Summary / Exploration #####

##### Models #####

##### Plotting #####
boxplot(flow.dat$mean ~ flow.dat$ms)
plot(flow.dat$mean ~ flow.dat$cv)