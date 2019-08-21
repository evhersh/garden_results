# aster model

##########
# Packages

library(MASS)
library(gdata)
library(plotrix)
library(aster)
library(lattice)
library(dplyr)
library(ggplot2)
library(aster)

##### load data #####

load("aster_example_Hersh.RData")

##### data prep #####

# replace all NA's with 0's
aster.dat[is.na(aster.dat)] <- 0

# reshape GOING WITH ONLY FLOWERING FOR NOW, ALSO REMOVING 2014
vars <- c("surv.2015", "surv.2016", "surv.2017", "surv.2018", "surv.2019",
          "flower.2015", "flower.2016", "flower.2017", "flower.2018", "flower.2019",
          "budnum.2015", "budnum.2016", "budnum.2017", "budnum.2018", "budnum.2019")

redata <- reshape(aster.dat, varying=list(vars), direction="long",
                  timevar="varb", times=as.factor(vars), v.names="resp") 

redata <- data.frame(redata, root = 1)

pred <- c(0, 1, 2, 3, 4,
          1, 2, 3, 4, 5,
          6, 7, 8, 9, 10)

fam <- c(1, 1, 1, 1, 1, 
         1, 1, 1, 1, 1,
         3, 3, 3, 3, 3)

sapply(fam.default(), as.character)[fam]

# something something "no naked predictors"?
layer <- gsub("[0-9]", "", as.character(redata$varb))
unique(layer)

redata <- data.frame(redata, layer = layer)

with(redata, class(layer))

fit <- as.numeric(layer == "budnum.")
unique(fit)

redata <- data.frame(redata, fit=fit)
with(redata, class(fit))

##### Fit Model #####

aout1 <- aster(resp ~ varb + fit : (ms*garden), pred=pred, fam=fam, varvar=varb, idvar=id, root=root, data = redata)
summary(aout1, show.graph=TRUE, info.tol=1e-20) # works if I set info.tol really high...

aout2 <- aster(resp ~ varb + fit : ms, pred=pred, fam=fam, varvar=varb, idvar=id, root=root, data = redata)
summary(aout2, show.graph=TRUE, info.tol = 1e-20) # fiddling with info.tol doesn't help...

##### direction of recession check? #####

info.tol <- 1e-12
infomat <- aout1$fisher
fred <- eigen(infomat, symmetric = TRUE)
sally <- fred$values < max(fred$values) * info.tol
foo <- zapsmall(fred$vectors[ , sally])

modmat <- aout1$modmat
modmat <- matrix(as.vector(modmat), ncol = dim(modmat)[3])

bar <- modmat %*% foo
