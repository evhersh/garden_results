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

aster.dat <<-read.csv("garden_data_final_wide_v2.csv", stringsAsFactors = TRUE, strip.white = TRUE, na.string = c("NA",""))

##### data prep #####
# log transform leaf lengths
aster.dat$leaflen.2014 <- log(aster.dat$leaflen.2014)
aster.dat$leaflen.2015 <- log(aster.dat$leaflen.2015)
aster.dat$leaflen.2016 <- log(aster.dat$leaflen.2016)
aster.dat$leaflen.2017 <- log(aster.dat$leaflen.2017)
aster.dat$leaflen.2018 <- log(aster.dat$leaflen.2018)
aster.dat$leaflen.2019 <- log(aster.dat$leaflen.2019)

sd(aster.dat$leaflen.2014, na.rm=TRUE) #0.5052409
sd(aster.dat$leaflen.2015, na.rm=TRUE) #0.5465294
sd(aster.dat$leaflen.2016, na.rm=TRUE) #0.4890473
sd(aster.dat$leaflen.2017, na.rm=TRUE) #0.4404425
sd(aster.dat$leaflen.2018, na.rm=TRUE) #0.4589257
sd(aster.dat$leaflen.2019, na.rm=TRUE) #0.5751151

# replace all NA's with 0's
aster.dat[is.na(aster.dat)] <- 0

aster.AA1 <- subset(aster.dat, garden=="AA1")
aster.AO1 <- subset(aster.dat, garden=="AO1")
# misc checking
  #names(aster.dat)
  #sapply(aster.dat, class) # not sure if all response variables should be "numeric" or "integer"?

# reshape
  # vars <- c("surv.2014", "surv.2015", "surv.2016", "surv.2017", "surv.2018", "surv.2019", 
  #           "flower.2014", "flower.2015", "flower.2016", "flower.2017", "flower.2018", "flower.2019",
  #           "budnum.2014", "budnum.2015", "budnum.2016", "budnum.2017", "budnum.2018", "budnum.2019",
  #           "leafnum.2014", "leafnum.2015", "leafnum.2016", "leafnum.2017", "leafnum.2018", "leafnum.2019",
  #           "leaflen.2014", "leaflen.2015", "leaflen.2016", "leaflen.2017", "leaflen.2018", "leaflen.2019")

# reshape GOING WITH ONLY FLOWERING FOR NOW, ALSO REMOVING 2014
vars <- c("surv.2015", "surv.2016", "surv.2017", "surv.2018", "surv.2019",
          "flower.2015", "flower.2016", "flower.2017", "flower.2018", "flower.2019",
          "budnum.2015", "budnum.2016", "budnum.2017", "budnum.2018", "budnum.2019")

redata <- reshape(aster.AO1, varying=list(vars), direction="long",
                  timevar="varb", times=as.factor(vars), v.names="resp") # a couple id's were duplicated in the original file, so I renamed them.
                # these are the ids that were duplicated : ‘EH0011.30’, ‘EH0114.11’, ‘EH0195.6’, ‘EH0242.ADD’, ‘EH0244.26’

  #names(redata)

# add root?
redata <- data.frame(redata, root = 1)
  #names(redata)

# structure of model

    # pred <- c(0, 1, 2, 3, 4, 5,
    #           1, 2, 3, 4, 5, 6, 
    #           7, 8, 9, 10, 11, 12, 
    #           1, 2, 3, 4, 5, 6, 
    #           1, 2, 3, 4, 5, 6)

# flowers/buds only, no 2014
pred <- c(0, 1, 2, 3, 4,
          1, 2, 3, 4, 5,
          6, 7, 8, 9, 10)

#famlist <- list(fam.bernoulli(),
             fam.truncated.poisson(truncation=0), 
             fam.normal.location(0.5052409),
             fam.normal.location(0.5465294),
             fam.normal.location(0.4890473),
             fam.normal.location(0.4404425),
             fam.normal.location(0.4589257),
             fam.normal.location(0.5751151))

    # fam <- c(1, 1, 1, 1, 1, 1, 
    #          1, 1, 1, 1, 1, 1, 
    #          2, 2, 2, 2, 2, 2, 
    #          2, 2, 2, 2, 2, 2, 
    #          3, 4, 5, 6, 7, 8)

fam <- c(1, 1, 1, 1, 1, 
         1, 1, 1, 1, 1,
         3, 3, 3, 3, 3)

sapply(fam.default(), as.character)[fam]

# something something pseudo-covariates? From slides on aster model
layer <- gsub("[0-9]", "", as.character(redata$varb))
unique(layer)

redata <- data.frame(redata, layer = layer)

with(redata, class(layer))

fit <- as.numeric(layer == "budnum.")
unique(fit)

redata <- data.frame(redata, fit=fit)
with(redata, class(fit))

##### Fit Model #####
  # remove inds with missing data (put back if I take out size variables):
  # AO2$261, SO2#321, AA2#23, AA1#86, AA1#96, AA2#197, AA2#53, AA2#131, AO2#158, AO1#74, AA1#131, AA2#114, AA2#125, AA1#283, AO2#211, AO2#229, AA2#147, AA1#173, AO1#144, AO2#170

# not converging for the model with flowers or with flowers+size...


aout1 <- aster(resp ~ varb + fit : ms, pred=pred, fam=fam, varvar=varb, idvar=id, root=root, data = redata)

summary(aout1, show.graph=TRUE,  info.tol = 1e-12)

# try to fix model convergence
info.tol <- 1e-12
infomat <- aout1$fisher
fred <- eigen(infomat, symmetric = TRUE)
sally <- fred$values < max(fred$values) * info.tol
foo <- zapsmall(fred$vectors[ , sally])

modmat <- aout1$modmat
modmat <- matrix(as.vector(modmat), ncol = dim(modmat)[3])

bar <- modmat %*% foo
