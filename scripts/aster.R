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
library(tidyr)

##### load data #####

aster.dat <<-read.csv("./data/aster_data_v3.csv", stringsAsFactors = TRUE, strip.white = TRUE, na.string = c("NA",""))

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

aster.dat$pop <- factor(aster.dat$pop, levels=c("B53", "B42", "B46", "B49", "L11", "L12", "L06", "L16", "L17", "C86", "C85", "C27"))
aster.dat$ms <- factor(aster.dat$ms, levels=c("S", "A"))
aster.dat$s.region <- factor(aster.dat$s.region, levels=c("S.s", "SO.s", "AO.s", "A.s"))
aster.dat$garden <- factor(aster.dat$garden, levels=c("SS1", "SS2", "SO1", "SO2", "AO1", "AO2", "AA1", "AA2"))
aster.dat$ms_g <- paste(aster.dat$ms, "-", aster.dat$garden)
aster.dat$ms_g <- factor(aster.dat$ms_g, levels=c("SS1 - S", "SS1 - A", "SS2 - S",  "SS2 - A", "SO1 - S", "SO1 - A", "SO2 - S", "SO2 - A", "AO1 - S", "AO1 - A", "AO2 - S", "AO2 - A", "AA1 - S", "AA1 - A", "AA2 - S", "AA2 - A"))

# aster model

##########
# Packages

library(tidyverse)
library(aster)

##### load data #####

#load("aster_example_Hersh.RData")

##### data prep #####

# replace all NA's with 0's
#aster.dat[is.na(aster.dat)] <- 0

# we see that the whole problem is with flower.2015 and budnum.2015

sum(redata$resp[redata$varb == "flower.2015"])

# so we need to get rid of these variables in the graph

vars <- c("surv.2015", "surv.2016", "surv.2017", "surv.2018", "surv.2019",
          "flower.2016", "flower.2017", "flower.2018", "flower.2019",
          "budnum.2016", "budnum.2017", "budnum.2018", "budnum.2019")

names(aster.dat)[names(aster.dat) == 'id'] <- 'rep'

redata <- reshape(aster.dat, varying=list(vars), direction="long",
                  timevar="varb", times=as.factor(vars), v.names="resp") 

redata <- data.frame(redata, root = 1)

pred <- c(0, 1, 2, 3, 4,
             2, 3, 4, 5,
             6, 7, 8, 9)

fam <- c(1, 1, 1, 1, 1, 
            1, 1, 1, 1,
            3, 3, 3, 3)

layer <- gsub("[0-9]", "", as.character(redata$varb))
redata <- data.frame(redata, layer = layer)

fit <- as.numeric(layer == "budnum.")
redata <- data.frame(redata, fit=fit)

# these work now
# aout0 <- aster(resp ~ varb + fit : (ms), pred=pred, fam=fam, varvar=varb,
#                idvar=id, root=root, data = redata)
# 
# aout1 <- aster(resp ~ varb + fit : (garden), pred=pred, fam=fam, varvar=varb,
#                idvar=id, root=root, data = redata)
# 
# aout2 <- aster(resp ~ varb + fit : (ms+garden), pred=pred, fam=fam,
#                varvar=varb, idvar=id, root=root, data = redata)
# 
# aout3 <- aster(resp ~ varb + fit : (ms*garden), pred=pred, fam=fam,
#                varvar=varb, idvar=id, root=root, data = redata)
# 
# aout4 <- aster(resp ~ varb + fit : (ms*g.region), pred=pred, fam=fam,
#                varvar=varb, idvar=id, root=root, data = redata)

aout.full <- aster(resp ~ varb + fit : (s.region*g.region), pred=pred, fam=fam,
               varvar=varb, idvar=id, root=root, data = redata)
summary(aout.full)

aout.noX <- aster(resp ~ varb + fit : (s.region+g.region), pred=pred, fam=fam,
               varvar=varb, idvar=id, root=root, data = redata)

aout.noX2 <- aster(resp ~ varb + fit : (g.region+s.region), pred=pred, fam=fam,
                  varvar=varb, idvar=id, root=root, data = redata)

aout.noS <- aster(resp ~ varb + fit : (g.region), pred=pred, fam=fam,
                  varvar=varb, idvar=id, root=root, data = redata)

aout.noG <- aster(resp ~ varb + fit : (s.region), pred=pred, fam=fam,
                  varvar=varb, idvar=id, root=root, data = redata)

anova(aout.noX, aout.full)
anova(aout.noS, aout.noX)
anova(aout.noG, aout.noX2)


# anova(aout2, aout3) # ms * garden fits better???
# 
# anova(aout0, aout2)
# 
# anova(aout1, aout3)
# 
# anova(aout1, aout2) # ms*garden fits better than ms*g.region
# 
# anova(aout4, aout5)
# 
# anova(rout1, rout2)




routX <- reaster(resp ~ varb + fit : (ms * garden), list(pop = ~ 0 + fit : pop), pred=pred, fam=fam, varvar=varb, idvar=id, root=root, data = redata)

rout.noX <- reaster(resp ~ varb + fit : (ms + garden), list(pop = ~ 0 + fit : pop), pred=pred, fam=fam, varvar=varb, idvar=id, root=root, data = redata)


rout2 <- reaster(resp ~ varb + fit : (ms * g.region), list(garden = ~ 0 + fit : garden, pop = ~ 0 + fit : pop), pred=pred, fam=fam, varvar=varb, idvar=id, root=root, data = redata)
summary(rout2)

# garden and pop have 0 estimates and NA for other values...msS:g.regionS.g has p of .075
rout3 <- reaster(resp ~ varb + fit : (ms * g.region), list(garden = ~ 0 + fit : garden, pop = ~ 0 + fit : pop, mom = ~ 0 + fit : mom), pred=pred, fam=fam, varvar=varb, idvar=id, root=root, data = redata)
summary(rout3)

rout.full <- rout4
#rout.full <- reaster(resp ~ varb + fit : (s.region * g.region), list(garden = ~ 0 + fit : garden, pop = ~ 0 + fit : pop, mom = ~ 0 + fit : mom), pred=pred, fam=fam, varvar=varb, idvar=id, root=root, data = redata)
rout.noX <- reaster(resp ~ varb + fit : (s.region + g.region), list(garden = ~ 0 + fit : garden, pop = ~ 0 + fit : pop, mom = ~ 0 + fit : mom), pred=pred, fam=fam, varvar=varb, idvar=id, root=root, data = redata)
rout.noX2 <- reaster(resp ~ varb + fit : (g.region + s.region), list(garden = ~ 0 + fit : garden, pop = ~ 0 + fit : pop, mom = ~ 0 + fit : mom), pred=pred, fam=fam, varvar=varb, idvar=id, root=root, data = redata)
rout.noS <- reaster(resp ~ varb + fit : (g.region), list(garden = ~ 0 + fit : garden, pop = ~ 0 + fit : pop, mom = ~ 0 + fit : mom), pred=pred, fam=fam, varvar=varb, idvar=id, root=root, data = redata)
rout.noG <- reaster(resp ~ varb + fit : (s.region), list(garden = ~ 0 + fit : garden, pop = ~ 0 + fit : pop, mom = ~ 0 + fit : mom), pred=pred, fam=fam, varvar=varb, idvar=id, root=root, data = redata)

anova(rout.noX, rout.full)
anova(rout.noS, rout.noX)
anova(rout.noG, rout.noX)

rout2.full <- reaster(resp ~ varb + fit : (s.region * g.region), list(garden = ~ 0 + fit : garden, pop = ~ 0 + fit : pop), pred=pred, fam=fam, varvar=varb, idvar=id, root=root, data = redata)
rout2.noX <- reaster(resp ~ varb + fit : (s.region + g.region), list(garden = ~ 0 + fit : garden, pop = ~ 0 + fit : pop), pred=pred, fam=fam, varvar=varb, idvar=id, root=root, data = redata)
rout2.noS <- reaster(resp ~ varb + fit : (g.region), list(garden = ~ 0 + fit : garden, pop = ~ 0 + fit : pop), pred=pred, fam=fam, varvar=varb, idvar=id, root=root, data = redata)
rout2.noG <- reaster(resp ~ varb + fit : (s.region), list(garden = ~ 0 + fit : garden, pop = ~ 0 + fit : pop), pred=pred, fam=fam, varvar=varb, idvar=id, root=root, data = redata)

anova(rout2.noS, rout2.full)

rout3.full <- reaster(resp ~ varb + fit : (s.region * g.region), list(garden = ~ 0 + fit : garden), pred=pred, fam=fam, varvar=varb, idvar=id, root=root, data = redata)
summary(rout3.full)
rout4.full <- reaster(resp ~ varb + fit : (s.region * g.region), list(pop = ~ 0 + fit : pop), pred=pred, fam=fam, varvar=varb, idvar=id, root=root, data = redata)
summary(rout4.full)


rout5.full <- reaster(resp ~ varb + fit : (s.region * g.region), list(mom = ~ 0 + fit : mom), pred=pred, fam=fam, varvar=varb, idvar=id, root=root, data = redata)
summary(rout5.full)
rout5.noX <- reaster(resp ~ varb + fit : (s.region + g.region), list(mom = ~ 0 + fit : mom), pred=pred, fam=fam, varvar=varb, idvar=id, root=root, data = redata)
rout5.noX2 <- reaster(resp ~ varb + fit : (g.region + s.region), list(mom = ~ 0 + fit : mom), pred=pred, fam=fam, varvar=varb, idvar=id, root=root, data = redata)
rout5.noS <- reaster(resp ~ varb + fit : (g.region), list(mom = ~ 0 + fit : mom), pred=pred, fam=fam, varvar=varb, idvar=id, root=root, data = redata)
rout5.noG <- reaster(resp ~ varb + fit : (s.region), list(mom = ~ 0 + fit : mom), pred=pred, fam=fam, varvar=varb, idvar=id, root=root, data = redata)

anova(rout5.noX,rout5.full)



rout6.full <- reaster(resp ~ varb + fit : (s.region * g.region), list(garden = ~ 0 + fit : garden, mom = ~ 0 + fit : mom), pred=pred, fam=fam, varvar=varb, idvar=id, root=root, data = redata)
summary(rout6.full)

rout5.noX <- reaster(resp ~ varb + fit : (s.region * g.region), list(mom = ~ 0 + fit : mom), pred=pred, fam=fam, varvar=varb, idvar=id, root=root, data = redata)

##### predicted values GARDEN ###### (from slides)
pout <- predict(aout3, se.fit = TRUE) #for fixed model
#pout <- predict(rout, se.fit = TRUE) #for mixed model, nm doesn't work


fred <- data.frame(ms = levels(redata$ms), garden=rep(levels(redata$garden), each=2), root = 1,
                   surv.2015=1, surv.2016=1, surv.2017=1, surv.2018=1, surv.2019=1,
                   flower.2016=1, flower.2017=1, flower.2018=1, flower.2019=1,
                   budnum.2016=1, budnum.2017=1, budnum.2018=1, budnum.2019=1)
fred

renewdata <- reshape(fred, varying = list(vars), direction = "long", timevar = "varb", times = as.factor(vars), v.names = "resp")
layer <- gsub("[0-9]", "", as.character(renewdata$varb))
renewdata <- data.frame(renewdata, layer = layer)
fit <- as.numeric(layer == "budnum.")
renewdata <- data.frame(renewdata, fit = fit)

names(renewdata)

pout <- predict(aout3, newdata = renewdata, varvar = varb, idvar = id, root = root, se.fit = TRUE)
sapply(pout, class)

sapply(pout, length)

renewdata$id

as.character(renewdata$varb)

nnode <- length(vars)
sally <- matrix(pout$fit, ncol = nnode)
dim(sally)

renewdata2 <- unite_(renewdata, "msgarden", c("ms", "garden"))

rownames(sally) <- unique(as.character(renewdata2$msgarden))
colnames(sally) <- unique(as.character(renewdata2$varb))

herman <- sally[ , grepl("budnum", colnames(sally))]
herman

rowSums(herman)

npop <- nrow(fred)
nnode <- length(vars)
amat <- array(0, c(npop, nnode, npop))
dim(amat)

foo <- grepl("budnum", vars)
for (k in 1:npop) amat[k, foo, k] <- 1

pout.amat <- predict(aout3, newdata = renewdata, varvar = varb, idvar = id, root = root, se.fit = TRUE, amat = amat)
pout.amat$fit

foo <- cbind(pout.amat$fit, pout.amat$se.fit)
rownames(foo) <- unique(as.character(renewdata2$msgarden))
colnames(foo) <- c("estimates", "std. err.")
round(foo, 3)

foo.1 <- as.data.frame(foo)
foo.1 <- setNames(cbind(rownames(foo.1), foo.1, row.names = NULL), 
                  c("ms_garden", "estimates", "std.err"))

foo.1 <- separate_(foo.1, "ms_garden", c("ms", "garden"))

foo.1$ms <- factor(foo.1$ms, levels=c("S", "A"))
foo.1$garden <- factor(foo.1$garden, levels=c("SS1", "SS2", "SO1", "SO2", "AO1", "AO2", "AA1", "AA2"))

ggplot(data=foo.1, aes(y=estimates, x=garden, fill=ms))+
  geom_point(position=position_dodge(width=0.4), pch=21, size=4)+
  geom_errorbar(aes(ymax=estimates+std.err, ymin=estimates-std.err), colour="black", width=0.2, position=position_dodge(width=0.4))+
  theme_bw()

######
##### predicted values G.REGION ###### (from slides)
######

pout <- predict(aout4, se.fit = TRUE) #for fixed model
#pout <- predict(rout, se.fit = TRUE) #for mixed model, nm doesn't work


fred <- data.frame(ms = levels(redata$ms), g.region=rep(levels(redata$g.region), each=2), root = 1,
                   surv.2015=1, surv.2016=1, surv.2017=1, surv.2018=1, surv.2019=1,
                   flower.2016=1, flower.2017=1, flower.2018=1, flower.2019=1,
                   budnum.2016=1, budnum.2017=1, budnum.2018=1, budnum.2019=1)
fred

renewdata <- reshape(fred, varying = list(vars), direction = "long", timevar = "varb", times = as.factor(vars), v.names = "resp")
layer <- gsub("[0-9]", "", as.character(renewdata$varb))
renewdata <- data.frame(renewdata, layer = layer)
fit <- as.numeric(layer == "budnum.")
renewdata <- data.frame(renewdata, fit = fit)

names(renewdata)

pout <- predict(aout4, newdata = renewdata, varvar = varb, idvar = id, root = root, se.fit = TRUE)
sapply(pout, class)

sapply(pout, length)

renewdata$id

as.character(renewdata$varb)

nnode <- length(vars)
sally <- matrix(pout$fit, ncol = nnode)
dim(sally)

renewdata2 <- unite_(renewdata, "msg.region", c("ms", "g.region"))

rownames(sally) <- unique(as.character(renewdata2$msg.region))
colnames(sally) <- unique(as.character(renewdata2$varb))

herman <- sally[ , grepl("budnum", colnames(sally))]
herman

rowSums(herman)

npop <- nrow(fred)
nnode <- length(vars)
amat <- array(0, c(npop, nnode, npop))
dim(amat)

foo <- grepl("budnum", vars)
for (k in 1:npop) amat[k, foo, k] <- 1

pout.amat <- predict(aout4, newdata = renewdata, varvar = varb, idvar = id, root = root, se.fit = TRUE, amat = amat)
pout.amat$fit

foo <- cbind(pout.amat$fit, pout.amat$se.fit)
rownames(foo) <- unique(as.character(renewdata2$msg.region))
colnames(foo) <- c("estimates", "std. err.")
round(foo, 3)

foo.1 <- as.data.frame(foo)
foo.1 <- setNames(cbind(rownames(foo.1), foo.1, row.names = NULL), 
                  c("msg.region", "estimates", "std.err"))

foo.1 <- separate_(foo.1, "msg.region", c("ms", "g.region"))

foo.1$ms <- factor(foo.1$ms, levels=c("S", "A"))
foo.1$g.region <- factor(foo.1$g.region, levels=c("S", "SO", "AO", "A"))

ggplot(data=foo.1, aes(y=estimates, x=g.region, fill=ms))+
  geom_point(position=position_dodge(width=0.4), pch=21, size=4)+
  geom_errorbar(aes(ymax=estimates+std.err, ymin=estimates-std.err), colour="black", width=0.2, position=position_dodge(width=0.4))+
  theme_bw()


######
##### predicted values G.REGION ###### (from slides)
######

pout <- predict(aout5, se.fit = TRUE) #for fixed model
#pout <- predict(rout, se.fit = TRUE) #for mixed model, nm doesn't work


fred <- data.frame(s.region = levels(redata$s.region), g.region=rep(levels(redata$g.region), each=4), root = 1,
                   surv.2015=1, surv.2016=1, surv.2017=1, surv.2018=1, surv.2019=1,
                   flower.2016=1, flower.2017=1, flower.2018=1, flower.2019=1,
                   budnum.2016=1, budnum.2017=1, budnum.2018=1, budnum.2019=1)
fred

renewdata <- reshape(fred, varying = list(vars), direction = "long", timevar = "varb", times = as.factor(vars), v.names = "resp")
layer <- gsub("[0-9]", "", as.character(renewdata$varb))
renewdata <- data.frame(renewdata, layer = layer)
fit <- as.numeric(layer == "budnum.")
renewdata <- data.frame(renewdata, fit = fit)

names(renewdata)

pout <- predict(aout5, newdata = renewdata, varvar = varb, idvar = id, root = root, se.fit = TRUE)
sapply(pout, class)

sapply(pout, length)

renewdata$id

as.character(renewdata$varb)

nnode <- length(vars)
sally <- matrix(pout$fit, ncol = nnode)
dim(sally)

renewdata2 <- unite_(renewdata, "s.region_g.region", c("s.region", "g.region"))

rownames(sally) <- unique(as.character(renewdata2$s.region_g.region))
colnames(sally) <- unique(as.character(renewdata2$varb))

herman <- sally[ , grepl("budnum", colnames(sally))]
herman

rowSums(herman)

npop <- nrow(fred)
nnode <- length(vars)
amat <- array(0, c(npop, nnode, npop))
dim(amat)

foo <- grepl("budnum", vars)
for (k in 1:npop) amat[k, foo, k] <- 1

pout.amat <- predict(aout5, newdata = renewdata, varvar = varb, idvar = id, root = root, se.fit = TRUE, amat = amat)
pout.amat$fit

foo <- cbind(pout.amat$fit, pout.amat$se.fit)
rownames(foo) <- unique(as.character(renewdata2$s.region_g.region))
colnames(foo) <- c("estimates", "std. err.")
round(foo, 3)

foo.1 <- as.data.frame(foo)
foo.1 <- setNames(cbind(rownames(foo.1), foo.1, row.names = NULL), 
                  c("s.region_g.region", "estimates", "std.err"))

foo.1 <- separate(foo.1, "s.region_g.region", sep= "_", c("s.region", "g.region"))

foo.1$s.region <- factor(foo.1$s.region, levels=c("S.s", "SO.s", "AO.s", "A.s"))
foo.1$g.region <- factor(foo.1$g.region, levels=c("S.g", "SO.g", "AO.g", "A.g"))

#full
ggplot(data=foo.1, aes(y=estimates, x=g.region, fill=s.region))+
  geom_point(position=position_dodge(width=0.4), pch=21, size=4)+
  geom_errorbar(aes(ymax=estimates+std.err, ymin=estimates-std.err), colour="black", width=0.2, position=position_dodge(width=0.4))+
  theme_bw()+
  scale_fill_manual(values=c("#F8766D", "orange", "#C77CFF", "#00BFC4"))+
  labs(x="g.region", y="lifetime flower bud production")

# S and SO only
ggplot(data=subset(foo.1, s.region == "S.s" | s.region == "SO.s"), aes(y=estimates, x=g.region, fill=s.region))+
  geom_point(position=position_dodge(width=0.4), pch=21, size=4)+
  geom_errorbar(aes(ymax=estimates+std.err, ymin=estimates-std.err), colour="black", width=0.2, position=position_dodge(width=0.4))+
  theme_bw()+
  scale_fill_manual(values=c("#F8766D", "orange"))+
  labs(x="g.region", y="lifetime flower bud production")

# AO and A only
ggplot(data=subset(foo.1, s.region == "A.s" | s.region == "AO.s"), aes(y=estimates, x=g.region, fill=s.region))+
  geom_point(position=position_dodge(width=0.4), pch=21, size=4)+
  geom_errorbar(aes(ymax=estimates+std.err, ymin=estimates-std.err), colour="black", width=0.2, position=position_dodge(width=0.4))+
  theme_bw()+
  scale_fill_manual(values=c("#C77CFF", "#00BFC4"))+
  labs(x="g.region", y="lifetime flower bud production")
