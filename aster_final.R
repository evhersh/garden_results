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


# replace all NA's with 0's
aster.dat[is.na(aster.dat)] <- 0

aster.dat$pop <- factor(aster.dat$pop, levels=c("B53", "B42", "B46", "B49", "L11", "L12", "L06", "L16", "L17", "C86", "C85", "C27"))
aster.dat$ms <- factor(aster.dat$ms, levels=c("S", "A"))
aster.dat$s.region <- factor(aster.dat$s.region, levels=c("S.s", "SO.s", "AO.s", "A.s"))
aster.dat$garden <- factor(aster.dat$garden, levels=c("SS1", "SS2", "SO1", "SO2", "AO1", "AO2", "AA1", "AA2"))

#save(aster.dat, file="aster.RData")
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


# no random effects model 
aout.full <- aster(resp ~ varb + fit : (s.region*g.region), pred=pred, fam=fam,
                   varvar=varb, idvar=id, root=root, data = redata)
summary(aout.full)

# random effects models
rout.full <- reaster(resp ~ varb + fit : (s.region * g.region), list(mom = ~ 0 + fit : mom), pred=pred, fam=fam, varvar=varb, idvar=id, root=root, data = redata)
summary(rout.full)
rout.noX <- reaster(resp ~ varb + fit : (s.region + g.region), list(mom = ~ 0 + fit : mom), pred=pred, fam=fam, varvar=varb, idvar=id, root=root, data = redata)
rout.noX2 <- reaster(resp ~ varb + fit : (g.region + s.region), list(mom = ~ 0 + fit : mom), pred=pred, fam=fam, varvar=varb, idvar=id, root=root, data = redata)
rout.noS <- reaster(resp ~ varb + fit : (g.region), list(mom = ~ 0 + fit : mom), pred=pred, fam=fam, varvar=varb, idvar=id, root=root, data = redata)
rout.noG <- reaster(resp ~ varb + fit : (s.region), list(mom = ~ 0 + fit : mom), pred=pred, fam=fam, varvar=varb, idvar=id, root=root, data = redata)

anova(rout.noX, rout.full)
anova(rout.noG, rout.noX)
anova(rout.noS, rout.noX2)


load("aster_cluster.RData")



######
##### predicted values s.region x g.region (no random effects) ###### (from slides)
######

pout <- predict(aout.full, se.fit = TRUE) #for fixed model
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

pout <- predict(aout.full, newdata = renewdata, varvar = varb, idvar = id, root = root, se.fit = TRUE)
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

pout.amat <- predict(aout.full, newdata = renewdata, varvar = varb, idvar = id, root = root, se.fit = TRUE, amat = amat)
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
