# aster cluster

library(tidyverse)
library(aster)

load("aster.RData")

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

### models ###

rout.full <- reaster(resp ~ varb + fit : (s.region * g.region), list(mom = ~ 0 + fit : mom), pred=pred, fam=fam, varvar=varb, idvar=id, root=root, data = redata)
summary(rout.full)
rout.noX <- reaster(resp ~ varb + fit : (s.region + g.region), list(mom = ~ 0 + fit : mom), pred=pred, fam=fam, varvar=varb, idvar=id, root=root, data = redata)
rout.noX2 <- reaster(resp ~ varb + fit : (g.region + s.region), list(mom = ~ 0 + fit : mom), pred=pred, fam=fam, varvar=varb, idvar=id, root=root, data = redata)
rout.noS <- reaster(resp ~ varb + fit : (g.region), list(mom = ~ 0 + fit : mom), pred=pred, fam=fam, varvar=varb, idvar=id, root=root, data = redata)
rout.noG <- reaster(resp ~ varb + fit : (s.region), list(mom = ~ 0 + fit : mom), pred=pred, fam=fam, varvar=varb, idvar=id, root=root, data = redata)

save.image(file="aster_cluster.RData")

q()