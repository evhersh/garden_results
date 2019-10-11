#Local adaptation and ecological differentiation under selection, migration and drift in Arabidopsis lyrata
#Tuomas Hämälä, Tiina M. Mattila & Outi Savolainen

#Example scripts for selection gradient analysis with aster models.

#For the Norwegian data sets, estimates are averaged across the three field seasons (Figure 6 in the paper).
#For the Oulu data set, each year is analysed separately with multiple-regression models (Table 4 in the paper).

library(aster)

##Low-field

low <- read.table("Low_phenotypes.txt", header=TRUE)

#Each trait is standardised to mean = 0 and sd = 1

low$Fstart_15 <- (low$Fstart_15 - mean(low$Fstart_15, na.rm=T)) / sd(low$Fstart_15, na.rm=T)

low$Fstart_16 <- (low$Fstart_16 - mean(low$Fstart_16, na.rm=T)) / sd(low$Fstart_16, na.rm=T)

low$Fstart_17 <- (low$Fstart_17 - mean(low$Fstart_17, na.rm=T)) / sd(low$Fstart_17, na.rm=T)

#After standardisation, traits are averaged across the three years
low$X <- rowMeans(cbind(low$Fstart_15, low$Fstart_16, low$Fstart_17), na.rm=T)

low <- low[!is.na(low$X),]

##Fitness components for multiple years
vars.low <- c("Flow_15", "Flow_16", "Flow_17", "Fruits_15", "Fruits_16", "Fruits_17")

redata.low <- reshape(low, varying=list(vars.low), direction="long", timevar="varb", times=as.factor(vars.low), v.names="resp")

redata.low <- data.frame(redata.low, root=1)

#Defining the hierarchy for multiple years
pred.low <- c(0,0,0,1,2,3)

#Defining sampling distributions
fam.low <- c(1,1,1,3,3,3)

Fruits <- grep("Fruits", as.character(redata.low$varb))
Fruits <- is.element(seq(along=redata.low$varb), Fruits)
redata.low <- data.frame(redata.low, Fruits=as.numeric(Fruits))

full.aster.low <- aster(resp ~ varb + Fruits * X - X + Fruits * I(X^2) - I(X^2) + Pop + Block, pred.low, fam.low, varb, id, root, data=redata.low)
full.aster.low$converged

red1.aster.low <- aster(resp ~ varb + Fruits * X - X + Pop + Block, pred.low, fam.low, varb, id, root, data=redata.low)
red1.aster.low$converged

#Testing the quadratic model
anova(red1.aster.low, full.aster.low)

red2.aster.low <- aster(resp ~ varb + Pop + Block, pred.low, fam.low, varb, id, root, data=redata.low)
red2.aster.low$converged

#Testing the main effect
anova(red2.aster.low, full.aster.low)

red3.aster.low <- aster(resp ~ varb + Fruits * X - X + Fruits * I(X^2) - I(X^2) + Block, pred.low, fam.low, varb, id, root, data=redata.low)
red3.aster.low$converged

#Testing the population effect
anova(red3.aster.low, full.aster.low)

red4.aster.low <- aster(resp ~ varb + Fruits * X - X + Fruits * I(X^2) - I(X^2) + Pop, pred.low, fam.low, varb, id, root, data=redata.low)
red4.aster.low$converged

#Testing the block effect
anova(red4.aster.low, full.aster.low)

#Point estimates for the beta and gamma terms
summary(full.aster.low, show.graph=TRUE)

#Estimating confidence intervals with parametric bootstrapping

coef.low <- full.aster.low$coefficients

theta.hat <- predict(full.aster.low, model.type = "cond", parm.type = "canon")
theta.hat <- matrix(theta.hat, nrow = nrow(full.aster.low$x), ncol = ncol(full.aster.low$x))
root <- full.aster.low$root
modmat <- full.aster.low$modmat
nboot <- 1000
boots <- matrix(NA, 2, nboot)
phenos <- c("Fruits:X", "Fruits:I(X^2)")

for(i in 1:nboot){
	foo <- raster(theta.hat, pred.low, fam.low, root)
	temp <- aster(foo, root, pred.low, fam.low, modmat, coef.low)
	boots[,i] <- coefficients(temp)[phenos]
}

ci.low <- apply(boots, 1, quantile, probs=c(0.025, 0.975), na.rm=T)

#95% CI for the beta and gamma terms
ci.low

##High-field

high <- read.table("High_phenotypes.txt", header=TRUE)

high$Fstart_15 <- (high$Fstart_15 - mean(high$Fstart_15, na.rm=T)) / sd(high$Fstart_15, na.rm=T)

high$Fstart_16 <- (high$Fstart_16 - mean(high$Fstart_16, na.rm=T)) / sd(high$Fstart_16, na.rm=T)

high$X <- rowMeans(cbind(high$Fstart_15, high$Fstart_16), na.rm=T)

high <- high[!is.na(high$X),]

vars.high <- c("Flow_15", "Flow_16", "Fruits_15", "Fruits_16")

redata.high <- reshape(high, varying=list(vars.high), direction="long", timevar="varb", times=as.factor(vars.high), v.names="resp")

redata.high <- data.frame(redata.high, root=1)

pred.high <- c(0,0,1,2)

fam.high <- c(1,1,3,3)

Fruits <- grep("Fruits", as.character(redata.high$varb))
Fruits <- is.element(seq(along=redata.high$varb), Fruits)
redata.high <- data.frame(redata.high, Fruits=as.numeric(Fruits))

full.aster.high <- aster(resp ~ varb + Fruits * X - X + Fruits * I(X^2) - I(X^2) + Pop + Block, pred.high, fam.high, varb, id, root, data=redata.high)
full.aster.high$converged

red1.aster.high <- aster(resp ~ varb + Fruits * X - X + Pop + Block, pred.high, fam.high, varb, id, root, data=redata.high)
red1.aster.high$converged

anova(red1.aster.high, full.aster.high)

red2.aster.high <- aster(resp ~ varb + Pop + Block, pred.high, fam.high, varb, id, root, data=redata.high)
red2.aster.high$converged

anova(red2.aster.high, red1.aster.high)

red3.aster.high <- aster(resp ~ varb + Fruits * X - X + Block, pred.high, fam.high, varb, id, root, data=redata.high)
red3.aster.high$converged

anova(red3.aster.high, red1.aster.high)

red4.aster.high <- aster(resp ~ varb + Fruits * X - X + Pop, pred.high, fam.high, varb, id, root, data=redata.high)
red4.aster.high$converged

anova(red4.aster.high, red1.aster.high)

summary(red1.aster.high, show.graph=TRUE)

coef.high <- red1.aster.high$coefficients

boot.model <- red1.aster.high

theta.hat <- predict(boot.model, model.type = "cond", parm.type = "canon")
theta.hat <- matrix(theta.hat, nrow = nrow(boot.model$x), ncol = ncol(boot.model$x))
root <- boot.model$root
modmat <- boot.model$modmat
nboot <- 1000
boots <- matrix(NA, 1, nboot)

for(i in 1:nboot){
	foo <- raster(theta.hat, pred.high, fam.high, root)
	temp <- aster(foo, root, pred.high, fam.high, modmat, coef.high)
	boots[,i] <- coefficients(temp)["Fruits:X"]
}

ci.high <- apply(boots, 1, quantile, probs=c(0.025, 0.975), na.rm=T)

ci.high

##Oulu-field

oulu <- read.table("Oulu_phenotypes.txt", header=TRUE)

oulu$Fstart_15 <- (oulu$Fstart_15 - mean(oulu$Fstart_15, na.rm=T)) / sd(oulu$Fstart_15, na.rm=T)

oulu$Fstart_16 <- (oulu$Fstart_16 - mean(oulu$Fstart_16, na.rm=T)) / sd(oulu$Fstart_16, na.rm=T)

oulu$Fstart_17 <- (oulu$Fstart_17 - mean(oulu$Fstart_17, na.rm=T)) / sd(oulu$Fstart_17, na.rm=T)

oulu$Shootl_15 <- (oulu$Shootl_15 - mean(oulu$Shootl_15, na.rm=T)) / sd(oulu$Shootl_15, na.rm=T)

oulu$Shootl_16 <- (oulu$Shootl_16 - mean(oulu$Shootl_16, na.rm=T)) / sd(oulu$Shootl_16, na.rm=T)

oulu$Shootl_17 <- (oulu$Shootl_17 - mean(oulu$Shootl_17, na.rm=T)) / sd(oulu$Shootl_17, na.rm=T)

oulu$Inf_15 <- (oulu$Inf_15 - mean(oulu$Inf_15, na.rm=T)) / sd(oulu$Inf_15, na.rm=T)

oulu$Inf_16 <- (oulu$Inf_16 - mean(oulu$Inf_16, na.rm=T)) / sd(oulu$Inf_16, na.rm=T)

oulu$Inf_17 <- (oulu$Inf_17 - mean(oulu$Inf_17, na.rm=T)) / sd(oulu$Inf_17, na.rm=T)

oulu$Slqd_15 <- (oulu$Slqd_15 - mean(oulu$Slqd_15, na.rm=T)) / sd(oulu$Slqd_15, na.rm=T)

oulu$Slqd_16 <- (oulu$Slqd_16 - mean(oulu$Slqd_16, na.rm=T)) / sd(oulu$Slqd_16, na.rm=T)

oulu$Slqd_17 <- (oulu$Slqd_17 - mean(oulu$Slqd_17, na.rm=T)) / sd(oulu$Slqd_17, na.rm=T)

oulu$Fstop_15 <- (oulu$Fstop_15 - mean(oulu$Fstop_15, na.rm=T)) / sd(oulu$Fstop_15, na.rm=T)

oulu$Fstop_16 <- (oulu$Fstop_16 - mean(oulu$Fstop_16, na.rm=T)) / sd(oulu$Fstop_16, na.rm=T)

oulu$Fstop_17 <- (oulu$Fstop_17 - mean(oulu$Fstop_17, na.rm=T)) / sd(oulu$Fstop_17, na.rm=T)

#For convenience, traits from a single year are copied to new variables
#The first year data is analysed. Change to _16 or _17 for the subsequent years.

oulu$Fstart <- oulu$Fstart_15

oulu$Shootl <- oulu$Shootl_15

oulu$inf <- oulu$Inf_15

oulu$Slqd <- oulu$Slqd_15

oulu$Fstop <- oulu$Fstop_15

oulu <- oulu[!is.na(oulu$Fstart),]

oulu <- oulu[!is.na(oulu$Shootl),]

oulu <- oulu[!is.na(oulu$inf),]

oulu <- oulu[!is.na(oulu$Slqd),]

oulu <- oulu[!is.na(oulu$Fstop),]

#Fitness components for single year. Change to _16 or _17 for subsequent years.
vars.oulu <- c("Flow_15", "Fruits_15")

redata.oulu <- reshape(oulu, varying=list(vars.oulu), direction="long", timevar="varb", times=as.factor(vars.oulu), v.names="reoulu")

redata.oulu <- data.frame(redata.oulu, root=1)

#Defining the hierarchy for single year
pred.oulu <- c(0,1)

#Defining the sampling distributions
fam.oulu <- c(1,3)

Fruits <- grep("Fruits", as.character(redata.oulu$varb))
Fruits <- is.element(seq(along=redata.oulu$varb), Fruits)
redata.oulu <- data.frame(redata.oulu, Fruits=as.numeric(Fruits))

#The effect of each trait is tested by removing the variable from the model. Here the effect of flowering cessation date is tested.

full.aster.oulu <- aster(reoulu ~ varb + Fruits * Fstart - Fstart + Fruits * I(Fstart^2) - I(Fstart^2) + Fruits * Shootl - Shootl + Fruits * I(Shootl^2) - I(Shootl^2) + Fruits * inf - inf + Fruits * I(inf^2) - I(inf^2) + Fruits * Slqd - Slqd + Fruits * I(Slqd^2) - I(Slqd^2) + Fruits * Fstop - Fstop + Fruits * I(Fstop^2) - I(Fstop^2) + Pop + Block, pred.oulu, fam.oulu, varb, id, root, data=redata.oulu)
full.aster.oulu$converged

red1.aster.oulu <- aster(reoulu ~ varb + Fruits * Fstart - Fstart + Fruits * I(Fstart^2) - I(Fstart^2) + Fruits * Shootl - Shootl + Fruits * I(Shootl^2) - I(Shootl^2) + Fruits * inf - inf + Fruits * I(inf^2) - I(inf^2) + Fruits * Slqd - Slqd + Fruits * I(Slqd^2) - I(Slqd^2) + Pop + Block, pred.oulu, fam.oulu, varb, id, root, data=redata.oulu)
red1.aster.oulu$converged

anova(red1.aster.oulu, full.aster.oulu)

#Point estimates for the beta and gamma terms
summary(full.aster.oulu, show.graph=TRUE)

coef.oulu <- full.aster.oulu$coefficients

boot.model <- full.aster.oulu

theta.hat <- predict(boot.model, model.type = "cond", parm.type = "canon")
theta.hat <- matrix(theta.hat, nrow = nrow(boot.model$x), ncol = ncol(boot.model$x))
root <- boot.model$root
modmat <- boot.model$modmat
nboot <- 1000
boots <- matrix(NA, 10, nboot)
phenos <- c("Fruits:Fstart", "Fruits:I(Fstart^2)","Fruits:Shootl","Fruits:I(Shootl^2)","Fruits:inf","Fruits:I(inf^2)","Fruits:Slqd","Fruits:I(Slqd^2)","Fruits:Fstop","Fruits:I(Fstop^2)")

for(i in 1:nboot){
	foo <- raster(theta.hat, pred, fam, root)
	temp <- aster(foo, root, pred, fam, modmat, coef.oulu)
	boots[,i] <- coefficients(temp)[phenos]
}

ci.oulu <- apply(boots, 1, quantile, probs=c(0.025, 0.975), na.rm=T)

#95% CI for each term
ci.oulu
