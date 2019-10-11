#Local adaptation and ecological differentiation under selection, migration and drift in Arabidopsis lyrata
#Tuomas Hämälä, Tiina M. Mattila & Outi Savolainen

#Example script for total fitness estimation with aster models.

library(aster)

#The low-field data is analysed. Change to "High_phenotypes.txt" for the high-field data.
field <- read.table("Low_phenotypes.txt", header=TRUE)

#Fitness components
vars <- c("Surv_15", "Surv_16", "Surv_17", "Flow_15", "Flow_16", "Flow_17", "Fruits_15", "Fruits_16", "Fruits_17")

redata <- reshape(field, varying=list(vars), direction="long", timevar="varb", times=as.factor(vars), v.names="resp")

redata <- data.frame(redata, root=1)

#Defining the hierarchy
pred <- c(0,1,2,1,2,3,4,5,6)

foo <- c("root", vars)
pvars <- foo[pred +1]
bar <- cbind(pvars, vars)
colnames(bar) <- c("pred", "succ")
bar

#Defining the sampling distributions
fam <- c(1,1,1,1,1,1,3,3,3)

sapply(fam.default(), as.character)[fam]

Fruits <- grep("Fruits", as.character(redata$varb))
Fruits <- is.element(seq(along=redata$varb), Fruits)
redata <- data.frame(redata, Fruits=as.numeric(Fruits))

#Checking population levels
levels(redata$Pop)

#Combining populations for pairwise comparisons

PopJ1J3 = with(redata, Pop)
levels(PopJ1J3)[c(2,3)] <- "J1J3"

PopJ1T1 = with(redata, Pop)
levels(PopJ1T1)[c(2,5)] <- "J1T1"

PopJ1T3 = with(redata, Pop)
levels(PopJ1T3)[c(2,6)] <- "J1T3"

PopJ1GER = with(redata, Pop)
levels(PopJ1GER)[c(2,1)] <- "J1GER"

PopJ1NC = with(redata, Pop)
levels(PopJ1NC)[c(2,4)] <- "J1NC"

PopJ3T1 = with(redata, Pop)
levels(PopJ3T1)[c(3,5)] <- "J3T1"

PopJ3T3 = with(redata, Pop)
levels(PopJ3T3)[c(3,6)] <- "J3T3"

PopJ3GER = with(redata, Pop)
levels(PopJ3GER)[c(3,1)] <- "J3GER"

PopJ3NC = with(redata, Pop)
levels(PopJ3NC)[c(3,4)] <- "J3NC"

PopT1T3 = with(redata, Pop)
levels(PopT1T3)[c(5,6)] <- "T1T3"

PopT1GER = with(redata, Pop)
levels(PopT1GER)[c(5,1)] <- "T1GER"

PopT1NC = with(redata, Pop)
levels(PopT1NC)[c(5,4)] <- "T1NC"

PopT3GER = with(redata, Pop)
levels(PopT3GER)[c(6,1)] <- "T3GER"

PopT3NC = with(redata, Pop)
levels(PopT3NC)[c(6,4)] <- "T3NC"

PopGERNC = with(redata, Pop)
levels(PopGERNC)[c(1,4)] <- "GERNC"

#Full model
full.aster <- aster(resp ~ varb + Fruits * Pop - Pop + Block, pred, fam, varb, id, root, data=redata)

#Reduced models

J1J3.aster <- aster(resp ~ varb + Fruits * PopJ1J3 - PopJ1J3 + Block, pred, fam, varb, id, root, data=redata)

J1T1.aster <- aster(resp ~ varb + Fruits * PopJ1T1 - PopJ1T1 + Block, pred, fam, varb, id, root, data=redata)

J1T3.aster <- aster(resp ~ varb + Fruits * PopJ1T3 - PopJ1T3 + Block, pred, fam, varb, id, root, data=redata)

J1GER.aster <- aster(resp ~ varb + Fruits * PopJ1GER - PopJ1GER + Block, pred, fam, varb, id, root, data=redata)

J1NC.aster <- aster(resp ~ varb + Fruits * PopJ1NC - PopJ1NC + Block, pred, fam, varb, id, root, data=redata)

J3T1.aster <- aster(resp ~ varb + Fruits * PopJ3T1 - PopJ3T1 + Block, pred, fam, varb, id, root, data=redata)

J3T3.aster <- aster(resp ~ varb + Fruits * PopJ3T3 - PopJ3T3 + Block, pred, fam, varb, id, root, data=redata)

J3GER.aster <- aster(resp ~ varb + Fruits * PopJ3GER - PopJ3GER + Block, pred, fam, varb, id, root, data=redata)

J3NC.aster <- aster(resp ~ varb + Fruits * PopJ3NC - PopJ3NC + Block, pred, fam, varb, id, root, data=redata)

T1T3.aster <- aster(resp ~ varb + Fruits * PopT1T3 - PopT1T3 + Block, pred, fam, varb, id, root, data=redata)

T1T3.aster <- aster(resp ~ varb + Fruits * PopT1T3 - PopT1T3 + Block, pred, fam, varb, id, root, data=redata)

T1GER.aster <- aster(resp ~ varb + Fruits * PopT1GER - PopT1GER + Block, pred, fam, varb, id, root, data=redata)

T1NC.aster <- aster(resp ~ varb + Fruits * PopT1NC - PopT1NC + Block, pred, fam, varb, id, root, data=redata)

T3GER.aster <- aster(resp ~ varb + Fruits * PopT3GER - PopT3GER + Block, pred, fam, varb, id, root, data=redata)

T3NC.aster <- aster(resp ~ varb + Fruits * PopT3NC - PopT3NC + Block, pred, fam, varb, id, root, data=redata)

GERNC.aster <- aster(resp ~ varb + Fruits * PopGERNC - PopGERNC + Block, pred, fam, varb, id, root, data=redata)

#Likelihood-ratio tests

#J1 vs. J3
anova(J1J3.aster, full.aster)

#J1 vs. T1
anova(J1T1.aster, full.aster)

#J1 vs. T3
anova(J1T3.aster, full.aster)

#J1 vs. GER
anova(J1GER.aster, full.aster)

#J1 vs. NC
anova(J1NC.aster, full.aster)

#J3 vs. T1
anova(J3T1.aster, full.aster)

#J3 vs. T3
anova(J3T3.aster, full.aster)

#J3 vs. GER
anova(J3GER.aster, full.aster)

#J3 vs. NC
anova(J3NC.aster, full.aster)

#T1 vs. T3
anova(T1T3.aster, full.aster)

#T1 vs. GER
anova(T1GER.aster, full.aster)

#T1 vs. NC
anova(T1NC.aster, full.aster)

#T3 vs. GER
anova(T3GER.aster, full.aster)

#T3 vs. NC
anova(T3NC.aster, full.aster)

#GER vs. NC
anova(GERNC.aster, full.aster)

#Predicting total fitness

newdata <- data.frame(Pop = levels(field$Pop))
 
for(v in vars)
	newdata[[v]] <- 1

newdata$root <- 1
newdata$Block <- 0
 
renewdata <- reshape(newdata, varying = list(vars), direction="long", timevar="varb", times = as.factor(vars), v.names="resp")
 
Fruits <- grep("Fruits", as.character(renewdata$varb))
Fruits <- is.element(seq(along = renewdata$varb), Fruits)
renewdata <- data.frame(renewdata, Fruits = as.numeric(Fruits))
 
nind <- nrow(newdata)
nnode <- length(vars)
amat <- array(0, c(nind, nnode, nind))
for(i in 1:nind)
	amat[i, grep("Fruits", vars), i] <- 1
	
paster <- predict(full.aster, varvar=varb, idvar=id, root=root, newdata = renewdata, se.fit = TRUE, amat = amat)
 
bar <- cbind(paster$fit, paster$se.fit)
 
dimnames(bar) <- list(as.character(newdata$Pop), c("Estimate", "Std. Error"))

#Total fitness estimates and standard errors 
bar
