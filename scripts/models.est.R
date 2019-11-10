##########
# Packages

library(MASS)
library(gdata)
library(plotrix)
library(aster)
library(lattice)
library(dplyr)
library(ggplot2)
library(lme4)
library(emmeans)
library(AER)
library(lmtest)
library(ggeffects)

#####
# Establishment
#####

# ALL GARDENS #

### glmer ###
# convergence issues
glmm.est.0 <- glmer(germ.10 ~ ms*g.region + (1|garden/pop/mom), family=binomial(link="logit"), data=G.dat, control=glmerControl(optimizer="nloptwrap", optCtrl=list(maxfun=2e5)))
summary(glmm.est.0)

# works without mom
glmm.est.1 <- glmer(germ.10 ~ ms*g.region + (1|garden/pop), family=binomial(link="logit"), data=G.dat)
summary(glmm.est.1)

glmm.est.2 <- glmer(germ.10 ~ ms*garden + (1|pop), family=binomial(link="logit"), data=G.dat)
summary(glmm.est.2)

lrtest(glmm.est.1, glmm.est.2) # likes the fit of garden as main effect better


glmm.est.5 <- glmer(germ.10 ~ s.region*g.region + (1|garden), family=binomial(link="logit"), data=G.dat, control=glmerControl(optimizer="nloptwrap", optCtrl=list(maxfun=2e5)))


#ggeffects

glmm.est.1.ggfx <- ggpredict(glmm.est.1, terms=c("g.region", "ms"))
plot(glmm.est.1.ggfx)+
  labs(y="predicted establishment success", x="garden region", title="")

glmm.est.2.ggfx <- ggpredict(glmm.est.2, terms=c("garden", "ms"))
plot(glmm.est.2.ggfx)+
  labs(y="predicted establishment success", title="")



