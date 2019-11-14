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


glmm.est.1.1 <- glmer(germ.10 ~ ms+g.region + (1|garden/pop), family=binomial(link="logit"), data=G.dat)

glmm.est.1.2 <- glmer(germ.10 ~ ms + (1|pop), family=binomial(link="logit"), data=G.dat)

glmm.est.1.3 <- glmer(germ.10 ~ g.region + (1|garden), family=binomial(link="logit"), data=G.dat)

lrtest(glmm.est.1.1, glmm.est.1) # no signifcant interaction?
lrtest(glmm.est.1.2, glmm.est.1.1) # g.region is significant
lrtest(glmm.est.1.3, glmm.est.1.1) # ms is significant


glmm.est.2 <- glmer(germ.10 ~ ms*garden + (1|pop), family=binomial(link="logit"), data=G.dat)
summary(glmm.est.2)

lrtest(glmm.est.1, glmm.est.2) # likes the fit of garden as main effect better

glmm.est.2.1 <- glmer(germ.10 ~ ms+garden + (1|pop), family=binomial(link="logit"), data=G.dat)

lrtest(glmm.est.2)





glmm.est.5 <- glmer(germ.10 ~ s.region*g.region + (1|garden), family=binomial(link="logit"), data=G.dat, control=glmerControl(optimizer="nloptwrap", optCtrl=list(maxfun=2e5)))


#ggeffects

glmm.est.1.ggfx <- ggpredict(glmm.est.1, terms=c("g.region", "ms"))
plot(glmm.est.1.ggfx)+
  labs(y="predicted establishment success", x="garden region", title="")

glmm.est.2.ggfx <- ggpredict(glmm.est.2, terms=c("garden", "ms"))
plot(glmm.est.2.ggfx)+
  labs(y="predicted establishment success", title="")

glmm.est.3.ggfx <- ggpredict(glmm.est.1.1, terms=c("ms"))
plot(glmm.est.3.ggfx)+
  labs(y="predicted establishment success", x="Mating System", title="")

glmm.est.5.ggfx <- ggpredict(glmm.est.5, terms=c("g.region", "s.region"))
plot(glmm.est.5.ggfx)+
  labs(y="predicted establishment success", x="Mating System", title="")+
  scale_colour_manual(values=c("#F8766D", "orange", "#C77CFF", "#00BFC4"))
