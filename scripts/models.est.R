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

#####
# Establishment
#####

# ALL GARDENS #

### glmer ###
# convergence issues for both pop/mom and pop
glmm.est.all <- glmer(germ.10 ~ ms*garden + (1|pop/mom), family=binomial(link="logit"), data=G.dat)
summary(glmm.est.all)

### glm ###
# works without random effects
m.est.all <- glm(germ.10 ~ ms*garden, family=binomial(link="logit"), data=G.dat)
summary(m.est.all)
m.est.all.emm <- emmeans(m.est.all, c("ms"), data=G.dat) #emmeans
plot(m.est.all.emm, comparisons = TRUE, horizontal= FALSE)
