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
### LEAF NUMBER
#####
# (Should check for overdispersion in count data???)

####
#### YEAR 1
####

### glmer y1###
# doesn't converge with mom in model
glmm.num.y1.0 <- glmer(leaf.num~ms*g.region+(1|garden/pop/mom), family=poisson(link="log"), data=H.dat.y1)
# converges without mom
glmm.num.y1.1 <- glmer(leaf.num~ms*g.region+(1|garden/pop), family=poisson(link="log"), data=H.dat.y1)

# doesn't converge with mom
glmm.num.y1.2 <- glmer(leaf.num~ms*garden+(1|pop/mom), family=poisson(link="log"), data=H.dat.y1)
# converges with mom
glmm.num.y1.3 <- glmer(leaf.num~ms*garden+(1|pop), family=poisson(link="log"), data=H.dat.y1)

# g.region * ms
glmm.num.y1.1.ggfx <- ggpredict(glmm.num.y1.1, terms=c("g.region", "ms"))
plot(glmm.num.y1.1.ggfx) # no diffs
# garden * ms
glmm.num.y1.3.ggfx <- ggpredict(glmm.num.y1.3, terms=c("garden", "ms"))
plot(glmm.num.y1.3.ggfx) # apos have more leaves in SO2 and AA2


### glmer y2###
# doesn't converge with mom in model
glmm.num.y2.0 <- glmer(leaf.num~ms*g.region+(1|garden/pop/mom), family=poisson(link="log"), data=H.dat.y2)
# converges without mom
glmm.num.y2.1 <- glmer(leaf.num~ms*g.region+(1|garden/pop), family=poisson(link="log"), data=H.dat.y2)

# doesn't converge with mom
glmm.num.y2.2 <- glmer(leaf.num~ms*garden+(1|pop/mom), family=poisson(link="log"), data=H.dat.y2)
# converges with mom
glmm.num.y2.3 <- glmer(leaf.num~ms*garden+(1|pop), family=poisson(link="log"), data=H.dat.y2)

lrtest(glmm.num.y2.1, glmm.num.y2.3)

# g.region * ms
glmm.num.y2.1.ggfx <- ggpredict(glmm.num.y2.1, terms=c("g.region", "ms"))
plot(glmm.num.y2.1.ggfx)+
  labs(y="predicted leaf number", x="source region", title="predicted leaf number in year 2")# apos have more leaves in A.g
# garden * ms
glmm.num.y2.3.ggfx <- ggpredict(glmm.num.y2.3, terms=c("garden", "ms"))
plot(glmm.num.y2.3.ggfx) # apos have more leaves in SS2, SO2, AA1, and AA2


### glmer y3###
glmm.num.y3.0 <- glmer(leaf.num~ms*g.region+(1|garden/pop/mom), family=poisson(link="log"), data=H.dat.y3)

# doesn't converge with mom
glmm.num.y3.2 <- glmer(leaf.num~ms*garden+(1|pop/mom), family=poisson(link="log"), data=H.dat.y3)
# converges with mom
glmm.num.y3.3 <- glmer(leaf.num~ms*garden+(1|pop), family=poisson(link="log"), data=H.dat.y3)

# g.region * ms
glmm.num.y3.0.ggfx <- ggpredict(glmm.num.y3.0, terms=c("g.region", "ms"))
plot(glmm.num.y3.0.ggfx) # apos have more leaves in A.g
# garden * ms
glmm.num.y3.3.ggfx <- ggpredict(glmm.num.y3.3, terms=c("garden", "ms"))
plot(glmm.num.y3.3.ggfx) # apos have more leaves in SS2, AO2 and AA2


### glmer y4###
glmm.num.y4.0 <- glmer(leaf.num~ms*g.region+(1|garden/pop/mom), family=poisson(link="log"), data=H.dat.y4)
summary(glmm.num.y4.0)
# doesn't converge with mom
glmm.num.y4.2 <- glmer(leaf.num~ms*garden+(1|pop/mom), family=poisson(link="log"), data=H.dat.y4)
# converges with mom
glmm.num.y4.3 <- glmer(leaf.num~ms*garden+(1|pop), family=poisson(link="log"), data=H.dat.y4)

# g.region * ms
glmm.num.y4.0.ggfx <- ggpredict(glmm.num.y4.0, terms=c("g.region", "ms"))
plot(glmm.num.y4.0.ggfx) # no diffs
# garden * ms
glmm.num.y4.3.ggfx <- ggpredict(glmm.num.y4.3, terms=c("garden", "ms"))
plot(glmm.num.y4.3.ggfx) # apos have more leaves in SO1, and WAY more AA2. Sexuals have marginally more leaves in SO2


### glmer y5###
# doesnt converge with mom
glmm.num.y5.0 <- glmer(leaf.num~ms*g.region+(1|garden/pop/mom), family=poisson(link="log"), data=H.dat.y5)

# doesnt converge with pop
glmm.num.y5.1 <- glmer(leaf.num~ms*g.region+(1|garden/pop), family=poisson(link="log"), data=H.dat.y5)
summary(glmm.num.y5.0)

# doesn't converge with mom
glmm.num.y5.2 <- glmer(leaf.num~ms*garden+(1|pop/mom), family=poisson(link="log"), data=H.dat.y5)
# converges with mom, rank deficient because no apo survivors in SO1
glmm.num.y5.3 <- glmer(leaf.num~ms*garden+(1|pop), family=poisson(link="log"), data=H.dat.y5)
summary(glmm.num.y5.3)
# g.region * ms
# glmm.num.y5.0.ggfx <- ggpredict(glmm.num.y5.0, terms=c("g.region", "ms"))
# plot(glmm.num.y5.0.ggfx) # no diffs
# garden * ms
glmm.num.y5.3.ggfx <- ggpredict(glmm.num.y5.3, terms=c("garden", "ms"))
plot(glmm.num.y5.3.ggfx) 






# Model singularity?
glmm2.y1.AA1.num.MS <- glmer(leaf.num~ms+(1|pop/mom), family=poisson(link="log"), data=H.dat.y1.AA1)

# works!
glmm2.y1.AA1.num.MS <- glmer(leaf.num~ms+(1|pop), family=poisson(link="log"), data=H.dat.y1.AA1)
plot(glmm2.y1.AA1.num.MS)
summary(glmm2.y1.AA1.num.MS) # no differences

### glm ###

# AA1 - MS
m.y1.AA1.num.MS <- glm(leaf.num~ms, family=poisson(link="log"), data=H.dat.y1.AA1)
summary(m.y1.AA1.num.MS) # Apos have more leaves!
m.y1.AA1.num.MS.emm <- emmeans(m.y1.AA1.num.MS, c("ms"), data=H.dat.y1.AA1) #emmeans
plot(m.y1.AA1.num.MS.emm, comparisons = TRUE, horizontal= FALSE)

# AA2 - MS
m.y1.AA2.num.MS <- glm(leaf.num~ms, family=poisson(link="log"), data=H.dat.y1.AA2)
summary(m.y1.AA2.num.MS) # apos have more leaves!
m.y1.AA2.num.MS.emm <- emmeans(m.y1.AA2.num.MS, c("ms"), data=H.dat.y1.AA2) #emmeans
plot(m.y1.AA2.num.MS.emm, comparisons = TRUE, horizontal= FALSE)

# AO1 - MS
m.y1.AO1.num.MS <- glm(leaf.num~ms, family=poisson(link="log"), data=H.dat.y1.AO1)
summary(m.y1.AO1.num.MS) # sex have more leaves!
m.y1.AO1.num.MS.emm <- emmeans(m.y1.AO1.num.MS, c("ms"), data=H.dat.y1.AO1) #emmeans
plot(m.y1.AO1.num.MS.emm, comparisons = TRUE, horizontal= FALSE)

# AO2 - MS
m.y1.AO2.num.MS <- glm(leaf.num~ms, family=poisson(link="log"), data=H.dat.y1.AO2)
summary(m.y1.AO2.num.MS) # apos have more leaves!
m.y1.AO2.num.MS.emm <- emmeans(m.y1.AO2.num.MS, c("ms"), data=H.dat.y1.AO2) #emmeans
plot(m.y1.AO2.num.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SO1 - MS
m.y1.SO1.num.MS <- glm(leaf.num~ms, family=poisson(link="log"), data=H.dat.y1.SO1)
summary(m.y1.SO1.num.MS) # apos have more leaves!
m.y1.SO1.num.MS.emm <- emmeans(m.y1.SO1.num.MS, c("ms"), data=H.dat.y1.SO1) #emmeans
plot(m.y1.SO1.num.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SO2 - MS
m.y1.SO2.num.MS <- glm(leaf.num~ms, family=poisson(link="log"), data=H.dat.y1.SO2)
summary(m.y1.SO2.num.MS) # apos have more leaves!
m.y1.SO2.num.MS.emm <- emmeans(m.y1.SO2.num.MS, c("ms"), data=H.dat.y1.SO2) #emmeans
plot(m.y1.SO2.num.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SS1 - MS
m.y1.SS1.num.MS <- glm(leaf.num~ms, family=poisson(link="log"), data=H.dat.y1.SS1)
summary(m.y1.SS1.num.MS) # apos have more leaves!
m.y1.SS1.num.MS.emm <- emmeans(m.y1.SS1.num.MS, c("ms"), data=H.dat.y1.SS1) #emmeans
plot(m.y1.SS1.num.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SS2 - MS
m.y1.SS2.num.MS <- glm(leaf.num~ms, family=poisson(link="log"), data=H.dat.y1.SS2)
summary(m.y1.SS2.num.MS) # apos have more leaves!
m.y1.SS2.num.MS.emm <- emmeans(m.y1.SS2.num.MS, c("ms"), data=H.dat.y1.SS2) #emmeans
plot(m.y1.SS2.num.MS.emm, comparisons = TRUE, horizontal= FALSE)



####
#### YEAR 2
####



### glmer ###
# all gardens - failed to converge
glmm.y2.all.num.MS <- glmer(leaf.num~ms*garden+(1|pop/mom), family=poisson(link="log"), data=H.dat.y2)
summary(glmm.y2.all.num.MS)
anova(glmm.y2.all.num.MS)

# Model singularity?
glmm2.y2.AA1.num.MS <- glmer(leaf.num~ms+(1|pop/mom), family=poisson(link="log"), data=H.dat.y2.AA1)

# works!
glmm2.y2.AA1.num.MS <- glmer(leaf.num~ms+(1|pop), family=poisson(link="log"), data=H.dat.y2.AA1)
plot(glmm2.y2.AA1.num.MS)
summary(glmm2.y2.AA1.num.MS) # differences! apos have more
glmm2.y2.AA1.num.MS.emm <- emmeans(glmm2.y2.AA1.num.MS, c("ms"), data=H.dat.y2.AA1) #emmeans
plot(glmm2.y2.AA1.num.MS.emm, comparisons = TRUE, horizontal= FALSE)

### glm ###

# AA1 - MS
m.y2.AA1.num.MS <- glm(leaf.num~ms, family=quasipoisson(link="log"), data=H.dat.y2.AA1)
summary(m.y2.AA1.num.MS) # Apos have more leaves!
m.y2.AA1.num.MS.emm <- emmeans(m.y2.AA1.num.MS, c("ms"), data=H.dat.y2.AA1) #emmeans
plot(m.y2.AA1.num.MS.emm, comparisons = TRUE, horizontal= FALSE)

# check for dispersion
dispersiontest(m.y2.AA1.num.MS, trafo=1)

# AA2 - MS
m.y2.AA2.num.MS <- glm(leaf.num~ms, family=poisson(link="log"), data=H.dat.y2.AA2)
summary(m.y2.AA2.num.MS) # apos have more leaves!
m.y2.AA2.num.MS.emm <- emmeans(m.y2.AA2.num.MS, c("ms"), data=H.dat.y2.AA2) #emmeans
plot(m.y2.AA2.num.MS.emm, comparisons = TRUE, horizontal= FALSE)

# AO1 - MS
m.y2.AO1.num.MS <- glm(leaf.num~ms, family=poisson(link="log"), data=H.dat.y2.AO1)
summary(m.y2.AO1.num.MS) # sex have more leaves!
m.y2.AO1.num.MS.emm <- emmeans(m.y2.AO1.num.MS, c("ms"), data=H.dat.y2.AO1) #emmeans
plot(m.y2.AO1.num.MS.emm, comparisons = TRUE, horizontal= FALSE)

# AO2 - MS
m.y2.AO2.num.MS <- glm(leaf.num~ms, family=poisson(link="log"), data=H.dat.y2.AO2)
summary(m.y2.AO2.num.MS) # apos have more leaves!
m.y2.AO2.num.MS.emm <- emmeans(m.y2.AO2.num.MS, c("ms"), data=H.dat.y2.AO2) #emmeans
plot(m.y2.AO2.num.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SO1 - MS
m.y2.SO1.num.MS <- glm(leaf.num~ms, family=poisson(link="log"), data=H.dat.y2.SO1)
summary(m.y2.SO1.num.MS) # apos have more leaves!
m.y2.SO1.num.MS.emm <- emmeans(m.y2.SO1.num.MS, c("ms"), data=H.dat.y2.SO1) #emmeans
plot(m.y2.SO1.num.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SO2 - MS
m.y2.SO2.num.MS <- glm(leaf.num~ms, family=poisson(link="log"), data=H.dat.y2.SO2)
summary(m.y2.SO2.num.MS) # apos have more leaves!
m.y2.SO2.num.MS.emm <- emmeans(m.y2.SO2.num.MS, c("ms"), data=H.dat.y2.SO2) #emmeans
plot(m.y2.SO2.num.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SS1 - MS
m.y2.SS1.num.MS <- glm(leaf.num~ms, family=poisson(link="log"), data=H.dat.y2.SS1)
summary(m.y2.SS1.num.MS) # apos have more leaves!
m.y2.SS1.num.MS.emm <- emmeans(m.y2.SS1.num.MS, c("ms"), data=H.dat.y2.SS1) #emmeans
plot(m.y2.SS1.num.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SS2 - MS
m.y2.SS2.num.MS <- glm(leaf.num~ms, family=poisson(link="log"), data=H.dat.y2.SS2)
summary(m.y2.SS2.num.MS) # apos have more leaves!
m.y2.SS2.num.MS.emm <- emmeans(m.y2.SS2.num.MS, c("ms"), data=H.dat.y2.SS2) #emmeans
plot(m.y2.SS2.num.MS.emm, comparisons = TRUE, horizontal= FALSE)



####
#### YEAR 3
####



### glmer ###
# all gardens - failed to converge
glmm.y3.all.num.MS <- glmer(leaf.num~ms*garden+(1|pop/mom), family=poisson(link="log"), data=H.dat.y3)
summary(glmm.y3.all.num.MS)
anova(glmm.y3.all.num.MS)

# Model convergence fails with mom in the model, works with just pop
glmm2.y3.AA1.num.MS <- glmer(leaf.num~ms+(1|pop), family=poisson(link="log"), data=H.dat.y3.AA1)
summary(glmm2.y3.AA1.num.MS)

# works!
glmm2.y3.AA1.num.MS <- glmer(leaf.num~ms+(1|pop), family=poisson(link="log"), data=H.dat.y3.AA1)
plot(glmm2.y3.AA1.num.MS)
summary(glmm2.y3.AA1.num.MS)
glmm2.y3.AA1.num.MS.emm <- emmeans(glmm2.y3.AA1.num.MS, c("ms"), data=H.dat.y3.AA1) #emmeans
plot(glmm2.y3.AA1.num.MS.emm, comparisons = TRUE, horizontal= FALSE)

### glm ###

# AA1 - MS
m.y3.AA1.num.MS <- glm(leaf.num~ms, family=poisson(link="log"), data=H.dat.y3.AA1)
summary(m.y3.AA1.num.MS) # Apos have more leaves!
m.y3.AA1.num.MS.emm <- emmeans(m.y3.AA1.num.MS, c("ms"), data=H.dat.y3.AA1) #emmeans
plot(m.y3.AA1.num.MS.emm, comparisons = TRUE, horizontal= FALSE)

# AA2 - MS
m.y3.AA2.num.MS <- glm(leaf.num~ms, family=poisson(link="log"), data=H.dat.y3.AA2)
summary(m.y3.AA2.num.MS) # apos have more leaves! (but not significant in mixed model)
m.y3.AA2.num.MS.emm <- emmeans(m.y3.AA2.num.MS, c("ms"), data=H.dat.y3.AA2) #emmeans
plot(m.y3.AA2.num.MS.emm, comparisons = TRUE, horizontal= FALSE)

# AO1 - MS
m.y3.AO1.num.MS <- glm(leaf.num~ms, family=poisson(link="log"), data=H.dat.y3.AO1)
summary(m.y3.AO1.num.MS)
glmm.y3.AO1.num.MS <- glmer(leaf.num~ms + (1|pop/mom), family=poisson(link="log"), data=H.dat.y3.AO1)
summary(glmm.y3.AO1.num.MS)
m.y3.AO1.num.MS.emm <- emmeans(m.y3.AO1.num.MS, c("ms"), data=H.dat.y3.AO1) #emmeans
plot(m.y3.AO1.num.MS.emm, comparisons = TRUE, horizontal= FALSE)

# AO2 - MS
m.y3.AO2.num.MS <- glm(leaf.num~ms, family=poisson(link="log"), data=H.dat.y3.AO2)
summary(m.y3.AO2.num.MS) # apos have more leaves!
glmm.y3.AO2.num.MS <- glmer(leaf.num~ms+(1|mom/pop), family=poisson(link="log"), data=H.dat.y3.AO2)
summary(glmm.y3.AO2.num.MS) # still significant with mixed model
m.y3.AO2.num.MS.emm <- emmeans(m.y3.AO2.num.MS, c("ms"), data=H.dat.y3.AO2) #emmeans
plot(m.y3.AO2.num.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SO1 - MS
m.y3.SO1.num.MS <- glm(leaf.num~ms, family=poisson(link="log"), data=H.dat.y3.SO1)
summary(m.y3.SO1.num.MS) # apos have more leaves!
glmm.y3.SO1.num.MS <- glmer(leaf.num~ms+(1|mom:pop), family=poisson(link="log"), data=H.dat.y3.SO1)
summary(glmm.y3.SO1.num.MS) # not even close in mixed model
m.y3.SO1.num.MS.emm <- emmeans(m.y3.SO1.num.MS, c("ms"), data=H.dat.y3.SO1) #emmeans
plot(m.y3.SO1.num.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SO2 - MS
m.y3.SO2.num.MS <- glm(leaf.num~ms, family=poisson(link="log"), data=H.dat.y3.SO2)
summary(m.y3.SO2.num.MS)
m.y3.SO2.num.MS.emm <- emmeans(m.y3.SO2.num.MS, c("ms"), data=H.dat.y3.SO2) #emmeans
plot(m.y3.SO2.num.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SS1 - MS
m.y3.SS1.num.MS <- glm(leaf.num~ms, family=poisson(link="log"), data=H.dat.y3.SS1)
summary(m.y3.SS1.num.MS) 
m.y3.SS1.num.MS.emm <- emmeans(m.y3.SS1.num.MS, c("ms"), data=H.dat.y3.SS1) #emmeans
plot(m.y3.SS1.num.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SS2 - MS
m.y3.SS2.num.MS <- glm(leaf.num~ms, family=poisson(link="log"), data=H.dat.y3.SS2)
summary(m.y3.SS2.num.MS) # apos have more leaves!
glmm.y3.SS2.num.MS <- glmer(leaf.num~ms+(1|pop/mom), family=poisson(link="log"), data=H.dat.y3.SS2) # doesn't converge with mom in the model
summary(glmm.y3.SS2.num.MS)
m.y3.SS2.num.MS.emm <- emmeans(m.y3.SS2.num.MS, c("ms"), data=H.dat.y3.SS2) #emmeans
plot(m.y3.SS2.num.MS.emm, comparisons = TRUE, horizontal= FALSE)



####
#### YEAR 4
####



### glmer ###
# all gardens - failed to converge
glmm.y4.all.num.MS <- glmer(leaf.num~ms*garden+(1|pop/mom), family=poisson(link="log"), data=H.dat.y4)
summary(glmm.y4.all.num.MS)
anova(glmm.y4.all.num.MS)

# Model singularity?
glmm2.y4.AA1.num.MS <- glmer(leaf.num~ms+(1|pop/mom), family=poisson(link="log"), data=H.dat.y4.AA1)

# works!
glmm2.y4.AA1.num.MS <- glmer(leaf.num~ms+(1|pop), family=poisson(link="log"), data=H.dat.y4.AA1)
plot(glmm2.y4.AA1.num.MS)
summary(glmm2.y4.AA1.num.MS) # differences! apos have more
glmm2.y4.AA1.num.MS.emm <- emmeans(glmm2.y4.AA1.num.MS, c("ms"), data=H.dat.y4.AA1) #emmeans
plot(glmm2.y4.AA1.num.MS.emm, comparisons = TRUE, horizontal= FALSE)

### glm ###

# AA1 - MS
m.y4.AA1.num.MS <- glm(leaf.num~ms, family=poisson(link="log"), data=H.dat.y4.AA1)
summary(m.y4.AA1.num.MS) # Apos have more leaves!
m.y4.AA1.num.MS.emm <- emmeans(m.y4.AA1.num.MS, c("ms"), data=H.dat.y4.AA1) #emmeans
plot(m.y4.AA1.num.MS.emm, comparisons = TRUE, horizontal= FALSE)

# AA2 - MS
m.y4.AA2.num.MS <- glm(leaf.num~ms, family=poisson(link="log"), data=H.dat.y4.AA2)
summary(m.y4.AA2.num.MS) # apos have more leaves!
m.y4.AA2.num.MS.emm <- emmeans(m.y4.AA2.num.MS, c("ms"), data=H.dat.y4.AA2) #emmeans
plot(m.y4.AA2.num.MS.emm, comparisons = TRUE, horizontal= FALSE)

# AO1 - MS
m.y4.AO1.num.MS <- glm(leaf.num~ms, family=poisson(link="log"), data=H.dat.y4.AO1)
summary(m.y4.AO1.num.MS) # sex have more leaves!
m.y4.AO1.num.MS.emm <- emmeans(m.y4.AO1.num.MS, c("ms"), data=H.dat.y4.AO1) #emmeans
plot(m.y4.AO1.num.MS.emm, comparisons = TRUE, horizontal= FALSE)

# AO2 - MS
m.y4.AO2.num.MS <- glm(leaf.num~ms, family=poisson(link="log"), data=H.dat.y4.AO2)
summary(m.y4.AO2.num.MS) # apos have more leaves!
m.y4.AO2.num.MS.emm <- emmeans(m.y4.AO2.num.MS, c("ms"), data=H.dat.y4.AO2) #emmeans
plot(m.y4.AO2.num.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SO1 - MS
m.y4.SO1.num.MS <- glm(leaf.num~ms, family=poisson(link="log"), data=H.dat.y4.SO1)
summary(m.y4.SO1.num.MS) # apos have more leaves!
m.y4.SO1.num.MS.emm <- emmeans(m.y4.SO1.num.MS, c("ms"), data=H.dat.y4.SO1) #emmeans
plot(m.y4.SO1.num.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SO2 - MS
m.y4.SO2.num.MS <- glm(leaf.num~ms, family=poisson(link="log"), data=H.dat.y4.SO2)
summary(m.y4.SO2.num.MS) # apos have more leaves!
m.y4.SO2.num.MS.emm <- emmeans(m.y4.SO2.num.MS, c("ms"), data=H.dat.y4.SO2) #emmeans
plot(m.y4.SO2.num.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SS1 - MS
m.y4.SS1.num.MS <- glm(leaf.num~ms, family=poisson(link="log"), data=H.dat.y4.SS1)
summary(m.y4.SS1.num.MS) # apos have more leaves!
m.y4.SS1.num.MS.emm <- emmeans(m.y4.SS1.num.MS, c("ms"), data=H.dat.y4.SS1) #emmeans
plot(m.y4.SS1.num.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SS2 - MS
m.y4.SS2.num.MS <- glm(leaf.num~ms, family=poisson(link="log"), data=H.dat.y4.SS2)
summary(m.y4.SS2.num.MS) # apos have more leaves!
m.y4.SS2.num.MS.emm <- emmeans(m.y4.SS2.num.MS, c("ms"), data=H.dat.y4.SS2) #emmeans
plot(m.y4.SS2.num.MS.emm, comparisons = TRUE, horizontal= FALSE)



####
#### YEAR 5
####



### glmer ###
# all gardens - failed to converge
glmm.y5.all.num.MS <- glmer(leaf.num~ms*garden+(1|pop/mom), family=poisson(link="log"), data=H.dat.y5)
summary(glmm.y5.all.num.MS)
anova(glmm.y5.all.num.MS)

# Model singularity?
glmm2.y5.AA1.num.MS <- glmer(leaf.num~ms+(1|pop/mom), family=poisson(link="log"), data=H.dat.y5.AA1)

# works!
glmm2.y5.AA1.num.MS <- glmer(leaf.num~ms+(1|pop), family=poisson(link="log"), data=H.dat.y5.AA1)
plot(glmm2.y5.AA1.num.MS)
summary(glmm2.y5.AA1.num.MS) # differences! apos have more
glmm2.y5.AA1.num.MS.emm <- emmeans(glmm2.y5.AA1.num.MS, c("ms"), data=H.dat.y5.AA1) #emmeans
plot(glmm2.y5.AA1.num.MS.emm, comparisons = TRUE, horizontal= FALSE)

### glm ###

# AA1 - MS
m.y5.AA1.num.MS <- glm(leaf.num~ms, family=poisson(link="log"), data=H.dat.y5.AA1)
summary(m.y5.AA1.num.MS) # Apos have more leaves!
m.y5.AA1.num.MS.emm <- emmeans(m.y5.AA1.num.MS, c("ms"), data=H.dat.y5.AA1) #emmeans
plot(m.y5.AA1.num.MS.emm, comparisons = TRUE, horizontal= FALSE)

# AA2 - MS
m.y5.AA2.num.MS <- glm(leaf.num~ms, family=poisson(link="log"), data=H.dat.y5.AA2)
summary(m.y5.AA2.num.MS) # apos have more leaves!
m.y5.AA2.num.MS.emm <- emmeans(m.y5.AA2.num.MS, c("ms"), data=H.dat.y5.AA2) #emmeans
plot(m.y5.AA2.num.MS.emm, comparisons = TRUE, horizontal= FALSE)

# AO1 - MS
m.y5.AO1.num.MS <- glm(leaf.num~ms, family=poisson(link="log"), data=H.dat.y5.AO1)
summary(m.y5.AO1.num.MS) # sex have more leaves!
m.y5.AO1.num.MS.emm <- emmeans(m.y5.AO1.num.MS, c("ms"), data=H.dat.y5.AO1) #emmeans
plot(m.y5.AO1.num.MS.emm, comparisons = TRUE, horizontal= FALSE)

# AO2 - MS
m.y5.AO2.num.MS <- glm(leaf.num~ms, family=poisson(link="log"), data=H.dat.y5.AO2)
summary(m.y5.AO2.num.MS) # apos have more leaves!
m.y5.AO2.num.MS.emm <- emmeans(m.y5.AO2.num.MS, c("ms"), data=H.dat.y5.AO2) #emmeans
plot(m.y5.AO2.num.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SO1 - MS
m.y5.SO1.num.MS <- glm(leaf.num~ms, family=poisson(link="log"), data=H.dat.y5.SO1)
summary(m.y5.SO1.num.MS) # apos have more leaves!
m.y5.SO1.num.MS.emm <- emmeans(m.y5.SO1.num.MS, c("ms"), data=H.dat.y5.SO1) #emmeans
plot(m.y5.SO1.num.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SO2 - MS
m.y5.SO2.num.MS <- glm(leaf.num~ms, family=poisson(link="log"), data=H.dat.y5.SO2)
summary(m.y5.SO2.num.MS) # apos have more leaves!
m.y5.SO2.num.MS.emm <- emmeans(m.y5.SO2.num.MS, c("ms"), data=H.dat.y5.SO2) #emmeans
plot(m.y5.SO2.num.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SS1 - MS
m.y5.SS1.num.MS <- glm(leaf.num~ms, family=poisson(link="log"), data=H.dat.y5.SS1)
summary(m.y5.SS1.num.MS) # apos have more leaves!
m.y5.SS1.num.MS.emm <- emmeans(m.y5.SS1.num.MS, c("ms"), data=H.dat.y5.SS1) #emmeans
plot(m.y5.SS1.num.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SS2 - MS
m.y5.SS2.num.MS <- glm(leaf.num~ms, family=poisson(link="log"), data=H.dat.y5.SS2)
summary(m.y5.SS2.num.MS) # apos have more leaves!
m.y5.SS2.num.MS.emm <- emmeans(m.y5.SS2.num.MS, c("ms"), data=H.dat.y5.SS2) #emmeans
plot(m.y5.SS2.num.MS.emm, comparisons = TRUE, horizontal= FALSE)
