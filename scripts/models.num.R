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

### glmer ###
# all gardens
glmm.y1.all.num.MS <- glmer(leaf.num~ms*garden+(1|pop/mom), family=poisson(link="log"), data=H.dat.y1)
summary(glmm.y1.all.num.MS)
anova(glmm.y1.all.num.MS)

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
