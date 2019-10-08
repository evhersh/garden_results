# Models (load dataprep.R first

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

###########
### EXPLORATION
####

##### 
### SURVIVAL 
####
# (apparently don't need to worry about overdispersion w/ binary data)

####
#### YEAR 1
####

### glmer ###
# all gardens
glmm.y1.all.surv.MS <- glmer(surv~ms*garden+(1|pop/mom), family=binomial(link="logit"), data=H.dat.y1)
summary(glmm.y1.all.surv.MS)
anova(glmm.y1.all.surv.MS)

# Model is nearly unidentifiable: large eigenvalue ratio - Rescale variables?
glmm2.y1.AA1.surv.MS <- glmer(surv~ms+(1|pop/mom), family=binomial(link="logit"), data=H.dat.y1.AA1)

# model singularity
glmm2.y1.AA1.surv.MS <- glmer(surv~ms+(1|pop), family=binomial(link="logit"), data=H.dat.y1.AA1)


### glm ###

# AA1 - MS
m.y1.AA1.surv.MS <- glm(surv~ms, family=binomial(link="logit"), data=H.dat.y1.AA1)
summary(m.y1.AA1.surv) # not different
m.y1.AA1.surv.MS.emm <- emmeans(m.y1.AA1.surv.MS, c("ms"), data=H.dat.y1.AA1) #emmeans
plot(m.y1.AA1.surv.MS.emm, comparisons = TRUE, horizontal= FALSE)

# AA2 - MS
m.y1.AA2.surv.MS <- glm(surv~ms, family=binomial(link="logit"), data=H.dat.y1.AA2)
summary(m.y1.AA2.surv.MS) # not different

# AO1 - MS
m.y1.AO1.surv.MS <- glm(surv~ms, family=binomial(link="logit"), data=H.dat.y1.AO1)
summary(m.y1.AO1.surv.MS) # not different

# AO2 - MS ***
m.y1.AO2.surv.MS <- glm(surv~ms, family=binomial(link="logit"), data=H.dat.y1.AO2)
summary(m.y1.AO2.surv.MS) # sexuals higher
m.y1.AO2.surv.MS.emm <- emmeans(m.y1.AO2.surv.MS, c("ms"), data=H.dat.y1.AO2) #emmeans
plot(m.y1.AO2.surv.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SO1 - MS
m.y1.SO1.surv.MS <- glm(surv~ms, family=binomial(link="logit"), data=H.dat.y1.SO1)
summary(m.y1.SO1.surv.MS) # not different

# SO2 - MS
m.y1.SO2.surv.MS <- glm(surv~ms, family=binomial(link="logit"), data=H.dat.y1.SO2)
summary(m.y1.SO2.surv.MS) # not different

# SS1 - MS
m.y1.SS1.surv.MS <- glm(surv~ms, family=binomial(link="logit"), data=H.dat.y1.SS1)
summary(m.y1.SS1.surv.MS) # not different

# SS2 - MS **
m.y1.SS2.surv.MS <- glm(surv~ms, family=binomial(link="logit"), data=H.dat.y1.SS2)
summary(m.y1.SS2.surv.MS) # sexuals higher
m.y1.SS2.surv.MS.emm <- emmeans(m.y1.SS2.surv.MS, c("ms"), data=H.dat.y1.SS2) #emmeans
plot(m.y1.SS2.surv.MS.emm, comparisons = TRUE, horizontal= FALSE)

####
#### YEAR 2
####

### glmer ###
# all gardens
glmm.y2.all.surv.MS <- glmer(surv~ms*garden+(1|pop/mom), family=binomial(link="logit"), data=H.dat.y2)
summary(glmm.y2.all.surv.MS)
anova(glmm.y2.all.surv.MS)

# Model is nearly unidentifiable: large eigenvalue ratio - Rescale variables?
glmm2.y2.AA1.surv.MS <- glmer(surv~ms+(1|pop/mom), family=binomial(link="logit"), data=H.dat.y2.AA1)

# model singularity
glmm2.y2.AA1.surv.MS <- glmer(surv~ms+(1|pop), family=binomial(link="logit"), data=H.dat.y2.AA1)


### glm ###

# AA1 - MS * sexuals have higher survival
m.y2.AA1.surv.MS <- glm(surv~ms, family=binomial(link="logit"), data=H.dat.y2.AA1)
summary(m.y2.AA1.surv.MS) # sexuals higher
m.y2.AA1.surv.MS.emm <- emmeans(m.y2.AA1.surv.MS, c("ms"), data=H.dat.y2.AA1) #emmeans
plot(m.y2.AA1.surv.MS.emm, comparisons = TRUE, horizontal= FALSE)

# AA2 - MS
m.y2.AA2.surv.MS <- glm(surv~ms, family=binomial(link="logit"), data=H.dat.y2.AA2)
summary(m.y2.AA2.surv.MS) # not different

# AO1 - MS
m.y2.AO1.surv.MS <- glm(surv~ms, family=binomial(link="logit"), data=H.dat.y2.AO1)
summary(m.y2.AO1.surv.MS) # not different

# AO2 - MS
m.y2.AO2.surv.MS <- glm(surv~ms, family=binomial(link="logit"), data=H.dat.y2.AO2)
summary(m.y2.AO2.surv.MS) # not different

# SO1 - MS
m.y2.SO1.surv.MS <- glm(surv~ms, family=binomial(link="logit"), data=H.dat.y2.SO1)
summary(m.y2.SO1.surv.MS) # not different

# SO2 - MS
m.y2.SO2.surv.MS <- glm(surv~ms, family=binomial(link="logit"), data=H.dat.y2.SO2)
summary(m.y2.SO2.surv.MS) # not different

# SS1 - MS
m.y2.SS1.surv.MS <- glm(surv~ms, family=binomial(link="logit"), data=H.dat.y2.SS1)
summary(m.y2.SS1.surv.MS) # not different

# SS2 - MS
m.y2.SS2.surv.MS <- glm(surv~ms, family=binomial(link="logit"), data=H.dat.y2.SS2)
summary(m.y2.SS2.surv.MS) # not different
m.y2.SS2.surv.MS.emm <- emmeans(m.y2.SS2.surv.MS, c("ms"), data=H.dat.y2.SS2) #emmeans
plot(m.y2.SS2.surv.MS.emm, comparisons = TRUE, horizontal= FALSE)


####
#### Year 3
####

### glmer ###
# all gardens
glmm.y3.all.surv.MS <- glmer(surv~ms*garden+(1|pop/mom), family=binomial(link="logit"), data=H.dat.y3)
summary(glmm.y3.all.surv.MS)
anova(glmm.y3.all.surv.MS)

# Model is nearly unidentifiable: large eigenvalue ratio - Rescale variables?
glmm2.y3.AA1.surv.MS <- glmer(surv~ms+(1|pop/mom), family=binomial(link="logit"), data=H.dat.y3.AA1)

# model singularity
glmm2.y3.AA1.surv.MS <- glmer(surv~ms+(1|pop), family=binomial(link="logit"), data=H.dat.y3.AA1)


### glm ###

# AA1 - MS * sexuals have higher survival
m.y3.AA1.surv.MS <- glm(surv~ms, family=binomial(link="logit"), data=H.dat.y3.AA1)
summary(m.y3.AA1.surv.MS) # sexuals higher
m.y3.AA1.surv.MS.emm <- emmeans(m.y3.AA1.surv.MS, c("ms"), data=H.dat.y3.AA1) #emmeans
plot(m.y3.AA1.surv.MS.emm, comparisons = TRUE, horizontal= FALSE)

# AA2 - MS
m.y3.AA2.surv.MS <- glm(surv~ms, family=binomial(link="logit"), data=H.dat.y3.AA2)
summary(m.y3.AA2.surv.MS) # not different

# AO1 - MS
m.y3.AO1.surv.MS <- glm(surv~ms, family=binomial(link="logit"), data=H.dat.y3.AO1)
summary(m.y3.AO1.surv.MS) # not different

# AO2 - MS
m.y3.AO2.surv.MS <- glm(surv~ms, family=binomial(link="logit"), data=H.dat.y3.AO2)
summary(m.y3.AO2.surv.MS) # not different

# SO1 - MS
m.y3.SO1.surv.MS <- glm(surv~ms, family=binomial(link="logit"), data=H.dat.y3.SO1)
summary(m.y3.SO1.surv.MS) # not different

# SO2 - MS
m.y3.SO2.surv.MS <- glm(surv~ms, family=binomial(link="logit"), data=H.dat.y3.SO2)
summary(m.y3.SO2.surv.MS) # not different

# SS1 - MS * sexuals have higher survival
m.y3.SS1.surv.MS <- glm(surv~ms, family=binomial(link="logit"), data=H.dat.y3.SS1)
summary(m.y3.SS1.surv.MS) # sexuals higher survival
m.y3.SS1.surv.MS.emm <- emmeans(m.y3.SS1.surv.MS, c("ms"), data=H.dat.y3.SS1) #emmeans
plot(m.y3.SS1.surv.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SS2 - MS
m.y3.SS2.surv.MS <- glm(surv~ms, family=binomial(link="logit"), data=H.dat.y3.SS2)
summary(m.y3.SS2.surv.MS) # not different
m.y3.SS2.surv.MS.emm <- emmeans(m.y3.SS2.surv.MS, c("ms"), data=H.dat.y3.SS2) #emmeans
plot(m.y3.SS2.surv.MS.emm, comparisons = TRUE, horizontal= FALSE)


####
#### Year 4
####

### glmer ###
# all gardens
glmm.y4.all.surv.MS <- glmer(surv~ms*garden+(1|pop/mom), family=binomial(link="logit"), data=H.dat.y4)
summary(glmm.y4.all.surv.MS)
anova(glmm.y4.all.surv.MS)

# Model is nearly unidentifiable: large eigenvalue ratio - Rescale variables?
glmm2.y4.AA1.surv.MS <- glmer(surv~ms+(1|pop/mom), family=binomial(link="logit"), data=H.dat.y4.AA1)

# model singularity
glmm2.y4.AA1.surv.MS <- glmer(surv~ms+(1|pop), family=binomial(link="logit"), data=H.dat.y4.AA1)
summary(glmm2.y4.AA1.surv.MS)

### glm ###

# AA1 - MS * sexuals have higher survival
m.y4.AA1.surv.MS <- glm(surv~ms, family=binomial(link="logit"), data=H.dat.y4.AA1)
summary(m.y4.AA1.surv.MS) # not different
m.y4.AA1.surv.MS.emm <- emmeans(m.y4.AA1.surv.MS, c("ms"), data=H.dat.y4.AA1) #emmeans
plot(m.y4.AA1.surv.MS.emm, comparisons = TRUE, horizontal= FALSE)

# AA2 - MS
m.y4.AA2.surv.MS <- glm(surv~ms, family=binomial(link="logit"), data=H.dat.y4.AA2)
summary(m.y4.AA2.surv.MS) # not different

# AO1 - MS
m.y4.AO1.surv.MS <- glm(surv~ms, family=binomial(link="logit"), data=H.dat.y4.AO1)
summary(m.y4.AO1.surv.MS) # not different

# AO2 - MS
m.y4.AO2.surv.MS <- glm(surv~ms, family=binomial(link="logit"), data=H.dat.y4.AO2)
summary(m.y4.AO2.surv.MS) # not different

# SO1 - MS
m.y4.SO1.surv.MS <- glm(surv~ms, family=binomial(link="logit"), data=H.dat.y4.SO1)
summary(m.y4.SO1.surv.MS) # not different

# SO2 - MS
m.y4.SO2.surv.MS <- glm(surv~ms, family=binomial(link="logit"), data=H.dat.y4.SO2)
summary(m.y4.SO2.surv.MS) # not different

# SS1 - MS * sexuals have higher survival
m.y4.SS1.surv.MS <- glm(surv~ms, family=binomial(link="logit"), data=H.dat.y4.SS1)
summary(m.y4.SS1.surv.MS) # sexuals higher survival
m.y4.SS1.surv.MS.emm <- emmeans(m.y4.SS1.surv.MS, c("ms"), data=H.dat.y4.SS1) #emmeans
plot(m.y4.SS1.surv.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SS2 - MS
m.y4.SS2.surv.MS <- glm(surv~ms, family=binomial(link="logit"), data=H.dat.y4.SS2)
summary(m.y4.SS2.surv.MS) # not different
m.y4.SS2.surv.MS.emm <- emmeans(m.y4.SS2.surv.MS, c("ms"), data=H.dat.y4.SS2) #emmeans
plot(m.y4.SS2.surv.MS.emm, comparisons = TRUE, horizontal= FALSE)


####
#### Year 5
####


### glmer ###
# all gardens
glmm.y5.all.surv.MS <- glmer(surv~ms*garden+(1|pop/mom), family=binomial(link="logit"), data=H.dat.y5)
summary(glmm.y5.all.surv.MS)
anova(glmm.y5.all.surv.MS)

# Model is nearly unidentifiable: large eigenvalue ratio - Rescale variables?
glmm2.y5.AA1.surv.MS <- glmer(surv~ms+(1|pop/mom), family=binomial(link="logit"), data=H.dat.y5.AA1)

# model singularity
glmm2.y5.AA1.surv.MS <- glmer(surv~ms+(1|pop), family=binomial(link="logit"), data=H.dat.y5.AA1)
summary(glmm2.y5.AA1.surv.MS)

### glm ###

# AA1 - MS * sexuals have higher survival
m.y5.AA1.surv.MS <- glm(surv~ms, family=binomial(link="logit"), data=H.dat.y5.AA1)
summary(m.y5.AA1.surv.MS) # not different
m.y5.AA1.surv.MS.emm <- emmeans(m.y5.AA1.surv.MS, c("ms"), data=H.dat.y5.AA1) #emmeans
plot(m.y5.AA1.surv.MS.emm, comparisons = TRUE, horizontal= FALSE)

# AA2 - MS
m.y5.AA2.surv.MS <- glm(surv~ms, family=binomial(link="logit"), data=H.dat.y5.AA2)
summary(m.y5.AA2.surv.MS) # not different

# AO1 - MS
m.y5.AO1.surv.MS <- glm(surv~ms, family=binomial(link="logit"), data=H.dat.y5.AO1)
summary(m.y5.AO1.surv.MS) # not different

# AO2 - MS
m.y5.AO2.surv.MS <- glm(surv~ms, family=binomial(link="logit"), data=H.dat.y5.AO2)
summary(m.y5.AO2.surv.MS) # not different

# SO1 - MS
m.y5.SO1.surv.MS <- glm(surv~ms, family=binomial(link="logit"), data=H.dat.y5.SO1)
summary(m.y5.SO1.surv.MS) # not different

# SO2 - MS
m.y5.SO2.surv.MS <- glm(surv~ms, family=binomial(link="logit"), data=H.dat.y5.SO2)
summary(m.y5.SO2.surv.MS) # not different

# SS1 - MS * sexuals have higher survival
m.y5.SS1.surv.MS <- glm(surv~ms, family=binomial(link="logit"), data=H.dat.y5.SS1)
summary(m.y5.SS1.surv.MS) # sexuals higher survival
m.y5.SS1.surv.MS.emm <- emmeans(m.y5.SS1.surv.MS, c("ms"), data=H.dat.y5.SS1) #emmeans
plot(m.y5.SS1.surv.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SS2 - MS
m.y5.SS2.surv.MS <- glm(surv~ms, family=binomial(link="logit"), data=H.dat.y5.SS2)
summary(m.y5.SS2.surv.MS) # not different
m.y5.SS2.surv.MS.emm <- emmeans(m.y5.SS2.surv.MS, c("ms"), data=H.dat.y5.SS2) #emmeans
plot(m.y5.SS2.surv.MS.emm, comparisons = TRUE, horizontal= FALSE)



#####
### LEAF LENGTH
####

####
#### Year 1
####

# all gardens
lmer.y1.all.surv.MS <- lmer(leaf.length~ms*garden+(1|pop/mom), data=H.dat.y1)
summary(lmer.y1.all.surv.MS)
anova(lmer.y1.all.surv.MS)

# model singularity
lmer1.y1.AA1.surv.MS <- lmer(surv~ms+(1|pop/mom), data=H.dat.y1.AA1)

# model singularity
lmer2.y1.AA1.surv.MS <- lmer(surv~ms+(1|pop), data=H.dat.y1.AA1)


### lm ###

# AA1 - MS
m.y1.AA1.length.MS <- lm(leaf.length~ms, data=H.dat.y1.AA1)
summary(m.y1.AA1.length.MS) # sexuals have longer leaves
m.y1.AA1.length.MS.emm <- emmeans(m.y1.AA1.length.MS, c("ms"), data=H.dat.y1.AA1) #emmeans
plot(m.y1.AA1.length.MS.emm, comparisons = TRUE, horizontal= FALSE)

# AA2 - MS
m.y1.AA2.length.MS <- lm(leaf.length~ms, data=H.dat.y1.AA2)
summary(m.y1.AA2.length.MS) # no difference
m.y1.AA2.length.MS.emm <- emmeans(m.y1.AA2.length.MS, c("ms"), data=H.dat.y1.AA2) #emmeans
plot(m.y1.AA2.length.MS.emm, comparisons = TRUE, horizontal= FALSE)

# AO1 - MS
m.y1.AO1.length.MS <- lm(leaf.length~ms, data=H.dat.y1.AO1)
summary(m.y1.AO1.length.MS) # Sexuals have longer leaves
m.y1.AO1.length.MS.emm <- emmeans(m.y1.AO1.length.MS, c("ms"), data=H.dat.y1.AO1) #emmeans
plot(m.y1.AO1.length.MS.emm, comparisons = TRUE, horizontal= FALSE)

# AO2 - MS
m.y1.AO2.length.MS <- lm(leaf.length~ms, data=H.dat.y1.AO2)
summary(m.y1.AO2.length.MS) # no difference
m.y1.AO2.length.MS.emm <- emmeans(m.y1.AO2.length.MS, c("ms"), data=H.dat.y1.AO2) #emmeans
plot(m.y1.AO2.length.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SO1 - MS
m.y1.SO1.length.MS <- lm(leaf.length~ms, data=H.dat.y1.SO1)
summary(m.y1.SO1.length.MS) # no difference
m.y1.SO1.length.MS.emm <- emmeans(m.y1.SO1.length.MS, c("ms"), data=H.dat.y1.SO1) #emmeans
plot(m.y1.SO1.length.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SO2 - MS
m.y1.SO2.length.MS <- lm(leaf.length~ms, data=H.dat.y1.SO2)
summary(m.y1.SO2.length.MS) # no difference
m.y1.SO2.length.MS.emm <- emmeans(m.y1.SO2.length.MS, c("ms"), data=H.dat.y1.SO2) #emmeans
plot(m.y1.SO2.length.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SS1 - MS
m.y1.SS1.length.MS <- lm(leaf.length~ms, data=H.dat.y1.SS1)
summary(m.y1.SS1.length.MS) # Sexuals have longer leaves
m.y1.SS1.length.MS.emm <- emmeans(m.y1.SS1.length.MS, c("ms"), data=H.dat.y1.SS1) #emmeans
plot(m.y1.SS1.length.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SS2 - MS
m.y1.SS2.length.MS <- lm(leaf.length~ms, data=H.dat.y1.SS2)
summary(m.y1.SS2.length.MS) # Sexuals have longer leaves
m.y1.SS2.length.MS.emm <- emmeans(m.y1.SS2.length.MS, c("ms"), data=H.dat.y1.SS2) #emmeans
plot(m.y1.SS2.length.MS.emm, comparisons = TRUE, horizontal= FALSE)



####
#### Year 2
####

# all gardens
lmer.y2.all.surv.MS <- lmer(leaf.length~ms*garden+(1|pop/mom), data=H.dat.y2)
summary(lmer.y2.all.surv.MS)
anova(lmer.y2.all.surv.MS)

# model singularity
lmer1.y2.AA1.surv.MS <- lmer(surv~ms+(1|pop/mom), data=H.dat.y2.AA1)

# model singularity
lmer2.y2.AA1.surv.MS <- lmer(surv~ms+(1|pop), data=H.dat.y2.AA1)


### lm ###

# AA1 - MS
m.y2.AA1.length.MS <- lm(leaf.length~ms, data=H.dat.y2.AA1)
summary(m.y2.AA1.length.MS) # no diff
m.y2.AA1.length.MS.emm <- emmeans(m.y2.AA1.length.MS, c("ms"), data=H.dat.y2.AA1) #emmeans
plot(m.y2.AA1.length.MS.emm, comparisons = TRUE, horizontal= FALSE)

# AA2 - MS
m.y2.AA2.length.MS <- lm(leaf.length~ms, data=H.dat.y2.AA2)
summary(m.y2.AA2.length.MS) # no difference
m.y2.AA2.length.MS.emm <- emmeans(m.y2.AA2.length.MS, c("ms"), data=H.dat.y2.AA2) #emmeans
plot(m.y2.AA2.length.MS.emm, comparisons = TRUE, horizontal= FALSE)

# AO1 - MS
m.y2.AO1.length.MS <- lm(leaf.length~ms, data=H.dat.y2.AO1)
summary(m.y2.AO1.length.MS) # no diff
m.y2.AO1.length.MS.emm <- emmeans(m.y2.AO1.length.MS, c("ms"), data=H.dat.y2.AO1) #emmeans
plot(m.y2.AO1.length.MS.emm, comparisons = TRUE, horizontal= FALSE)

# AO2 - MS
m.y2.AO2.length.MS <- lm(leaf.length~ms, data=H.dat.y2.AO2)
summary(m.y2.AO2.length.MS) # no difference
m.y2.AO2.length.MS.emm <- emmeans(m.y2.AO2.length.MS, c("ms"), data=H.dat.y2.AO2) #emmeans
plot(m.y2.AO2.length.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SO1 - MS
m.y2.SO1.length.MS <- lm(leaf.length~ms, data=H.dat.y2.SO1)
summary(m.y2.SO1.length.MS) # no difference
m.y2.SO1.length.MS.emm <- emmeans(m.y2.SO1.length.MS, c("ms"), data=H.dat.y2.SO1) #emmeans
plot(m.y2.SO1.length.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SO2 - MS
m.y2.SO2.length.MS <- lm(leaf.length~ms, data=H.dat.y2.SO2)
summary(m.y2.SO2.length.MS) # asexuals have longer leaves
m.y2.SO2.length.MS.emm <- emmeans(m.y2.SO2.length.MS, c("ms"), data=H.dat.y2.SO2) #emmeans
plot(m.y2.SO2.length.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SS1 - MS
m.y2.SS1.length.MS <- lm(leaf.length~ms, data=H.dat.y2.SS1)
summary(m.y2.SS1.length.MS) # Sexuals have longer leaves
m.y2.SS1.length.MS.emm <- emmeans(m.y2.SS1.length.MS, c("ms"), data=H.dat.y2.SS1) #emmeans
plot(m.y2.SS1.length.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SS2 - MS
m.y2.SS2.length.MS <- lm(leaf.length~ms, data=H.dat.y2.SS2)
summary(m.y2.SS2.length.MS) # Sexuals have longer leaves
m.y2.SS2.length.MS.emm <- emmeans(m.y2.SS2.length.MS, c("ms"), data=H.dat.y2.SS2) #emmeans
plot(m.y2.SS2.length.MS.emm, comparisons = TRUE, horizontal= FALSE)



####
#### Year 3
####

# all gardens
lmer.y3.all.surv.MS <- lmer(leaf.length~ms*garden+(1|pop/mom), data=H.dat.y3)
summary(lmer.y3.all.surv.MS)
anova(lmer.y3.all.surv.MS)

# model singularity
lmer1.y3.AA1.surv.MS <- lmer(surv~ms+(1|pop/mom), data=H.dat.y3.AA1)

# model singularity
lmer2.y3.AA1.surv.MS <- lmer(surv~ms+(1|pop), data=H.dat.y3.AA1)


### lm ###

# AA1 - MS
m.y3.AA1.length.MS <- lm(leaf.length~ms, data=H.dat.y3.AA1)
summary(m.y3.AA1.length.MS) # sexuals have longer leaves in AA1
m.y3.AA1.length.MS.emm <- emmeans(m.y3.AA1.length.MS, c("ms"), data=H.dat.y3.AA1) #emmeans
plot(m.y3.AA1.length.MS.emm, comparisons = TRUE, horizontal= FALSE)

# AA2 - MS
m.y3.AA2.length.MS <- lm(leaf.length~ms, data=H.dat.y3.AA2)
summary(m.y3.AA2.length.MS) # no difference
m.y3.AA2.length.MS.emm <- emmeans(m.y3.AA2.length.MS, c("ms"), data=H.dat.y3.AA2) #emmeans
plot(m.y3.AA2.length.MS.emm, comparisons = TRUE, horizontal= FALSE)

# AO1 - MS
m.y3.AO1.length.MS <- lm(leaf.length~ms, data=H.dat.y3.AO1)
summary(m.y3.AO1.length.MS) # Sex longer
m.y3.AO1.length.MS.emm <- emmeans(m.y3.AO1.length.MS, c("ms"), data=H.dat.y3.AO1) #emmeans
plot(m.y3.AO1.length.MS.emm, comparisons = TRUE, horizontal= FALSE)

# AO2 - MS
m.y3.AO2.length.MS <- lm(leaf.length~ms, data=H.dat.y3.AO2)
summary(m.y3.AO2.length.MS) # no difference
m.y3.AO2.length.MS.emm <- emmeans(m.y3.AO2.length.MS, c("ms"), data=H.dat.y3.AO2) #emmeans
plot(m.y3.AO2.length.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SO1 - MS
m.y3.SO1.length.MS <- lm(leaf.length~ms, data=H.dat.y3.SO1)
summary(m.y3.SO1.length.MS) # no difference
m.y3.SO1.length.MS.emm <- emmeans(m.y3.SO1.length.MS, c("ms"), data=H.dat.y3.SO1) #emmeans
plot(m.y3.SO1.length.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SO2 - MS
m.y3.SO2.length.MS <- lm(leaf.length~ms, data=H.dat.y3.SO2)
summary(m.y3.SO2.length.MS) # asexuals have longer leaves
m.y3.SO2.length.MS.emm <- emmeans(m.y3.SO2.length.MS, c("ms"), data=H.dat.y3.SO2) #emmeans
plot(m.y3.SO2.length.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SS1 - MS
m.y3.SS1.length.MS <- lm(leaf.length~ms, data=H.dat.y3.SS1)
summary(m.y3.SS1.length.MS) # Sexuals have longer leaves
m.y3.SS1.length.MS.emm <- emmeans(m.y3.SS1.length.MS, c("ms"), data=H.dat.y3.SS1) #emmeans
plot(m.y3.SS1.length.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SS2 - MS
m.y3.SS2.length.MS <- lm(leaf.length~ms, data=H.dat.y3.SS2)
summary(m.y3.SS2.length.MS) # Sexuals have longer leaves
m.y3.SS2.length.MS.emm <- emmeans(m.y3.SS2.length.MS, c("ms"), data=H.dat.y3.SS2) #emmeans
plot(m.y3.SS2.length.MS.emm, comparisons = TRUE, horizontal= FALSE)



####
#### Year 4
####

# all gardens
lmer.y4.all.surv.MS <- lmer(leaf.length~ms*garden+(1|pop/mom), data=H.dat.y4)
summary(lmer.y4.all.surv.MS)
anova(lmer.y4.all.surv.MS)

# model singularity
lmer1.y4.AA1.surv.MS <- lmer(surv~ms+(1|pop/mom), data=H.dat.y4.AA1)

# model singularity
lmer2.y4.AA1.surv.MS <- lmer(surv~ms+(1|pop), data=H.dat.y4.AA1)


### lm ###

# AA1 - MS
m.y4.AA1.length.MS <- lm(leaf.length~ms, data=H.dat.y4.AA1)
summary(m.y4.AA1.length.MS) # sexuals have longer leaves in AA1
m.y4.AA1.length.MS.emm <- emmeans(m.y4.AA1.length.MS, c("ms"), data=H.dat.y4.AA1) #emmeans
plot(m.y4.AA1.length.MS.emm, comparisons = TRUE, horizontal= FALSE)

# AA2 - MS
m.y4.AA2.length.MS <- lm(leaf.length~ms, data=H.dat.y4.AA2)
summary(m.y4.AA2.length.MS) # no difference
m.y4.AA2.length.MS.emm <- emmeans(m.y4.AA2.length.MS, c("ms"), data=H.dat.y4.AA2) #emmeans
plot(m.y4.AA2.length.MS.emm, comparisons = TRUE, horizontal= FALSE)

# AO1 - MS
m.y4.AO1.length.MS <- lm(leaf.length~ms, data=H.dat.y4.AO1)
summary(m.y4.AO1.length.MS) # Sex longer
m.y4.AO1.length.MS.emm <- emmeans(m.y4.AO1.length.MS, c("ms"), data=H.dat.y4.AO1) #emmeans
plot(m.y4.AO1.length.MS.emm, comparisons = TRUE, horizontal= FALSE)

# AO2 - MS
m.y4.AO2.length.MS <- lm(leaf.length~ms, data=H.dat.y4.AO2)
summary(m.y4.AO2.length.MS) # no difference
m.y4.AO2.length.MS.emm <- emmeans(m.y4.AO2.length.MS, c("ms"), data=H.dat.y4.AO2) #emmeans
plot(m.y4.AO2.length.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SO1 - MS
m.y4.SO1.length.MS <- lm(leaf.length~ms, data=H.dat.y4.SO1)
summary(m.y4.SO1.length.MS) # sex longer
m.y4.SO1.length.MS.emm <- emmeans(m.y4.SO1.length.MS, c("ms"), data=H.dat.y4.SO1) #emmeans
plot(m.y4.SO1.length.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SO2 - MS
m.y4.SO2.length.MS <- lm(leaf.length~ms, data=H.dat.y4.SO2)
summary(m.y4.SO2.length.MS) # asexuals have longer leaves
m.y4.SO2.length.MS.emm <- emmeans(m.y4.SO2.length.MS, c("ms"), data=H.dat.y4.SO2) #emmeans
plot(m.y4.SO2.length.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SS1 - MS
m.y4.SS1.length.MS <- lm(leaf.length~ms, data=H.dat.y4.SS1)
summary(m.y4.SS1.length.MS) # Sexuals have longer leaves
m.y4.SS1.length.MS.emm <- emmeans(m.y4.SS1.length.MS, c("ms"), data=H.dat.y4.SS1) #emmeans
plot(m.y4.SS1.length.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SS2 - MS
m.y4.SS2.length.MS <- lm(leaf.length~ms, data=H.dat.y4.SS2)
summary(m.y4.SS2.length.MS) # Sexuals have longer leaves
m.y4.SS2.length.MS.emm <- emmeans(m.y4.SS2.length.MS, c("ms"), data=H.dat.y4.SS2) #emmeans
plot(m.y4.SS2.length.MS.emm, comparisons = TRUE, horizontal= FALSE)



####
#### Year 5
####


# all gardens
lmer.y5.all.surv.MS <- lmer(leaf.length~ms*garden+(1|pop/mom), data=H.dat.y5)
summary(lmer.y5.all.surv.MS)
anova(lmer.y5.all.surv.MS)

# model singularity
lmer1.y5.AA1.surv.MS <- lmer(surv~ms+(1|pop/mom), data=H.dat.y5.AA1)

# model singularity
lmer2.y5.AA1.surv.MS <- lmer(surv~ms+(1|pop), data=H.dat.y5.AA1)


### lm ###

# AA1 - MS
m.y5.AA1.length.MS <- lm(leaf.length~ms, data=H.dat.y5.AA1)
summary(m.y5.AA1.length.MS) # sexuals have longer leaves in AA1
m.y5.AA1.length.MS.emm <- emmeans(m.y5.AA1.length.MS, c("ms"), data=H.dat.y5.AA1) #emmeans
plot(m.y5.AA1.length.MS.emm, comparisons = TRUE, horizontal= FALSE)

# AA2 - MS
m.y5.AA2.length.MS <- lm(leaf.length~ms, data=H.dat.y5.AA2)
summary(m.y5.AA2.length.MS) # no difference
m.y5.AA2.length.MS.emm <- emmeans(m.y5.AA2.length.MS, c("ms"), data=H.dat.y5.AA2) #emmeans
plot(m.y5.AA2.length.MS.emm, comparisons = TRUE, horizontal= FALSE)

# AO1 - MS
m.y5.AO1.length.MS <- lm(leaf.length~ms, data=H.dat.y5.AO1)
summary(m.y5.AO1.length.MS) # Sex longer
m.y5.AO1.length.MS.emm <- emmeans(m.y5.AO1.length.MS, c("ms"), data=H.dat.y5.AO1) #emmeans
plot(m.y5.AO1.length.MS.emm, comparisons = TRUE, horizontal= FALSE)

# AO2 - MS
m.y5.AO2.length.MS <- lm(leaf.length~ms, data=H.dat.y5.AO2)
summary(m.y5.AO2.length.MS) # no difference
m.y5.AO2.length.MS.emm <- emmeans(m.y5.AO2.length.MS, c("ms"), data=H.dat.y5.AO2) #emmeans
plot(m.y5.AO2.length.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SO1 - MS
m.y5.SO1.length.MS <- lm(leaf.length~ms, data=H.dat.y5.SO1)
summary(m.y5.SO1.length.MS) # Sexuals all dead
m.y5.SO1.length.MS.emm <- emmeans(m.y5.SO1.length.MS, c("ms"), data=H.dat.y5.SO1) #emmeans
plot(m.y5.SO1.length.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SO2 - MS
m.y5.SO2.length.MS <- lm(leaf.length~ms, data=H.dat.y5.SO2)
summary(m.y5.SO2.length.MS) # no diff
m.y5.SO2.length.MS.emm <- emmeans(m.y5.SO2.length.MS, c("ms"), data=H.dat.y5.SO2) #emmeans
plot(m.y5.SO2.length.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SS1 - MS
m.y5.SS1.length.MS <- lm(leaf.length~ms, data=H.dat.y5.SS1)
summary(m.y5.SS1.length.MS) # no diff
m.y5.SS1.length.MS.emm <- emmeans(m.y5.SS1.length.MS, c("ms"), data=H.dat.y5.SS1) #emmeans
plot(m.y5.SS1.length.MS.emm, comparisons = TRUE, horizontal= FALSE)

# SS2 - MS
m.y5.SS2.length.MS <- lm(leaf.length~ms, data=H.dat.y5.SS2)
summary(m.y5.SS2.length.MS) # Sexuals have longer leaves
m.y5.SS2.length.MS.emm <- emmeans(m.y5.SS2.length.MS, c("ms"), data=H.dat.y5.SS2) #emmeans
plot(m.y5.SS2.length.MS.emm, comparisons = TRUE, horizontal= FALSE)
