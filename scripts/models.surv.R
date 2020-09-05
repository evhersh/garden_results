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
library(AER)

glmer.surv.y1 <- glmer(surv~s.region*g.region+(1|pop)+(1|garden), family=binomial(link="logit"), data=H.dat.y1)
glmer.surv.y1.2 <- glmer(surv~s.region+g.region+(1|pop)+(1|garden), family=binomial(link="logit"), data=H.dat.y1)
anova(glmer.surv.y1, glmer.surv.y1.2)

glmer.surv.y2 <- glmer(surv~s.region*g.region+(1|pop)+(1|garden), family=binomial(link="logit"), data=H.dat.y2)
glmer.surv.y2.2 <- glmer(surv~s.region+g.region+(1|pop)+(1|garden), family=binomial(link="logit"), data=H.dat.y2)
anova(glmer.surv.y2, glmer.surv.y2.2)

glm.surv.y3 <- glm(surv~s.region*g.region, family=binomial(link="logit"), data=H.dat.y3)
glm.surv.y3.2 <- glm(surv~s.region+g.region, family=binomial(link="logit"), data=H.dat.y3)
lrtest(glm.surv.y3, glm.surv.y3.2)

glmer.surv.y4 <- glmer(surv~s.region*g.region+(1|garden), family=binomial(link="logit"), data=H.dat.y4)
glmer.surv.y4.2 <- glmer(surv~s.region+g.region+(1|garden), family=binomial(link="logit"), data=H.dat.y4)
anova(glmer.surv.y4, glmer.surv.y4.2)

glm.surv.y5 <- glm(surv~s.region*g.region, family=binomial(link="logit"), data=H.dat.y5)
glm.surv.y5.2 <- glm(surv~s.region+g.region, family=binomial(link="logit"), data=H.dat.y5)
lrtest(glm.surv.y5, glm.surv.y5.2)

glmer.surv.y1.ggfx <- ggpredict(glmer.surv.y1, terms=c("g.region", "s.region"))
plot(glmer.surv.y1.ggfx)

glmer.surv.y2.ggfx <- ggpredict(glmer.surv.y2, terms=c("g.region", "s.region"))
plot(glmer.surv.y2.ggfx)

glm.surv.y3.ggfx <- ggpredict(glm.surv.y3, terms=c("g.region", "s.region"))
plot(glm.surv.y3.ggfx)

glmer.surv.y4.ggfx <- ggpredict(glmer.surv.y4, terms=c("g.region", "s.region"))
plot(glmer.surv.y4.ggfx)

glm.surv.y5.ggfx <- ggpredict(glm.surv.y5, terms=c("g.region", "s.region"))
plot(glm.surv.y5.ggfx)

glmer.surv.y1.ggfxdf <- as.data.frame(glmer.surv.y1.ggfx)
glmer.surv.y2.ggfxdf <- as.data.frame(glmer.surv.y2.ggfx)
glm.surv.y3.ggfxdf <- as.data.frame(glm.surv.y3.ggfx)
glmer.surv.y4.ggfxdf <- as.data.frame(glmer.surv.y4.ggfx)
glm.surv.y5.ggfxdf <- as.data.frame(glm.surv.y5.ggfx)




###########
### EXPLORATION
####

####
### SURVIVAL 
#####
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
glmm1.y1.AA1.surv.MS <- glmer(surv~ms+(1|pop/mom), family=binomial(link="logit"), data=H.dat.y1.AA1)
summary(glmm2.y1.AA1.surv.MS)

# model singularity
glmm2.y1.AA1.surv.MS <- glmer(surv~ms+(1|pop), family=binomial(link="logit"), data=H.dat.y1.AA1)
summary(glmm2.y1.AA1.surv.MS)

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

