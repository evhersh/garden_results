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


####
### LEAF LENGTH
#####

# all gardens y1
lmer.length.y1.0 <- lmer(leaf.length~ms*g.region+(1|garden/pop/mom), data=H.dat.y1)
summary(lmer.length.y1.0)

lmer.length.y1.1 <- lmer(leaf.length~ms*garden+(1|pop/mom), data=H.dat.y1)
summary(lmer.length.y1.1)

# garden model fits better?
lrtest(lmer.length.y1.0, lmer.length.y1.1)

lmer.length.y1.0.ggfx <- ggpredict(lmer.length.y1.0, terms=c("g.region", "ms"))
plot(lmer.length.y1.0.ggfx)

lmer.length.y1.1.ggfx <- ggpredict(lmer.length.y1.1, terms=c("garden", "ms"))
plot(lmer.length.y1.1.ggfx) # sexuals have longer leaves in SS1


# all gardens y2
lmer.length.y2.0 <- lmer(leaf.length~ms*g.region+(1|garden/pop/mom), data=H.dat.y2)
summary(lmer.length.y2.0)

lmer.length.y2.1 <- lmer(leaf.length~ms*garden+(1|pop/mom), data=H.dat.y2)
summary(lmer.length.y2.1)

# garden model fits better?
lrtest(lmer.length.y2.0, lmer.length.y2.1)

lmer.length.y2.0.ggfx <- ggpredict(lmer.length.y2.0, terms=c("g.region", "ms"))
plot(lmer.length.y2.0.ggfx)

lmer.length.y2.1.ggfx <- ggpredict(lmer.length.y2.1, terms=c("garden", "ms"))
plot(lmer.length.y2.1.ggfx) # sexuals have longer leaves in SS1 and SS2


# all gardens y3
# doesnt converge
lmer.length.y3.0 <- lmer(leaf.length~ms*g.region+(1|garden/pop/mom), data=H.dat.y3)
summary(lmer.length.y3.0)

# doesn't converge
lmer.length.y3.1 <- lmer(leaf.length~ms*garden+(1|pop/mom), data=H.dat.y3)
summary(lmer.length.y3.1)

# these do converge
lmer.length.y3.2 <- lmer(leaf.length~ms*g.region+(1|garden/pop), data=H.dat.y3)
summary(lmer.length.y3.2)

lmer.length.y3.3 <- lmer(leaf.length~ms*garden+(1|pop), data=H.dat.y3)
summary(lmer.length.y3.3)

# garden model fits better?
lrtest(lmer.length.y3.2, lmer.length.y3.3) # garden model fits better

lmer.length.y3.2.ggfx <- ggpredict(lmer.length.y3.2, terms=c("g.region", "ms"))
plot(lmer.length.y3.2.ggfx) # sexuals have longer leaves in S.g

lmer.length.y3.3.ggfx <- ggpredict(lmer.length.y3.3, terms=c("garden", "ms"))
plot(lmer.length.y3.3.ggfx) # sexuals have longer leaves in SS1 and AA1 (marginal)


# all gardens y4
# doesnt converge
lmer.length.y4.0 <- lmer(leaf.length~ms*g.region+(1|garden/pop/mom), data=H.dat.y4)
summary(lmer.length.y4.0)
# removing random factors doesn't help g.region models converge
lmer.length.y4.1 <- lmer(leaf.length~ms*g.region+(1|garden), data=H.dat.y4)
summary(lmer.length.y4.0)

lmer.length.y4.2 <- lmer(leaf.length~ms*garden+(1|pop/mom), data=H.dat.y4)
summary(lmer.length.y4.2)

# garden model fits better?
#lrtest(lmer.length.y4.0, lmer.length.y4.1)

# lmer.length.y4.0.ggfx <- ggpredict(lmer.length.y4.0, terms=c("g.region", "ms"))
# plot(lmer.length.y4.0.ggfx)

lmer.length.y4.2.ggfx <- ggpredict(lmer.length.y4.2, terms=c("garden", "ms"))
plot(lmer.length.y4.2.ggfx) # sexuals have (marginally) longer leaves in SS1, SS2, and SO1 (best)


# all gardens y5
lmer.length.y5.0 <- lmer(leaf.length~ms*g.region+(1|garden/pop/mom), data=H.dat.y5)
summary(lmer.length.y5.0)

# doesn't converge (probably one garden with no survivors)
lmer.length.y5.1 <- lmer(leaf.length~ms*garden+(1|pop/mom), data=H.dat.y5)
summary(lmer.length.y5.1)

lmer.length.y5.2 <- lmer(leaf.length~ms*garden+(1|pop), data=H.dat.y5)
summary(lmer.length.y5.2)

lm.length.y5.3 <- lm(leaf.length~ms*garden, data=H.dat.y5)
summary(lm.length.y5.3) # no apos surviving in SO1 in y5

# garden model fits better?
#lrtest(lmer.length.y5.0, lmer.length.y5.1)

lmer.length.y5.0.ggfx <- ggpredict(lmer.length.y5.0, terms=c("g.region", "ms"))
plot(lmer.length.y5.0.ggfx) # no diffs

lm.length.y5.3.ggfx <- ggpredict(lm.length.y5.3, terms=c("garden", "ms"))
plot(lm.length.y5.3.ggfx)+
  labs(y= "predicted leaf length", x="garden", title="predicted leaf length in year 5")# sexuals have longer leaves in SS2 






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