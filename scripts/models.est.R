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

glmm.est.overall <- glmer(germ.10 ~ ms + (1|g.region/garden/pop), family=binomial(link="logit"), data=G.dat, control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))
glmm.est.overall2 <- glmer(germ.10 ~ (1|g.region/garden/pop), family=binomial(link="logit"), data=G.dat, control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))

lrtest(glmm.est.overall, glmm.est.overall2)

summary(glmm.est.overall)
glmm.est.overall.ggfx <- ggpredict(glmm.est.overall, terms=c("ms"))
plot(glmm.est.overall.ggfx)

# convergence issues
glmm.est.0 <- glmer(germ.10 ~ ms*g.region + (1|garden/pop/mom), family=binomial(link="logit"), data=G.dat, control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))
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


glm.est <- glmer(germ~ms+(1|pop), family=binomial(link="logit"), data=G.dat)
glm.est.ggfx <- ggpredict(glm.est, terms=c("ms"))
plot(glm.est.ggfx)

est.mean <- G.dat %>%
  group_by(ms) %>%
  summarize(mean.est = mean(germ, na.rm=TRUE))

#### works now???? ####
est.mod <- glmer(germ ~ s.region*g.region + (1|garden/pop/mom), family=binomial(link="logit"), data=G.dat, control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))
summary(est.mod)

glmer.est.full <- glmer(germ ~ s.region*g.region + (1|garden), family=binomial(link="logit"), data=G.dat, control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))
glmer.est.noX <- glmer(germ ~ s.region+g.region + (1|garden), family=binomial(link="logit"), data=G.dat)
glmer.est.noS <- glmer(germ ~ g.region + (1|garden), family=binomial(link="logit"), data=G.dat)
glmer.est.noG <- glmer(germ ~ s.region + (1|garden), family=binomial(link="logit"), data=G.dat)

anova(glmer.est.full, glmer.est.noX)
anova(glmer.est.noX, glmer.est.noS)
anova(glmer.est.noX, glmer.est.noG)

glmer.est.full.ggfx <- ggpredict(glmer.est.full, terms=c("s.region"))
plot(glmer.est.full.ggfx)

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

glmm.est.5.ggfx <- ggpredict(glmer.est.full, terms=c("g.region", "s.region"))
glmm.est.5.ggfx.df <- as.data.frame(glmm.est.5.ggfx)
plot(glmm.est.5.ggfx)+
  labs(y="predicted establishment success", x="Mating System", title="")+
  scale_color_manual(values=c("#F8766D", "orange", "#C77CFF", "#00BFC4"))



ggplot(data=glmm.est.5.ggfx.df, aes(x=x, y=predicted, fill=group))+
  geom_errorbar(aes(ymax=conf.high, ymin=conf.low), colour="black", width=0, position=position_dodge(width=0.4))+
  geom_point(position=position_dodge(width=0.4), pch=21, size=4, stroke=0.8)+
  theme_classic()+
  scale_fill_manual(values=c("coral3", "orange", "cyan2", "cornflowerblue"), labels=c(expression("S"[s]), expression("SO"[s]), expression("AO"[s]), expression("A"[s])))+
  labs(y="predicted establishment success", x="g.region", fill="Source region")+
  scale_x_discrete(labels=c("S.g" = expression("S"[g]),"SO.g" = expression("SO"[g]),"AO.g"= expression("AO"[g]),"A.g"= expression("A"[g])))
  
ggplot(data=foo.1, aes(y=estimates, x=g.region, fill=s.region))+
  geom_errorbar(aes(ymax=estimates+std.err, ymin=estimates-std.err), colour="black", width=0, position=position_dodge(width=0.4))+
  geom_point(position=position_dodge(width=0.4), pch=21, size=4, stroke=0.8)+
  theme_classic()+
  labs(y="Estimated lifetime fitness", x="Garden region", fill="Source region")+
  scale_fill_manual(values=c("coral3", "orange", "cyan2", "cornflowerblue"), labels=c(expression("S"[s]), expression("SO"[s]), expression("AO"[s]), expression("A"[s])))+
  scale_x_discrete(labels=c("S.g" = expression("S"[g]),"SO.g" = expression("SO"[g]),"AO.g"= expression("AO"[g]),"A.g"= expression("A"[g])))


