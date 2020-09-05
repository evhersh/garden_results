library(tidyverse)

# percent cover of gardens
ggplot(data=aster.dat)+
  geom_boxplot(aes(x=garden, y=percent.cover), fill="grey90")+
  labs(x="Garden site", y="% vegetation cover")+
  theme_classic()

cover.lmer <- lmer(percent.cover ~ g.region + (1|garden), data=aster.dat)
cover.lmer2 <- lmer(percent.cover ~ (1|garden), data=aster.dat)
anova(cover.lmer, cover.lmer2)

summary(cover.lmer)
cover.ggfx <- ggpredict(cover.lmer, terms="g.region")
plot(cover.ggfx)
cover.ggfx.df <- as.data.frame(cover.ggfx)


# percent cover of demography plots
ggplot(data=subset(D.dat))+
  geom_boxplot(aes(x=Site, y=percent.cover))

surv.cover <- aster.dat %>%
  group_by(ms, mom) %>%
  summarize(mean.surv = mean(surv.2015), mean.cover=mean(percent.cover))

# survival (2015) by cover
ggplot(data=surv.cover)+
  geom_point(aes(x=mean.cover, y=mean.surv, color=ms))+
  geom_smooth(method='lm', aes(x=mean.cover, y=mean.surv, color=ms))

# volume
V.dat <- aster.dat%>%
  filter(!is.na(RTSratio), !is.infinite(RTSratio))



ggplot(data=V.dat) +
  geom_boxplot(aes(x=ms, y=RTSratio), outlier.shape = NA, width=0.5)+
  geom_jitter(aes(x=ms, y=RTSratio, fill=ms), pch=21, width=0.1)+
  scale_fill_manual(values=c("coral3", "cornflowerblue"))+
  theme_classic()


lmer.rts <- lmer(RTSratio~ms+(1|pop), data=V.dat)
lmer.rts2 <- lmer(RTSratio~(1|pop), data=V.dat)
anova(lmer.rts, lmer.rts2)

rts.ggfx <- ggpredict(lmer.rts, terms="ms")
rts.ggfx.df <- as.data.frame(rts.ggfx)
plot(rts.ggfx)

# root to shoot

##### seed #####
S.dat
S.dat.garden
S.dat.nat

lmer.gseed.nat <- lmer(good.ratio~ms+(1|pop), data=S.dat.nat)
lmer.gseed.nat2 <- lmer(good.ratio~(1|pop), data=S.dat.nat)
anova(lmer.gseed.nat, lmer.gseed.nat2)
gseed.nat.ggfx <- ggpredict(lmer.gseed.nat, terms="ms")
plot(gseed.nat.ggfx)
gseed.nat.ggfx.df <- as.data.frame(gseed.nat.ggfx)

lmer.gseed.garden.msX <- lmer(good.ratio~ms*g.region+(1|pop)+(1|garden), data=S.dat.garden)
lmer.gseed.garden.msX.ggfx <- ggpredict(lmer.gseed.garden.msX, terms=c("g.region", "ms"))
plot(lmer.gseed.garden.msX.ggfx)

lmer.gseed.garden <- lmer(good.ratio~ms+(1|pop), data=S.dat.garden)
lmer.gseed.garden2 <- lmer(good.ratio~(1|pop), data=S.dat.garden)
anova(lmer.gseed.garden, lmer.gseed.garden2)
gseed.garden.ggfx <- ggpredict(lmer.gseed.garden, terms="ms")
plot(gseed.garden.ggfx)
gseed.garden.ggfx.df <- as.data.frame(gseed.garden.ggfx)

lmer.seed.nat <- lmer(seed.per.bud~ms+(1|pop), data=S.dat.nat)
lmer.seed.nat2 <- lmer(seed.per.bud~(1|pop), data=S.dat.nat)
anova(lmer.seed.nat, lmer.seed.nat2)
seed.nat.ggfx <- ggpredict(lmer.seed.nat, terms = "ms")
plot(seed.nat.ggfx)
seed.nat.ggfx.df <- as.data.frame(seed.nat.ggfx)

lmer.seed.garden <- lmer(seed.per.bud~ms+(1|pop), data=S.dat.garden)
lmer.seed.garden2 <- lmer(seed.per.bud~(1|pop), data=S.dat.garden)
anova(lmer.seed.garden, lmer.seed.garden2)
seed.garden.ggfx <- ggpredict(lmer.seed.garden, terms="ms")
plot(seed.garden.ggfx)
seed.garden.ggfx.df <- as.data.frame(seed.garden.ggfx.df)

# garden - seed per bud (apos higher?)
ggplot(data=S.dat.garden)+
  geom_boxplot(aes(x=ms, y=seed.per.bud))

# garden - good seed (apos higher?)
ggplot(data=S.dat.garden)+
  geom_boxplot(aes(x=ms, y=good.seed))

# garden - ratio of good seed (apos higher)
ggplot(data=S.dat.garden)+
  geom_boxplot(aes(x=ms, y=good.ratio))+
  geom_jitter(aes(x=ms, y=good.ratio), width=0.1)

# nat - seed per bud (no diff)
ggplot(data=S.dat.nat)+
  geom_boxplot(aes(x=ms, y=seed.per.bud))

ggplot(data=S.dat.nat)+
  geom_boxplot(aes(x=ms, y=good.ratio))

ggplot(data=S.dat.nat)+
  geom_boxplot(aes(x=ms, y=good.ratio))


####### summary stats ########

aster.dat$flower.all <- aster.dat$flower.2014 + aster.dat$flower.2015 + aster.dat$flower.2016 + aster.dat$flower.2017 + aster.dat$flower.2018 + aster.dat$flower.2019
aster.dat$flower.10 <- ifelse(aster.dat$flower.all >=1, 1, 0)


garden_summary <- aster.dat %>%
  group_by(g.region, s.region) %>%
  summarize(n=n(), surv=sum(surv.2019), buds = sum(budnum.2014, budnum.2015, budnum.2016, budnum.2017, budnum.2018, budnum.2019, na.rm = TRUE), flowering.inds=sum(flower.10))
  
est_summary <- G.dat %>%
  group_by(g.region, s.region) %>%
  drop_na() %>%
  summarize(n=n(), germ=mean(germ.10, na.rm=TRUE))
