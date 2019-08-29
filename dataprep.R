# Data Exploration and averages

##########
# Packages

library(MASS)
library(gdata)
library(plotrix)
library(aster)
library(lattice)
library(dplyr)
library(ggplot2)

#################
# Load raw Data #
#################

aster.dat <<-read.csv("garden_data_final_wide.csv", stringsAsFactors = FALSE, strip.white = TRUE, na.string = c("NA",""))
H.dat <<-read.csv("garden_data_final.csv", stringsAsFactors = FALSE, strip.white = TRUE, na.string = c("NA",""))


#### Factors, levels, variables, etc #####

H.dat$flower <- ifelse(H.dat$bud.num>=1, 1, 0)

H.dat$flower[is.na(H.dat$flower)] <- 0
H.dat$bud.num[is.na(H.dat$bud.num)] <- 0

H.dat$pop <- factor(H.dat$pop, levels=c("B53", "B42", "B46", "B49", "L11", "L12", "L06", "L16", "L17", "C86", "C85", "C27"))
H.dat$ms <- factor(H.dat$ms, levels=c("S", "A"))

save(aster.dat, H.dat, file="aster_example2_Hersh.RData")

AA1 <- subset(H.dat, garden=="AA1")


###############
# exploration #
###############
# cor.test(H.dat$surv.2015, H.dat$surv.2019)
# cor.test(H.dat$surv.2019, H.dat$flower.2019)
# H.dat[which(H.dat$flower.2019 == 0 & H.dat$surv.2019 == 1), c("garden","flower.2019","surv.2019","budnum.2019")]


##### summaries #####
surv.means.ms.all <- H.dat %>%
  group_by(ms, garden, year) %>%
  filter(year>0)%>%
  summarize(surv.mean = mean(surv), surv.se=std.error(surv))

length.means.ms.all <- H.dat %>%
  group_by(ms, garden, year) %>%
  filter(year>0)%>%
  summarize(length.mean = mean(leaf.length, na.rm=TRUE), length.se=std.error(leaf.length))

num.means.ms.all <- H.dat %>%
  group_by(ms, garden, year) %>%
  filter(year>0)%>%
  summarize(num.mean = mean(leaf.num, na.rm=TRUE), num.se=std.error(leaf.num))

budsum.ms.all <- H.dat %>%
  group_by(ms, garden, year) %>%
  filter(bud.num>0, year>0) %>%
  summarize(bud.sum = sum(bud.num))


flowers.ms.all <- H.dat %>%
  group_by(ms, garden, year) %>%
  filter(flower>0, year>0) %>%
  summarize(num.flowering = sum(flower))

mean.flower.ms.all <- H.dat %>%
  group_by(ms,garden,year) %>%
  filter(surv>0, year>0) %>%
  summarize(mean.flowering=mean(flower))

mean.budnum.ms.all <- H.dat %>%
  group_by(ms,garden,year) %>%
  filter(surv>0, year>0, flower>0) %>%
  summarize(mean.budnum=mean(bud.num))

surv.pop <- H.dat %>%
  group_by(pop, garden, year) %>%
  filter(year>0)%>%
  summarize(num.surv = sum(surv))

flowers.pop <- H.dat %>%
  group_by(ms, garden, year) %>%
  filter(year>0) %>%
  summarize(num.flowering = sum(flower))

##### Plots #####
gg.surv.means.ms.all <- ggplot(data=(surv.means.ms.all), aes(x=year, y=surv.mean, colour=ms, group=ms))+
  geom_point(aes(fill=ms), colour="black", pch=21, size=3, position=position_dodge(width=0.2))+
  geom_errorbar(data=surv.means.ms.all, aes(y=surv.mean, ymin=surv.mean-surv.se, ymax=surv.mean+surv.se), colour="black", position=position_dodge(width=0.2), width=0.2, alpha=0.5)+
  geom_line(position=position_dodge(width=0.2))+
  facet_grid(garden~.)+
  theme_bw()

gg.length.means.ms.all <- ggplot(data=(length.means.ms.all), aes(x=year, y=length.mean, colour=ms, group=ms))+
  geom_point(aes(fill=ms), colour="black", pch=21, size=3, position=position_dodge(width=0.2))+
  geom_errorbar(data=length.means.ms.all, aes(y=length.mean, ymin=length.mean-length.se, ymax=length.mean+length.se), colour="black", position=position_dodge(width=0.2), width=0.2, alpha=0.5)+
  geom_line(position=position_dodge(width=0.2))+
  facet_grid(garden~.)+
  theme_bw()

gg.num.means.ms.all <- ggplot(data=(num.means.ms.all), aes(x=year, y=num.mean, colour=ms, group=ms))+
  geom_point(aes(fill=ms), colour="black", pch=21, size=3, position=position_dodge(width=0.2))+
  geom_errorbar(data=num.means.ms.all, aes(y=num.mean, ymin=num.mean-num.se, ymax=num.mean+num.se), colour="black", position=position_dodge(width=0.2), width=0.2, alpha=0.5)+
  geom_line(position=position_dodge(width=0.2))+
  facet_grid(garden~.)+
  theme_bw()

gg.numflower.ms.all <- ggplot(data=(flowers.ms.all), aes(x=year, y=num.flowering, colour=ms, group=ms))+
  geom_point(aes(fill=ms), colour="black", pch=21, size=3, position=position_dodge(width=0.1))+
  geom_line(position=position_dodge(width=0.1))+
  facet_grid(garden~.)+
  theme_bw()

gg.budsum.ms.all <- ggplot(data=(budsum.ms.all), aes(x=year, y=bud.sum, colour=ms, group=ms))+
  geom_point(aes(fill=ms), colour="black", pch=21, size=3, position=position_dodge(width=0.1))+
  geom_line(position=position_dodge(width=0.1))+
  facet_grid(garden~.)+
  theme_bw()

gg.meanflower.ms.all <- ggplot(data=(mean.flower.ms.all), aes(x=year, y=mean.flowering, colour=ms, group=ms))+
  geom_point(aes(fill=ms), colour="black", pch=21, size=3, position=position_dodge(width=0.1))+
  geom_line(position=position_dodge(width=0.1))+
  facet_grid(garden~.)+
  theme_bw()
