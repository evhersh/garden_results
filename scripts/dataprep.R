# Data Exploration and averages

## @knitr exploration

##########
# Packages

library(MASS)
library(gdata)
library(plotrix)
library(aster)
library(lattice)
library(dplyr)
library(ggplot2)
library(knitr)

#################
# Load raw Data #
#################

aster.dat <<-read.csv("./data/garden_data_final_wide.csv", stringsAsFactors = FALSE, strip.white = TRUE, na.string = c("NA",""))
H.dat <<-read.csv("./data/garden_data_final.csv", stringsAsFactors = FALSE, strip.white = TRUE, na.string = c("NA",""))
G.dat <<-read.csv("./data/mastergermv3.csv", stringsAsFactors = FALSE, strip.white = TRUE, na.string = c("NA","")) 
S.dat <<-read.csv("./data/seed_2018.csv", stringsAsFactors = FALSE, strip.white = TRUE, na.string = c("NA",""))

#### Factors, levels, variables, etc #####

#H.dat$flower <- ifelse(H.dat$bud.num>=1, 1, 0)

#H.dat$flower[is.na(H.dat$flower)] <- 0
#H.dat$bud.num[is.na(H.dat$bud.num)] <- 0

H.dat$pop <- factor(H.dat$pop, levels=c("B53", "B42", "B46", "B49", "L11", "L12", "L06", "L16", "L17", "C86", "C85", "C27"))
H.dat$ms <- factor(H.dat$ms, levels=c("S", "A"))
H.dat$s.region <- factor(H.dat$s.region, levels=c("S.s", "SO.s", "AO.s", "A.s"))
H.dat$garden <- factor(H.dat$garden, levels=c("SS1", "SS2", "SO1", "SO2", "AO1", "AO2", "AA1", "AA2"))

G.dat$pop <- factor(G.dat$pop, levels=c("B53", "B42", "B46", "B49", "L11", "L12", "L06", "L16", "L17", "C86", "C85", "C27"))
G.dat$ms <- factor(G.dat$ms, levels=c("S", "A"))
G.dat$s.region <- factor(G.dat$s.region, levels=c("S.s", "SO.s", "AO.s", "A.s"))
G.dat$garden <- factor(G.dat$garden, levels=c("SS1", "SS2", "SO1", "SO2", "AO1", "AO2", "AA1", "AA2"))

save(aster.dat, H.dat, file="aster_example2_Hersh.RData")

AA1 <- subset(H.dat, garden=="AA1")

# subset seed data
S.dat.nat <- subset(S.dat, source=="natural")

# subset into years
H.dat.y1 <- subset(H.dat, year==1)
H.dat.y2 <- subset(H.dat, year==2)
H.dat.y3 <- subset(H.dat, year==3)
H.dat.y4 <- subset(H.dat, year==4)
H.dat.y5 <- subset(H.dat, year==5)

#subset into gardens
#y1
H.dat.y1.AA1 <- subset(H.dat.y1, garden=="AA1")
H.dat.y1.AA2 <- subset(H.dat.y1, garden=="AA2")
H.dat.y1.AO1 <- subset(H.dat.y1, garden=="AO1")
H.dat.y1.AO2 <- subset(H.dat.y1, garden=="AO2")
H.dat.y1.SO1 <- subset(H.dat.y1, garden=="SO1")
H.dat.y1.SO2 <- subset(H.dat.y1, garden=="SO2")
H.dat.y1.SS1 <- subset(H.dat.y1, garden=="SS1")
H.dat.y1.SS2 <- subset(H.dat.y1, garden=="SS2")
#y2
H.dat.y2.AA1 <- subset(H.dat.y2, garden=="AA1")
H.dat.y2.AA2 <- subset(H.dat.y2, garden=="AA2")
H.dat.y2.AO1 <- subset(H.dat.y2, garden=="AO1")
H.dat.y2.AO2 <- subset(H.dat.y2, garden=="AO2")
H.dat.y2.SO1 <- subset(H.dat.y2, garden=="SO1")
H.dat.y2.SO2 <- subset(H.dat.y2, garden=="SO2")
H.dat.y2.SS1 <- subset(H.dat.y2, garden=="SS1")
H.dat.y2.SS2 <- subset(H.dat.y2, garden=="SS2")
#y3
H.dat.y3.AA1 <- subset(H.dat.y3, garden=="AA1")
H.dat.y3.AA2 <- subset(H.dat.y3, garden=="AA2")
H.dat.y3.AO1 <- subset(H.dat.y3, garden=="AO1")
H.dat.y3.AO2 <- subset(H.dat.y3, garden=="AO2")
H.dat.y3.SO1 <- subset(H.dat.y3, garden=="SO1")
H.dat.y3.SO2 <- subset(H.dat.y3, garden=="SO2")
H.dat.y3.SS1 <- subset(H.dat.y3, garden=="SS1")
H.dat.y3.SS2 <- subset(H.dat.y3, garden=="SS2")
#y4
H.dat.y4.AA1 <- subset(H.dat.y4, garden=="AA1")
H.dat.y4.AA2 <- subset(H.dat.y4, garden=="AA2")
H.dat.y4.AO1 <- subset(H.dat.y4, garden=="AO1")
H.dat.y4.AO2 <- subset(H.dat.y4, garden=="AO2")
H.dat.y4.SO1 <- subset(H.dat.y4, garden=="SO1")
H.dat.y4.SO2 <- subset(H.dat.y4, garden=="SO2")
H.dat.y4.SS1 <- subset(H.dat.y4, garden=="SS1")
H.dat.y4.SS2 <- subset(H.dat.y4, garden=="SS2")
#y5
H.dat.y5.AA1 <- subset(H.dat.y5, garden=="AA1")
H.dat.y5.AA2 <- subset(H.dat.y5, garden=="AA2")
H.dat.y5.AO1 <- subset(H.dat.y5, garden=="AO1")
H.dat.y5.AO2 <- subset(H.dat.y5, garden=="AO2")
H.dat.y5.SO1 <- subset(H.dat.y5, garden=="SO1")
H.dat.y5.SO2 <- subset(H.dat.y5, garden=="SO2")
H.dat.y5.SS1 <- subset(H.dat.y5, garden=="SS1")
H.dat.y5.SS2 <- subset(H.dat.y5, garden=="SS2")

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

budsum.pop.all <- H.dat %>%
  group_by(s.region,pop,garden,year) %>%
  filter(surv>0, year>0, flower>0) %>%
  summarize(bud.sum = sum(bud.num))

num.planted <- H.dat %>%
  group_by(ms, garden) %>%
  filter(year==0) %>%
  summarize(n=n())

num.planted.pop <- H.dat %>%
  group_by(pop, ms, garden) %>%
  filter(year==0) %>%
  summarize(n=n())

budsum.ms <- H.dat %>%
  group_by(ms, garden) %>%
  summarize(bud.sum = sum(bud.num, na.rm=TRUE))

budsum.pop <- H.dat %>%
  group_by(pop, ms, garden) %>%
  summarize(bud.sum = sum(bud.num, na.rm=TRUE))

buds.per.planted <- data.frame(budsum.ms[,1:3], num.planted[,3])
buds.per.planted$bpp <- buds.per.planted$bud.sum/buds.per.planted$n

buds.per.planted.pop <- data.frame(budsum.pop[,1:4], num.planted.pop[,4])
buds.per.planted.pop$bpp <- buds.per.planted.pop$bud.sum/buds.per.planted.pop$n

# SEEDS #
S.dat.nat$gspb <- S.dat.nat$good.ratio * S.dat.nat$seed.per.bud

seeds.ms <- S.dat.nat %>%
  group_by(ms) %>%
  summarize(mean.gspb = mean(gspb), se.gspb = std.error(gspb))

# ESTABLISHMENT #
est.ms <- G.dat %>%
  group_by(ms, garden) %>%
  summarize(mean.est = mean(germ.10, na.rm=TRUE), se.est = std.error(germ.10))

est.pop <- G.dat %>%
  group_by(pop, ms, garden) %>%
  summarize(mean.est = mean(germ.10, na.rm=TRUE), se.est = std.error(germ.10))

# establishment success * buds per planted * good seeds per bud (from source pops)
ebpp.ms <- data.frame(buds.per.planted, est.ms[,3])
ebpps.ms <- merge(ebpp.ms, seeds.ms, by="ms")
ebpps.ms$ebpps <- ebpps.ms$bpp * ebpps.ms$mean.est * ebpps.ms$mean.gspb
  

ebpp.pop <- data.frame(buds.per.planted.pop, est.pop[,4])  
ebpp.pop$ebpp <- ebpp.pop$bpp * ebpp.pop$mean.est



##### Plots #####

# establishment
gg.est.ms <- ggplot(data=est.ms, aes(x=garden, y=mean.est, group=ms))+
  geom_point(aes(fill=ms), colour="black", pch=21, size=3, position=position_dodge(width=0.2))+
  geom_errorbar(data=est.ms, aes(y=mean.est, ymin=mean.est-se.est, ymax=mean.est+se.est), colour="black", position=position_dodge(width=0.2), width=0.2, alpha=0.5)+
  theme_bw()+
  labs(y= "Mean establishment success w/ SE", x="Garden")

gg.est.pop <- ggplot(data=est.pop, aes(x=pop, y=mean.est, group=ms))+
  geom_point(aes(fill=ms), colour="black", pch=21, size=3, position=position_dodge(width=0.2))+
  geom_errorbar(data=est.pop, aes(y=mean.est, ymin=mean.est-se.est, ymax=mean.est+se.est), colour="black", position=position_dodge(width=0.2), width=0.2, alpha=0.5)+
  theme_bw()+
  facet_grid(garden~.)+
  labs(y= "Mean establishment success w/ SE", x="Population")

# survival
gg.surv.means.ms.all <- ggplot(data=(surv.means.ms.all), aes(x=year, y=surv.mean, colour=ms, group=ms))+
  geom_point(aes(fill=ms), colour="black", pch=21, size=3, position=position_dodge(width=0.2))+
  geom_errorbar(data=surv.means.ms.all, aes(y=surv.mean, ymin=surv.mean-surv.se, ymax=surv.mean+surv.se), colour="black", position=position_dodge(width=0.2), width=0.2, alpha=0.5)+
  geom_line(position=position_dodge(width=0.2))+
  facet_grid(garden~.)+
  theme_bw()

# leaf length
gg.length.means.ms.all <- ggplot(data=(length.means.ms.all), aes(x=year, y=length.mean, colour=ms, group=ms))+
  geom_point(aes(fill=ms), colour="black", pch=21, size=3, position=position_dodge(width=0.2))+
  geom_errorbar(data=length.means.ms.all, aes(y=length.mean, ymin=length.mean-length.se, ymax=length.mean+length.se), colour="black", position=position_dodge(width=0.2), width=0.2, alpha=0.5)+
  geom_line(position=position_dodge(width=0.2))+
  facet_grid(garden~.)+
  theme_bw()

# leaf number
gg.num.means.ms.all <- ggplot(data=(num.means.ms.all), aes(x=year, y=num.mean, colour=ms, group=ms))+
  geom_point(aes(fill=ms), colour="black", pch=21, size=3, position=position_dodge(width=0.2))+
  geom_errorbar(data=num.means.ms.all, aes(y=num.mean, ymin=num.mean-num.se, ymax=num.mean+num.se), colour="black", position=position_dodge(width=0.2), width=0.2, alpha=0.5)+
  geom_line(position=position_dodge(width=0.2))+
  facet_grid(garden~.)+
  theme_bw()

#flowers
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

gg.budsum.pop.all <- ggplot(data=(budsum.pop.all), aes(x=year, y=bud.sum, colour=s.region, group=pop))+
  geom_point(aes(fill=s.region), colour="black", pch=21, size=3, position=position_dodge(width=0.1))+
  geom_line(position=position_dodge(width=0.1))+
  facet_grid(garden~.)+
  theme_bw()

# bpp
gg.bpp <- ggplot(data=buds.per.planted, aes(x=garden, y=bpp, colour=ms, group=ms))+
  geom_bar(aes(fill=ms), colour="black", stat="identity", size=1, position="dodge")+
  theme_bw()+
  labs(y = "buds per individual planted", x = "Garden")

gg.bpp.pop <- ggplot(data=buds.per.planted.pop, aes(x=pop, y=bpp, group=ms))+
  geom_bar(aes(fill=ms), stat="identity", colour="black", size=1)+
  theme_bw()+
  labs(y = "buds per individual planted", x = "population")+
  facet_grid(garden~.)

gg.ebpp.ms <- ggplot(data=ebpp.ms, aes(x=garden, y=ebpp, colour=ms, group=ms))+
  geom_bar(aes(fill=ms), colour="black", stat="identity", size=1, position="dodge")+
  theme_bw()+
  labs(y = "EBPP", x = "Garden")

gg.ebpps.ms <- ggplot(data=ebpps.ms, aes(x=garden, y=ebpps, colour=ms, group=ms))+
  geom_bar(aes(fill=ms), colour="black", stat="identity", size=1, position="dodge")+
  theme_bw()+
  labs(y = "EBPPS", x = "Garden")

gg.ebpp.pop <- ggplot(data=ebpp.pop, aes(x=pop, y=ebpp, colour=ms, group=ms))+
  geom_bar(aes(fill=ms), colour="black", stat="identity", size=1, position="dodge")+
  theme_bw()+
  labs(y = "EBPP", x = "population")+
  facet_grid(garden~.)
