library(tidyverse)

C.dat <- C.dat %>%
  dplyr::rename(site = X.ID1, type = ID2)

cdat.2004 <- C.dat %>%
  filter(Year < 2005)

cdat.2012 <- C.dat %>%
  filter(Year < 2013, type=="garden")

cdat.exp <- C.dat %>%
  filter(Year > 2013)

cdat.exp.means <- C.dat %>%
  filter(Year > 2013) %>%
  group_by(type, site) %>%
  summarize(meanMAT = mean(MAT), meanMAP = mean(MAP))

gg.gardenclimeMAT <- ggplot()+
  geom_violin(data=subset(cdat.2012, type=="garden"), aes(y=MAT, x=factor(site)))+
  geom_point(data=subset(cdat.exp, type=="garden"), aes(y=MAT, x=factor(site), color=factor(Year)))+
  coord_flip()
  
gg.gardenclimeMAP <- ggplot()+
  geom_violin(data=subset(cdat.2012, type=="garden"), aes(y=log(MAP), x=factor(site)))+
  geom_point(data=subset(cdat.exp, type=="garden"), aes(y=log(MAP), x=factor(site), color=factor(Year)))+
  coord_flip()

ggplot()+
  geom_violin(data=subset(cdat.2004, site=="B53"), aes(x=0, y=MAT))+
  geom_point(data=subset(cdat.exp.means, type=="garden"), aes(x=0, y=meanMAT, color=site))

ggplot()+
  geom_violin(data=subset(cdat.2004, site=="B42"), aes(x=0, y=MAT))+
  geom_point(data=subset(cdat.exp.means, type=="garden"), aes(x=0, y=meanMAT, color=site))

ggplot()+
  geom_violin(data=subset(cdat.2004, site=="B46"), aes(x=0, y=MAT))+
  geom_point(data=subset(cdat.exp.means, type=="garden"), aes(x=0, y=meanMAT, color=site))

ggplot()+
  geom_violin(data=subset(cdat.2004, site=="B49"), aes(x=0, y=MAT))+
  geom_point(data=subset(cdat.exp.means, type=="garden"), aes(x=0, y=meanMAT, color=site))

ggplot()+
  geom_violin(data=subset(cdat.2004, site=="L11"), aes(x=0, y=MAT))+
  geom_point(data=subset(cdat.exp.means, type=="garden"), aes(x=0, y=meanMAT, color=site))

ggplot()+
  geom_violin(data=subset(cdat.2004, site=="L12"), aes(x=0, y=MAT))+
  geom_point(data=subset(cdat.exp.means, type=="garden"), aes(x=0, y=meanMAT, color=site))

ggplot()+
  geom_violin(data=subset(cdat.2004, site=="L06"), aes(x=0, y=MAT))+
  geom_point(data=subset(cdat.exp.means, type=="garden"), aes(x=0, y=meanMAT, color=site))

ggplot()+
  geom_violin(data=subset(cdat.2004, site=="L16"), aes(x=0, y=MAT))+
  geom_point(data=subset(cdat.exp.means, type=="garden"), aes(x=0, y=meanMAT, color=site))

ggplot()+
  geom_violin(data=subset(cdat.2004, site=="L17"), aes(x=0, y=MAT))+
  geom_point(data=subset(cdat.exp.means, type=="garden"), aes(x=0, y=meanMAT, color=site))

ggplot()+
  geom_violin(data=subset(cdat.2004, site=="C86"), aes(x=0, y=MAT))+
  geom_point(data=subset(cdat.exp.means, type=="garden"), aes(x=0, y=meanMAT, color=site))

ggplot()+
  geom_violin(data=subset(cdat.2004, site=="C85"), aes(x=0, y=MAT))+
  geom_point(data=subset(cdat.exp.means, type=="garden"), aes(x=0, y=meanMAT, color=site))

ggplot()+
  geom_violin(data=subset(cdat.2004, site=="C27"), aes(x=0, y=MAT))+
  geom_point(data=subset(cdat.exp.means, type=="garden"), aes(x=0, y=meanMAT, color=site))


