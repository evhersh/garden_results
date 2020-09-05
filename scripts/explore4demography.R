# Apo pops = L06, L17, C27
# Sex pops = B42, B46, L11, L12
# years go 2014 (year 0), 2015, 2016, 2017, 2018, 2019

years = c("2014", "2015", "2016", "2017", "2018", "2019")

# survival 
surv.pops <- H.dat %>%
  group_by(pop, g.region, year) %>%
  summarize(sum.surv = sum(surv))

gg.surv.B42 <- ggplot(data=filter(surv.pops, pop=="B42" & g.region=="S.g"), aes(x=factor(year), y=sum.surv, group=1))+
  geom_point()+
  geom_line(color="red")+
  scale_x_discrete(breaks=c("0", "1", "2", "3", "4", "5"), labels=years)+
  labs(x="", y="")

gg.surv.B46 <- ggplot(data=filter(surv.pops, pop=="B46" & g.region=="S.g"), aes(x=factor(year), y=sum.surv, group=1))+
  geom_point()+
  geom_line(color="red")+
  scale_x_discrete(breaks=c("0", "1", "2", "3", "4", "5"), labels=years)+
  labs(x="", y="")

gg.surv.L11 <- ggplot(data=filter(surv.pops, pop=="L11" & g.region=="AO.g"), aes(x=factor(year), y=sum.surv, group=1))+
  geom_point()+
  geom_line(color="red")+
  scale_x_discrete(breaks=c("0", "1", "2", "3", "4", "5"), labels=years)+
  labs(x="", y="number surviving in gardens")

gg.surv.L12 <- ggplot(data=filter(surv.pops, pop=="L12" & g.region=="AO.g"), aes(x=factor(year), y=sum.surv, group=1))+
  geom_point()+
  geom_line(color="red")+
  scale_x_discrete(breaks=c("0", "1", "2", "3", "4", "5"), labels=years)+
  labs(x="", y="number surviving in gardens")

gg.surv.L06 <- ggplot(data=filter(surv.pops, pop=="L06" & g.region=="AO.g"), aes(x=factor(year), y=sum.surv, group=1))+
  geom_point()+
  geom_line(color="blue")+
  scale_x_discrete(breaks=c("0", "1", "2", "3", "4", "5"), labels=years)+
  labs(x="", y="")

gg.surv.L17 <- ggplot(data=filter(surv.pops, pop=="L17" & g.region=="AO.g"), aes(x=factor(year), y=sum.surv, group=1))+
  geom_point()+
  geom_line(color="blue")+
  scale_x_discrete(breaks=c("0", "1", "2", "3", "4", "5"), labels=years)+
  labs(x="year", y="")

gg.surv.C27 <- ggplot(data=filter(surv.pops, pop=="C27" & g.region=="A.g"), aes(x=factor(year), y=sum.surv, group=1))+
  geom_point()+
  geom_line(color="blue")+
  scale_x_discrete(breaks=c("0", "1", "2", "3", "4", "5"), labels=years)+
  labs(x="year", y="")

png("survival_pops", height=8, width=11, res=300, units="in")
ggarrange(gg.surv.B42, gg.surv.B46, gg.surv.L11, gg.surv.L12, gg.surv.L06, gg.surv.L17, gg.surv.C27, ncol=2, nrow=4, labels=c("B42", "B46", "L11", "L12", "L06", "L17", "C27"), hjust=-10, vjust=3, font.label=list(size=12))
dev.off()


gg.surv.means.ms.all <- ggplot(data=(surv.means.ms.all), aes(x=year, y=surv.mean, colour=ms, group=ms))+
  geom_point(aes(fill=ms), colour="black", pch=21, size=3, position=position_dodge(width=0.2))+
  geom_errorbar(data=surv.means.ms.all, aes(y=surv.mean, ymin=surv.mean-surv.se, ymax=surv.mean+surv.se), colour="black", position=position_dodge(width=0.2), width=0.2, alpha=0.5)+
  geom_line(position=position_dodge(width=0.2))+
  facet_grid(garden~.)+
  theme_bw()