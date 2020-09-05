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
library(lmerTest)
library(ggpubr)

#******* ESTABLISHMENT *****#

gg.est <- ggplot(data=glmm.est.5.ggfx.df, aes(x=x, y=predicted, fill=group))+
              geom_line(aes(group=group), linetype="dotted", position=position_dodge(width=0.4))+
              geom_errorbar(aes(ymax=conf.high, ymin=conf.low), colour="black", width=0, position=position_dodge(width=0.4))+
              geom_point(position=position_dodge(width=0.4), pch=21, size=2, stroke=0.8)+
              theme_bw()+
              scale_fill_manual(values=c("coral3", "orange", "cyan2", "cornflowerblue"), labels=c(expression("S"[s]), expression("SO"[s]), expression("AO"[s]), expression("A"[s])))+
              labs(y="Predicted establishment success", x="Garden region", fill="Source region")+
              scale_x_discrete(labels=c("S.g" = expression("S"[g]),"SO.g" = expression("SO"[g]),"AO.g"= expression("AO"[g]),"A.g"= expression("A"[g])))

png("est.png", height=6, width=9, res=300, units="in")
gg.est
dev.off()

#******* ASTER MODEL *******#

gg.aster <- ggplot(data=foo.1, aes(y=estimates, x=g.region, fill=s.region))+
                geom_line(aes(group=s.region), linetype="dotted", position=position_dodge(width=0.4))+
                geom_errorbar(aes(ymax=estimates+std.err, ymin=estimates-std.err), colour="black", width=0, position=position_dodge(width=0.4))+
                geom_point(position=position_dodge(width=0.4), pch=21, size=3, stroke=0.8)+
                theme_bw()+
                labs(y="Predicted lifetime bud production", x="Garden region", fill="Source region")+
                scale_fill_manual(values=c("coral3", "orange", "cyan2", "cornflowerblue"), labels=c(expression("S"[s]), expression("SO"[s]), expression("AO"[s]), expression("A"[s])))+
                scale_x_discrete(labels=c("S.g" = expression("S"[g]),"SO.g" = expression("SO"[g]),"AO.g"= expression("AO"[g]),"A.g"= expression("A"[g])))

png("aster.png", height=6, width=9, res=300, units="in")
gg.aster
dev.off()

png("garden.png", height=5, width=10, res=300, units="in")
ggarrange(gg.est, gg.aster, nrow=1, ncol=2, common.legend = TRUE, legend="bottom", hjust=-4.5, vjust=2, labels=c("A", "B"))
dev.off()

#******* BOOTSTRAPPING *****#

gg.ebpp.boot <- ggplot(data=(ebpp.ci), aes(x=g.region, y=mean.ebpp, colour=s.region, group=s.region))+
                    geom_line(aes(group=s.region), linetype="dotted", position=position_dodge(width=0.2), colour="black")+
                    geom_errorbar(data=ebpp.ci, aes(ymin=lower.ci, ymax=upper.ci), colour="black", position=position_dodge(width=0.2), width=0)+
                    geom_point(aes(fill=s.region), colour="black", pch=21, size=3, position=position_dodge(width=0.2))+
                    theme_bw()+
                    labs(y = "Establishment x Buds produced per individual", x = "Garden region", fill="Source region")+
                    scale_fill_manual(values=c("coral3", "orange", "cyan2", "cornflowerblue"), labels=c(expression("S"[s]), expression("SO"[s]), expression("AO"[s]), expression("A"[s])))+
                    scale_x_discrete(labels=c("S.g" = expression("S"[g]),"SO.g" = expression("SO"[g]),"AO.g"= expression("AO"[g]),"A.g"= expression("A"[g])))

png("boot.png", height=6, width=9, res=300, units="in")
gg.ebpp.boot
dev.off()

#******* LEAF LENGTH *******#
# lmer.length.y1.ggfxdf

gg.length.y1  <- ggplot(data=lmer.length.y1.ggfxdf, aes(x=x, y=predicted, fill=group))+
                    geom_errorbar(aes(ymax=conf.high, ymin=conf.low), colour="black", width=0, position=position_dodge(width=0.4))+
                    geom_point(position=position_dodge(width=0.4), pch=21, size=3, stroke=0.8)+
                    theme_bw()+
                    labs(y="Predicted leaf length", x="", fill="Source region")+
                    scale_fill_manual(values=c("coral3", "orange", "cyan2", "cornflowerblue"), labels=c(expression("S"[s]), expression("SO"[s]), expression("AO"[s]), expression("A"[s])))+
                    scale_x_discrete(labels=c("S.g" = expression("S"[g]),"SO.g" = expression("SO"[g]),"AO.g"= expression("AO"[g]),"A.g"= expression("A"[g])))

gg.length.y2  <- ggplot(data=lmer.length.y2.ggfxdf, aes(x=x, y=predicted, fill=group))+
                    geom_errorbar(aes(ymax=conf.high, ymin=conf.low), colour="black", width=0, position=position_dodge(width=0.4))+
                    geom_point(position=position_dodge(width=0.4), pch=21, size=3, stroke=0.8)+
                    theme_bw()+
                    labs(y="", x="", fill="Source region")+
                    scale_fill_manual(values=c("coral3", "orange", "cyan2", "cornflowerblue"), labels=c(expression("S"[s]), expression("SO"[s]), expression("AO"[s]), expression("A"[s])))+
                    scale_x_discrete(labels=c("S.g" = expression("S"[g]),"SO.g" = expression("SO"[g]),"AO.g"= expression("AO"[g]),"A.g"= expression("A"[g])))

gg.length.y3  <- ggplot(data=lmer.length.y3.ggfxdf, aes(x=x, y=predicted, fill=group))+
                    geom_errorbar(aes(ymax=conf.high, ymin=conf.low), colour="black", width=0, position=position_dodge(width=0.4))+
                    geom_point(position=position_dodge(width=0.4), pch=21, size=3, stroke=0.8)+
                    theme_bw()+
                    labs(y="Predicted leaf length", x="", fill="Source region")+
                    scale_fill_manual(values=c("coral3", "orange", "cyan2", "cornflowerblue"), labels=c(expression("S"[s]), expression("SO"[s]), expression("AO"[s]), expression("A"[s])))+
                    scale_x_discrete(labels=c("S.g" = expression("S"[g]),"SO.g" = expression("SO"[g]),"AO.g"= expression("AO"[g]),"A.g"= expression("A"[g])))

gg.length.y4  <- ggplot(data=lmer.length.y4.ggfxdf, aes(x=x, y=predicted, fill=group))+
                    geom_errorbar(aes(ymax=conf.high, ymin=conf.low), colour="black", width=0, position=position_dodge(width=0.4))+
                    geom_point(position=position_dodge(width=0.4), pch=21, size=3, stroke=0.8)+
                    theme_bw()+
                    labs(y="", x="Garden region", fill="Source region")+
                    scale_fill_manual(values=c("coral3", "orange", "cyan2", "cornflowerblue"), labels=c(expression("S"[s]), expression("SO"[s]), expression("AO"[s]), expression("A"[s])))+
                    scale_x_discrete(labels=c("S.g" = expression("S"[g]),"SO.g" = expression("SO"[g]),"AO.g"= expression("AO"[g]),"A.g"= expression("A"[g])))

gg.length.y5  <- ggplot(data=lmer.length.y5.ggfxdf, aes(x=x, y=predicted, fill=group))+
                    geom_errorbar(aes(ymax=conf.high, ymin=conf.low), colour="black", width=0, position=position_dodge(width=0.4))+
                    geom_point(position=position_dodge(width=0.4), pch=21, size=3, stroke=0.8)+
                    theme_bw()+
                    labs(y="Predicted leaf length", x="Garden region", fill="Source region")+
                    scale_fill_manual(values=c("coral3", "orange", "cyan2", "cornflowerblue"), labels=c(expression("S"[s]), expression("SO"[s]), expression("AO"[s]), expression("A"[s])))+
                    scale_x_discrete(labels=c("S.g" = expression("S"[g]),"SO.g" = expression("SO"[g]),"AO.g"= expression("AO"[g]),"A.g"= expression("A"[g])))

png("length.png", height=9, width=11, res=300, units="in")
ggarrange(gg.length.y1, gg.length.y2, gg.length.y3, gg.length.y4, gg.length.y5, ncol=2, nrow=3, common.legend=TRUE, legend="bottom", labels=c("Year 1", "Year 2", "Year 3", "Year 4", "Year 5"), hjust=-5.5, vjust=2, font.label = list(size=12))
dev.off()

#******* LEAF NUMBER *******#

gg.num.y1  <- ggplot(data=glmer.num.y1.ggfxdf, aes(x=x, y=predicted, fill=group))+
  geom_errorbar(aes(ymax=conf.high, ymin=conf.low), colour="black", width=0, position=position_dodge(width=0.4))+
  geom_point(position=position_dodge(width=0.4), pch=21, size=3, stroke=0.8)+
  theme_bw()+
  labs(y="Predicted leaf number", x="", fill="Source region")+
  scale_fill_manual(values=c("coral3", "orange", "cyan2", "cornflowerblue"), labels=c(expression("S"[s]), expression("SO"[s]), expression("AO"[s]), expression("A"[s])))+
  scale_x_discrete(labels=c("S.g" = expression("S"[g]),"SO.g" = expression("SO"[g]),"AO.g"= expression("AO"[g]),"A.g"= expression("A"[g])))

gg.num.y2  <- ggplot(data=glmer.num.y2.ggfxdf, aes(x=x, y=predicted, fill=group))+
  geom_errorbar(aes(ymax=conf.high, ymin=conf.low), colour="black", width=0, position=position_dodge(width=0.4))+
  geom_point(position=position_dodge(width=0.4), pch=21, size=3, stroke=0.8)+
  theme_bw()+
  labs(y="", x="", fill="Source region")+
  scale_fill_manual(values=c("coral3", "orange", "cyan2", "cornflowerblue"), labels=c(expression("S"[s]), expression("SO"[s]), expression("AO"[s]), expression("A"[s])))+
  scale_x_discrete(labels=c("S.g" = expression("S"[g]),"SO.g" = expression("SO"[g]),"AO.g"= expression("AO"[g]),"A.g"= expression("A"[g])))

gg.num.y3  <- ggplot(data=glmer.num.y3.ggfxdf, aes(x=x, y=predicted, fill=group))+
  geom_errorbar(aes(ymax=conf.high, ymin=conf.low), colour="black", width=0, position=position_dodge(width=0.4))+
  geom_point(position=position_dodge(width=0.4), pch=21, size=3, stroke=0.8)+
  theme_bw()+
  labs(y="Predicted leaf number", x="", fill="Source region")+
  scale_fill_manual(values=c("coral3", "orange", "cyan2", "cornflowerblue"), labels=c(expression("S"[s]), expression("SO"[s]), expression("AO"[s]), expression("A"[s])))+
  scale_x_discrete(labels=c("S.g" = expression("S"[g]),"SO.g" = expression("SO"[g]),"AO.g"= expression("AO"[g]),"A.g"= expression("A"[g])))

gg.num.y4  <- ggplot(data=glmer.num.y4.ggfxdf, aes(x=x, y=predicted, fill=group))+
  geom_errorbar(aes(ymax=conf.high, ymin=conf.low), colour="black", width=0, position=position_dodge(width=0.4))+
  geom_point(position=position_dodge(width=0.4), pch=21, size=3, stroke=0.8)+
  theme_bw()+
  labs(y="", x="Garden region", fill="Source region")+
  scale_fill_manual(values=c("coral3", "orange", "cyan2", "cornflowerblue"), labels=c(expression("S"[s]), expression("SO"[s]), expression("AO"[s]), expression("A"[s])))+
  scale_x_discrete(labels=c("S.g" = expression("S"[g]),"SO.g" = expression("SO"[g]),"AO.g"= expression("AO"[g]),"A.g"= expression("A"[g])))

gg.num.y5  <- ggplot(data=glmer.num.y5.ggfxdf, aes(x=x, y=predicted, fill=group))+
  geom_errorbar(aes(ymax=conf.high, ymin=conf.low), colour="black", width=0, position=position_dodge(width=0.4))+
  geom_point(position=position_dodge(width=0.4), pch=21, size=3, stroke=0.8)+
  theme_bw()+
  labs(y="Predicted leaf number", x="Garden region", fill="Source region")+
  scale_fill_manual(values=c("coral3", "orange", "cyan2", "cornflowerblue"), labels=c(expression("S"[s]), expression("SO"[s]), expression("AO"[s]), expression("A"[s])))+
  scale_x_discrete(labels=c("S.g" = expression("S"[g]),"SO.g" = expression("SO"[g]),"AO.g"= expression("AO"[g]),"A.g"= expression("A"[g])))

png("num.png", height=9, width=11, res=300, units="in")
ggarrange(gg.num.y1, gg.num.y2, gg.num.y3, gg.num.y4, gg.num.y5, ncol=2, nrow=3, common.legend=TRUE, legend="bottom", labels=c("Year 1", "Year 2", "Year 3", "Year 4", "Year 5"), hjust=-5.5, vjust=2, font.label = list(size=12))
dev.off()

#******* RTS VOLUME ****#

gg.RTS <- ggplot(data=V.dat) +
              geom_boxplot(aes(x=ms, y=RTSratio), outlier.shape = NA, width=0.2)+
              geom_jitter(aes(x=ms, y=RTSratio, fill=ms), pch=21, width=0.05, alpha=0.8)+
              geom_errorbar(data=rts.ggfx.df, aes(ymax=conf.high, ymin=conf.low, x=as.numeric(as.factor(x))+0.4), width=0)+
              geom_point(data=rts.ggfx.df, aes(x=as.numeric(as.factor(x))+0.4, y=predicted, fill=x), shape=21, size=3)+
              scale_fill_manual(values=c("coral3", "cornflowerblue"))+
              theme_bw()+
              labs(y="Root:shoot volume", x="Mating system")+
              scale_x_discrete(labels=c("S" = "Sexual", "A" = "Apomictic"))+
              theme(legend.position = "none")

png("RTS.png", height=6, width=9, res=300, units="in")
gg.RTS
dev.off()

#******* CLIMATE *******#


gg.gardenclimeMAT <- ggplot()+
                        geom_violin(data=cdat.2012, aes(y=MAT, x=factor(site)), fill="grey90")+
                        geom_point(data=subset(cdat.exp, type=="garden"), aes(y=MAT, x=factor(site),fill=factor(Year)), pch=21)+
                        coord_flip()+
                        theme_classic()+
                        labs(y="Mean annual temperature", x="Garden site", fill="Year")+
                        scale_x_discrete(limits = rev(levels(as.factor(cdat.2012$site))))

gg.gardenclimeMAP <- ggplot()+
                        geom_violin(data=cdat.2012, aes(y=log(MAP), x=factor(site)),fill="grey90")+
                        geom_point(data=subset(cdat.exp, type=="garden"), aes(y=log(MAP), x=factor(site),fill=factor(Year)), pch=21)+
                        coord_flip()+
                        theme_classic()+
                        labs(y=expression("Mean annual precipitation"[log]), x="", fill="Year")+
                        scale_x_discrete(limits = rev(levels(as.factor(cdat.2012$site))))

png("climate.png", height=8, width=11, res=300, units="in")
ggarrange(gg.gardenclimeMAT, gg.gardenclimeMAP, ncol=2, nrow=1, common.legend=TRUE, legend="bottom", labels=c("MAT", "MAP"), hjust=-2)
dev.off()

#****** % COVER ********#

gg.cover <- ggplot(data=aster.dat)+
              geom_boxplot(aes(x=garden, y=percent.cover), fill="grey90")+
              labs(x="Garden site", y="% vegetation cover")+
              theme_classic()

#****** SEED ***********#

# garden - seed per bud (apos higher?)
gg.seed1 <- ggplot(data=S.dat.garden)+
  geom_boxplot(aes(x=ms, y=seed.per.bud), outlier.shape = NA, width=0.3)+
  geom_jitter(aes(x=ms, y=seed.per.bud, fill=ms),pch=21, width=0.05, alpha=0.9)+
  geom_errorbar(data=seed.garden.ggfx.df, aes(ymax=conf.high, ymin=conf.low, x=as.numeric(as.factor(x))+0.4), width=0)+
  geom_point(data=seed.garden.ggfx.df, aes(x=as.numeric(as.factor(x))+0.4, y=predicted, fill=x), shape=21, size=3)+
  theme_bw()+
  labs(y="Number of seeds per bud", x="")+
  scale_x_discrete(labels=c("S" = "Sexual", "A" = "Apomictic"))+
  scale_fill_manual(values=c("coral3", "cornflowerblue"))+
  theme(legend.position = "none")
  

# garden - ratio of good seed (apos higher)
gg.seed2 <- ggplot(data=S.dat.garden)+
  geom_boxplot(aes(x=ms, y=good.ratio), outlier.shape = NA, width=0.3)+
  geom_jitter(aes(x=ms, y=good.ratio, fill=ms),pch=21, width=0.05, alpha=0.9)+
  geom_errorbar(data=gseed.garden.ggfx.df, aes(ymax=conf.high, ymin=conf.low, x=as.numeric(as.factor(x))+0.4), width=0)+
  geom_point(data=gseed.garden.ggfx.df, aes(x=as.numeric(as.factor(x))+0.4, y=predicted, fill=x), shape=21, size=3)+
  theme_bw()+
  labs(y="% viable seeds", x="Mating system")+
  scale_x_discrete(labels=c("S" = "Sexual", "A" = "Apomictic"))+
  scale_fill_manual(values=c("coral3", "cornflowerblue"))+
  theme(legend.position = "none")

# nat - seed per bud (no diff)
gg.seed3 <- ggplot(data=S.dat.nat)+
  geom_boxplot(aes(x=ms, y=seed.per.bud), outlier.shape = NA, width=0.3)+
  geom_jitter(aes(x=ms, y=seed.per.bud, fill=ms),pch=21, width=0.05, alpha=0.9)+
  geom_errorbar(data=seed.nat.ggfx.df, aes(ymax=conf.high, ymin=conf.low, x=as.numeric(as.factor(x))+0.4), width=0)+
  geom_point(data=seed.nat.ggfx.df, aes(x=as.numeric(as.factor(x))+0.4, y=predicted, fill=x), shape=21, size=3)+
  theme_bw()+
  labs(y="", x="")+
  scale_x_discrete(labels=c("S" = "Sexual", "A" = "Apomictic"))+
  scale_fill_manual(values=c("coral3", "cornflowerblue"))+
  theme(legend.position = "none")

# nat - good ratio
gg.seed4 <- ggplot(data=S.dat.nat)+
  geom_boxplot(aes(x=ms, y=good.ratio), outlier.shape = NA, width=0.3)+
  geom_jitter(aes(x=ms, y=good.ratio, fill=ms),pch=21, width=0.05, alpha=0.9)+
  geom_errorbar(data=gseed.nat.ggfx.df, aes(ymax=conf.high, ymin=conf.low, x=as.numeric(as.factor(x))+0.4), width=0)+
  geom_point(data=gseed.nat.ggfx.df, aes(x=as.numeric(as.factor(x))+0.4, y=predicted, fill=x), shape=21, size=3)+
  theme_bw()+
  labs(y="", x="Mating system")+
  scale_x_discrete(labels=c("S" = "Sexual", "A" = "Apomictic"))+
  scale_fill_manual(values=c("coral3", "cornflowerblue"))+
  theme(legend.position = "none")

png("seed.png", height=8, width=11, res=300, units="in")
ggarrange(gg.seed1, gg.seed3, gg.seed2, gg.seed4, ncol=2, nrow=2, labels=c("Garden", "Source", "Garden", "Source"), hjust=-1.25, vjust=2, font.label=list(size=12))
dev.off()

#### cover ####

cover.ggfx.df$x <- factor(cover.ggfx.df$x, levels=c("S.g", "SO.g", "AO.g", "A.g"))

gg.cover <- ggplot(data=cover.ggfx.df) +
  geom_errorbar(aes(ymax=conf.high, ymin=conf.low, x=x), width=0)+
  geom_point(aes(x=x, y=predicted), shape=21, size=3, fill="black")+
  theme_bw()+
  labs(y="Predicted % vegetation cover", x="Garden region")+
  scale_x_discrete(labels=c("S.g" = expression("S"[g]),"SO.g" = expression("SO"[g]),"AO.g"= expression("AO"[g]),"A.g"= expression("A"[g])))

png("cover.png", height=8, width=11, res=300, units="in")
gg.cover
dev.off()


##### aster mods #####

foo.2 <- foo.1 %>%
  filter(s.region!="A.s", s.region!="AO.s")

foo.3 <- foo.1 %>%
  filter(s.region!="S.s", s.region!="SO.s")

gg.aster1 <- ggplot(data=foo.2, aes(y=estimates, x=g.region, fill=s.region))+
  geom_line(aes(group=s.region), linetype="dotted", position=position_dodge(width=0.4))+
  geom_errorbar(aes(ymax=estimates+std.err, ymin=estimates-std.err), colour="black", width=0, position=position_dodge(width=0.4))+
  geom_point(position=position_dodge(width=0.4), pch=21, size=3, stroke=0.8)+
  theme_bw()+
  labs(y="Predicted lifetime bud production", x="Garden region", fill="Source region")+
  scale_fill_manual(values=c("coral3", "orange"), labels=c(expression("S"[s]), expression("SO"[s])))+
  scale_x_discrete(labels=c("S.g" = expression("S"[g]),"SO.g" = expression("SO"[g]),"AO.g"= expression("AO"[g]),"A.g"= expression("A"[g])))

png("aster1.png", height=6, width=9, res=300, units="in")
gg.aster1
dev.off()

gg.aster2 <- ggplot(data=foo.3, aes(y=estimates, x=g.region, fill=s.region))+
  geom_line(aes(group=s.region), linetype="dotted", position=position_dodge(width=0.4))+
  geom_errorbar(aes(ymax=estimates+std.err, ymin=estimates-std.err), colour="black", width=0, position=position_dodge(width=0.4))+
  geom_point(position=position_dodge(width=0.4), pch=21, size=3, stroke=0.8)+
  theme_bw()+
  labs(y="Predicted lifetime bud production", x="Garden region", fill="Source region")+
  scale_fill_manual(values=c("cyan2", "cornflowerblue"), labels=c(expression("AO"[s]), expression("A"[s])))+
  scale_x_discrete(labels=c("S.g" = expression("S"[g]),"SO.g" = expression("SO"[g]),"AO.g"= expression("AO"[g]),"A.g"= expression("A"[g])))

png("aster2.png", height=6, width=9, res=300, units="in")
gg.aster2
dev.off()

#ebpps.region$ebpps
gg.ebpps.reg <- ggplot(data=ebpps.region, aes(y=ebpps, x=g.region, fill=s.region))+
  geom_hline(yintercept=1)+
  geom_line(aes(group=s.region), linetype="dotted", position=position_dodge(width=0.4))+
  geom_point(position=position_dodge(width=0.4), pch=21, size=3, stroke=0.8)+
  theme_bw()+
  labs(y="EBPPS", x="Garden region", fill="Source region")+
  scale_fill_manual(values=c("coral3", "orange", "cyan2", "cornflowerblue"), labels=c(expression("S"[s]), expression("SO"[s]), expression("AO"[s]), expression("A"[s])))+
  scale_x_discrete(labels=c("S.g" = expression("S"[g]),"SO.g" = expression("SO"[g]),"AO.g"= expression("AO"[g]),"A.g"= expression("A"[g])))

png("ebpps_reg.png", height=6, width=9, res=300, units="in")
gg.ebpps.reg
dev.off()



