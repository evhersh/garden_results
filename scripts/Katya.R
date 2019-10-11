##### Load Packages #####
install.packages("tidyverse")
library(tidyverse)
devtools::install_github("choisy/cutoff")
library(cutoff)
install.packages("bbmle")
library(bbmle)

##### Load Data #####
flow.dat <- read.csv(file.choose(), header=TRUE, sep=",", strip.white=TRUE, na.string=c("NA", ""), stringsAsFactors = FALSE)






##### Factors/Levels/etc #####
names(flow.dat)
class(flow.dat$ID)

# shortcut object for hookeri.pg from flow.dat dataframe
gsize <- flow.dat$hookeri.pg

# set Assumed.ploidy to be a "factor", and set "levels"
flow.dat$Assumed.ploidy <- factor(flow.dat$Assumed.ploidy, levels=c("Diploid", "Trip", "Tetra"))





##### Summary / Exploration #####

# change a single value in the dataframe
flow.dat$ID[1] = "3003"
flow.dat[1,1] = "3003B"
# change this one value from "Triploid" to "Trip" so that it matches the others
flow.dat$Assumed.ploidy[44] = "Trip"

# calculate means of variables, and subset
mean(flow.dat$hookeri.pg)
mean(subset(flow.dat$hookeri.pg, flow.dat$Assumed.ploidy == "Diploid"))
mean(subset(flow.dat$hookeri.pg, flow.dat$Assumed.ploidy == "Trip"))
mean(subset(flow.dat$hookeri.pg, flow.dat$Assumed.ploidy == "Tetra"))

# calculate sd
sd(subset(flow.dat$hookeri.pg, flow.dat$Assumed.ploidy == "Diploid"))





##### Models #####

# linear model (regression?) checking to see if genome size differs between ploidy levels
lm.pg <- lm(hookeri.pg ~ Assumed.ploidy, data=flow.dat)
summary(lm.pg)
anova(lm.pg)

# lm for genome size differing by population
lm.pg_pop <- lm(hookeri.pg ~ pop, data=subset(flow.dat, Assumed.ploidy=="Trip"))
summary(lm.pg_pop)
anova(lm.pg_pop)






##### Plotting #####

# simple "base" R plots
boxplot(flow.dat$hookeri.pg ~ flow.dat$Assumed.ploidy)
plot(flow.dat$mean ~ flow.dat$cv)

# make histogram and look at cutoffs from http://marcchoisy.free.fr/fmm/index.html
hist(flow.dat$hookeri.pg, 75)

hist(flow.dat$hookeri.pg,100,F,xlab="Genome Size (pg)",ylab="density", main=NULL,col="grey")
lines(density(flow.dat$hookeri.pg),lwd=1.5,col="blue")

flow.dat_out <- em(gsize,"normal","normal")

hist(flow.dat$hookeri.pg,100,F,xlab="genome size",ylab="density", main=NULL,col="grey")
lines(flow.dat_out,lwd=1.5,col="red")

cut_off <- cutoff(flow.dat_out)

polygon(c(cut_off[-1],rev(cut_off[-1])),c(0,0,.55,.55),col=rgb(0,0,1,.2),border=NA)
abline(v=cut_off[-1],lty=2,col="blue")
abline(v=cut_off[1],col="blue")



# ggplot boxplot
gg.box <- ggplot(data=flow.dat, aes(y=hookeri.pg, x=Assumed.ploidy, fill=Assumed.ploidy), colour="black")+
  geom_boxplot()+
  theme_bw()+
  geom_jitter(alpha=0.5, width=0.1)

# same but for pops
gg.points <- ggplot(data=flow.dat, aes(y=hookeri.pg, x=pop, fill=Assumed.ploidy))+
  geom_point(pch=21, colour="black", size=4)+
  theme_bw()+
  labs(y="Genome Size (pg)", x="Population", title="Flow Cytometry results")
  

