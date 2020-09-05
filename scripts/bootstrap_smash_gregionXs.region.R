# remove objects and clear workspace
#rm(list = ls(all=TRUE))

# require packages
#require(plyr)
# require(dplyr)
require(Hmisc)
require(tidyverse)

# obtain mean seed counts per fruit per site
# seeds.per.site <- S.bootstrapped.data %>%
#   dplyr::group_by(garden, ms) %>%
#   dplyr::summarize(mean.seed = mean(seed.per.bud))

# Create a vector of unique Site names for subsetting; note this is sorted by decreasing latitude 
site=unique(H.bootstrapped.data$garden)

# Set number of bootstrap replicate datasets
n.boot=1000

#*******************************************************************************
#### 2. Obtain vital rate parameters across all sites for each replicate bootstrap dataset ###
#*******************************************************************************

# Create empty list to be filled in loop
means.boot=list()

# Begin loop to extract vital rate parameters for each bootstrapped dataset
for (k in 1:n.boot) {
  
  # Set up data frame of model parameters
  means=c()
  G.means=c()
  H.means=c()
  S.means=c()
  ## ESTABLISHMENT ##
  
  G.data.rep=subset(G.bootstrapped.data,Replicate==k) # select data from replicate k
  
  est.s.region <- G.data.rep %>%
    dplyr::group_by(s.region, g.region) %>%
    dplyr::summarize(mean.est = mean(germ.10, na.rm=TRUE), se.est = std.error(germ.10))
  
  G.means$s.region <- est.s.region$s.region 
  G.means$g.region <- est.s.region$g.region
  G.means$est <- est.s.region$mean.est
  
  # bpp
  
  H.data.rep=subset(H.bootstrapped.data,Replicate==k)
  
  budsum.s.region <- H.data.rep %>%
    dplyr::group_by(s.region, g.region) %>%
    dplyr::summarize(budsum = sum(budsum, na.rm=TRUE))
  
  num.planted <- H.data.rep %>%
    dplyr::group_by(s.region, g.region) %>%
    dplyr::summarize(n=n())
  
  buds.per.planted <- data.frame(budsum.s.region[,1:3], num.planted[,3])
  buds.per.planted$bpp <- buds.per.planted$budsum/buds.per.planted$n
  
  H.means$s.region <- buds.per.planted$s.region
  H.means$g.region <- buds.per.planted$g.region
  H.means$bpp <- buds.per.planted$bpp
  
  # seeds
  
  # S.data.rep=subset(S.bootstrapped.data,Replicate==k)
  # 
  # spb.ms.garden <- S.data.rep %>%
  #   dplyr::group_by(ms, garden) %>%
  #   dplyr::summarize(mean.spb = mean(seed.per.bud), se.spb = std.error(seed.per.bud))
  # 
  # S.means$ms <- spb.ms.garden$ms
  # S.means$garden <- spb.ms.garden$garden
  # S.means$spb <- spb.ms.garden$mean.spb
  
  # merge
  
  means = merge(G.means, H.means, by = c("s.region", "g.region"))
  #means = merge(means, S.means, by=c("ms", "garden"))
  
  # Store data frame of parameter values for a given bootstrap replicate dataset into list
  means$Replicate=rep(k,each=nrow(means)) # create a column in data frame that corresponds to bootstrap replicate
  means.boot[[k]]=means
  print(k)
} # end loop


# turn bootstrap table into DF
bootstrapped.means <- do.call(rbind, means.boot)

# calculate ebpps (estimate of seed to seed)
#bootstrapped.means$ebpps <- bootstrapped.means$est * bootstrapped.means$bpp * bootstrapped.means$spb
bootstrapped.means$ebpp <- bootstrapped.means$est * bootstrapped.means$bpp
bootstrapped.means$s.region_gregion <- paste(bootstrapped.means$s.region, "-", bootstrapped.means$g.region)

# calculate mean for each ms*garden

# mean.ebpps <- bootstrapped.means %>%
#   dplyr::group_by(ms, garden) %>%
#   dplyr::summarize(mean.ebpps=mean(ebpps))

mean.ebpp <- bootstrapped.means %>%
  dplyr::group_by(s.region, g.region) %>%
  dplyr::summarize(mean.ebpp=mean(ebpp))

#mean.ebpps$ms_g <- paste(mean.ebpps$ms, "-", mean.ebpps$garden)
mean.ebpp$s.region_gregion <- paste(mean.ebpp$s.region, "-", mean.ebpp$g.region)

mean.ci.bias=c()

# switch between ebpp and ebpps
for (i in 1:length(mean.ebpp$s.region_gregion)) {
  boot.site=bootstrapped.means$ebpp[bootstrapped.means$s.region_gregion==mean.ebpp$s.region_gregion[i]]
  obs.lambda=mean.ebpp$mean.ebpp[mean.ebpp$s.region_gregion==mean.ebpp$s.region_gregion[i]]
  z=qnorm(length(boot.site[boot.site<obs.lambda])/length(boot.site))
  mean.ci.bias$s.region_gregion=mean.ebpp$s.region_gregion
  lower.ci=pnorm(2*z-1.96)
  upper.ci=pnorm(2*z+1.96)
  mean.ci.bias$lower.ci[i]=quantile(boot.site,probs=lower.ci)
  mean.ci.bias$upper.ci[i]=quantile(boot.site,probs=upper.ci) 
}

# for (i in 1:length(site.lambda$Site)) {
#   boot.site=boot.lambda$lambda[boot.lambda$Site==site.lambda$Site[i]]
#   obs.lambda=site.lambda$lambda[site.lambda$Site==site.lambda$Site[i]]
#   z=qnorm(length(boot.site[boot.site<obs.lambda])/length(boot.site))
#   lambda.ci.bias$Site=site.lambda$Site 
#   lower.ci=pnorm(2*z-1.96)
#   upper.ci=pnorm(2*z+1.96)
#   lambda.ci.bias$lower.ci[i]=quantile(boot.site,probs=lower.ci)
#   lambda.ci.bias$upper.ci[i]=quantile(boot.site,probs=upper.ci) 
# }


# convert to data frame
mean.ci.bias=data.frame(mean.ci.bias)

# Merge confidence intervals for lambda with data frame including lambda, latitude, etc.
#ebpps.ci=merge(mean.ebpps,mean.ci.bias,by="ms_g")
ebpp.ci=merge(mean.ebpp,mean.ci.bias,by="s.region_gregion")

# gg.ebpps.boot <- ggplot(data=(ebpps.ci), aes(x=garden, y=mean.ebpps, colour=ms, group=ms))+
#   geom_point(aes(fill=ms), colour="black", pch=21, size=3, position=position_dodge(width=0.2))+
#   geom_errorbar(data=ebpps.ci, aes(y=mean.ebpps, ymin=lower.ci, ymax=upper.ci), colour="black", position=position_dodge(width=0.2), width=0.2, alpha=0.5)+
#   theme_bw()+
#   labs(y = "EBPPS", x = "Garden")

ebpp.ci$g.region <- factor(ebpp.ci$g.region, levels = c("S.g", "SO.g", "AO.g", "A.g"))

gg.ebpp.boot <- ggplot(data=(ebpp.ci), aes(x=g.region, y=mean.ebpp, colour=s.region, group=s.region))+
  geom_point(aes(fill=s.region), colour="black", pch=21, size=3, position=position_dodge(width=0.2))+
  geom_errorbar(data=ebpp.ci, aes(ymin=lower.ci, ymax=upper.ci), colour="black", position=position_dodge(width=0.2), width=0.2, alpha=0.5)+
  theme_bw()+
  labs(y = "EBPP", x = "Garden region")+
  scale_fill_manual(values=c("#F8766D", "orange", "#C77CFF", "#00BFC4"))


