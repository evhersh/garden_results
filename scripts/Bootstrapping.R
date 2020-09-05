##########
# Packages

library(MASS)
library(gdata)
library(plotrix)
library(aster)
library(lattice)
library(ggplot2)
library(knitr)
library(rsample)
library(plyr)
library(dplyr)
require(Hmisc)
require(tidyverse)

########
# bootstrap

#*******************************************************************************
#### 2. Create nested loop to obtain replicate bootstrap datasets for each site, sampling with replacement ###
#*******************************************************************************

# Obtain a data frame of unique IDs from each Site
ID.by.Site=unique(aster.dat[,c(43,10)])

# Create a vector of unique Site names for subsetting; note this is sorted by decreasing latitude 
site=unique(aster.dat$ms_p_g)

# Create empty list to be filled in loop
data.boot.rep=list()
id.boot=list()

# Set seed for random sampling to obtain reproducible results
#seed=123

# Set number of bootstrap replicate datasets
n.boot=3000

# Create loop to obtain replicate bootstrap datasets
for (i in 1:length(site)) {
  data.garden=subset(aster.dat,ms_p_g==site[i]) # select data from site i
  id.site=subset(ID.by.Site,ms_p_g==site[i]) # select list of unique individual IDs from site i
  id.boot <- lapply(1:n.boot, function(j) { 
    #set.seed(j+seed)
    sample_n(id.site,size=nrow(id.site), replace = T)}) %>% ldply() # resample rows of site i's data with replacement and size=number of unique individuals in original dataset for each site and convert list to data frame
  
  id.boot$Replicate=rep(seq(1:n.boot),each=nrow(id.site)) # create a column in data frame that corresponds to bootstrap replicate
  data.boot=join(id.boot,data.garden,type="left",match="all") # merge bootstrapped list of unique IDs to full dataset
  data.boot.rep[[i]]=data.boot # add each site's dataframe of n.boot bootstrap replicates to list
}

# Convert list to data frame
H.bootstrapped.data <- do.call(rbind, data.boot.rep) 


#************************************
# Same but for establishment data
#************************************
# Obtain a data frame of unique IDs from each Site
G.dat.narm <- na.omit(G.dat)
ID.by.Site=unique(G.dat.narm[,c(17,14)])

# Create a vector of unique Site names for subsetting; note this is sorted by decreasing latitude 
site=unique(G.dat.narm$ms_p_g)

# Create empty list to be filled in loop
data.boot.rep=list()
id.boot=list()

# Set seed for random sampling to obtain reproducible results
#seed=123

# Set number of bootstrap replicate datasets
n.boot=3000

# Create loop to obtain replicate bootstrap datasets
for (i in 1:length(site)) {
  data.garden=subset(G.dat.narm,ms_p_g==site[i]) # select data from site i
  id.site=subset(ID.by.Site,ms_p_g==site[i]) # select list of unique individual IDs from site i
  id.boot <- lapply(1:n.boot, function(j) { 
    #set.seed(j+seed)
    sample_n(id.site,size=nrow(id.site), replace = T)}) %>% ldply() # resample rows of site i's data with replacement and size=number of unique individuals in original dataset for each site and convert list to data frame
  
  id.boot$Replicate=rep(seq(1:n.boot),each=nrow(id.site)) # create a column in data frame that corresponds to bootstrap replicate
  data.boot=join(id.boot,data.garden,type="left",match="all") # merge bootstrapped list of unique IDs to full dataset
  data.boot.rep[[i]]=data.boot # add each site's dataframe of n.boot bootstrap replicates to list
}

# Convert list to data frame
G.bootstrapped.data <- do.call(rbind, data.boot.rep)




#***********************
# Combine bootstrapped datasets and calculate estimates
#***********************

# Create a vector of unique Site names for subsetting; note this is sorted by decreasing latitude 
site=unique(H.bootstrapped.data$garden)

# Set number of bootstrap replicate datasets
n.boot=3000

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
  ## ESTABLISHMENT ##
  
  G.data.rep=subset(G.bootstrapped.data,Replicate==k) # select data from replicate k
  
  est.ms <- G.data.rep %>%
    dplyr::group_by(ms, garden) %>%
    dplyr::summarize(mean.est = mean(germ, na.rm=TRUE), se.est = std.error(germ))
  
  G.means$ms <- est.ms$ms 
  G.means$garden <- est.ms$garden
  G.means$est <- est.ms$mean.est
  
  # bpp
  
  H.data.rep=subset(H.bootstrapped.data,Replicate==k)
  
  budsum.ms <- H.data.rep %>%
    dplyr::group_by(ms, garden) %>%
    dplyr::summarize(budsum = sum(budsum, na.rm=TRUE))
  
  num.planted <- H.data.rep %>%
    dplyr::group_by(ms, garden) %>%
    dplyr::summarize(n=n())
  
  buds.per.planted <- data.frame(budsum.ms[,1:3], num.planted[,3])
  buds.per.planted$bpp <- buds.per.planted$budsum/buds.per.planted$n
  
  H.means$ms <- buds.per.planted$ms
  H.means$garden <- buds.per.planted$garden
  H.means$bpp <- buds.per.planted$bpp
  
  
  # merge
  
  means = merge(G.means, H.means, by = c("ms", "garden"))
  
  # Store data frame of parameter values for a given bootstrap replicate dataset into list
  means$Replicate=rep(k,each=nrow(means)) # create a column in data frame that corresponds to bootstrap replicate
  means.boot[[k]]=means
  print(k)
} # end loop


# turn bootstrap table into DF
bootstrapped.means <- do.call(rbind, means.boot)

# calculate ebpp (estimate of seed to seed)
bootstrapped.means$ebpp <- bootstrapped.means$est * bootstrapped.means$bpp
bootstrapped.means$ms_g <- paste(bootstrapped.means$ms, "-", bootstrapped.means$garden)

# calculate mean for each ms*garden

est.ms <- G.dat %>%
  dplyr::group_by(ms, garden) %>%
  dplyr::summarize(mean.est = mean(germ, na.rm=TRUE), se.est = std.error(germ))

budsum.ms <- aster.dat %>%
  dplyr::group_by(ms, garden) %>%
  dplyr::summarize(budsum = sum(budsum, na.rm=TRUE))

num.planted <- aster.dat %>%
  dplyr::group_by(ms, garden) %>%
  dplyr::summarize(n=n())

buds.per.planted <- data.frame(budsum.ms[,1:3], num.planted[,3])
buds.per.planted$bpp <- buds.per.planted$budsum/buds.per.planted$n

ebpp.ms <- data.frame(buds.per.planted, est.ms[,3])

ebpp.ms$ebpp <- ebpp.ms$bpp * ebpp.ms$mean.est

ebpp.ms$ms_g <- paste(ebpp.ms$ms, "-", ebpp.ms$garden)

mean.ci.bias=c()

# switch between ebpp and ebpps
for (i in 1:length(ebpp.ms$ms_g)) { # calculate CI's for each ms_g
  boot.ms_g=bootstrapped.means$ebpp[bootstrapped.means$ms_g==ebpp.ms$ms_g[i]] # sets bootstrapped ebpp value for that ms_g
  obs.ebpp=ebpp.ms$ebpp[ebpp.ms$ms_g==ebpp.ms$ms_g[i]] # sets observed bpp value for that ms_g
  z=qnorm(length(boot.ms_g[boot.ms_g<obs.ebpp])/length(boot.ms_g)) 
  mean.ci.bias$ms_g=ebpp.ms$ms_g
  lower.ci=pnorm(2*z-1.96)
  upper.ci=pnorm(2*z+1.96)
  mean.ci.bias$lower.ci[i]=quantile(boot.ms_g,probs=lower.ci)
  mean.ci.bias$upper.ci[i]=quantile(boot.ms_g,probs=upper.ci) 
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

ebpp.ci=merge(ebpp.ms,mean.ci.bias,by="ms_g")


gg.ebpp.boot <- ggplot(data=(ebpp.ci), aes(x=garden, y=ebpp, colour=ms, group=ms))+
  geom_point(aes(fill=ms), colour="black", pch=21, size=3, position=position_dodge(width=0.2))+
  geom_errorbar(data=ebpp.ci, aes(ymin=lower.ci, ymax=upper.ci), colour="black", position=position_dodge(width=0.2), width=0.2, alpha=0.5)+
  theme_bw()+
  labs(y = "EBPP", x = "Garden")


###### check calculations ######

hist(subset(bootstrapped.means$ebpp, bootstrapped.means$ms_g == "A - AA1"))
hist(subset(bootstrapped.means$ebpp, bootstrapped.means$ms_g == "A - AA2"))
