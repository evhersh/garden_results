##########
# Packages

# library(MASS)
# library(gdata)
# library(plotrix)
# library(aster)
# library(lattice)
# library(ggplot2)
# library(knitr)
# library(rsample)
# library(plyr)
# library(dplyr)

########
# OG calcs

# establishment success
est.ms <- G.dat %>%
  group_by(ms, garden) %>%
  summarize(mean.est = mean(germ, na.rm=TRUE), se.est = std.error(germ))

# buds per planted
budsum.ms <- aster.dat %>%
  group_by(ms, garden) %>%
  summarize(bud.sum = sum(bud.num, na.rm=TRUE))

buds.per.planted <- data.frame(budsum.ms[,1:3], num.planted[,3])
buds.per.planted$bpp <- buds.per.planted$bud.sum/buds.per.planted$n

# seeds
S.dat.nat$gspb <- S.dat.nat$good.ratio * S.dat.nat$seed.per.bud

seeds.ms <- S.dat.nat %>%
  group_by(ms) %>%
  summarize(mean.gspb = mean(gspb), se.gspb = std.error(gspb))

########
# bootstrap

#*******************************************************************************
#### 2. Create nested loop to obtain replicate bootstrap datasets for each site, sampling with replacement ###
#*******************************************************************************

# Obtain a data frame of unique IDs from each Site
ID.by.Site=unique(aster.dat[,c(1,10)])

# Create a vector of unique Site names for subsetting; note this is sorted by decreasing latitude 
site=unique(aster.dat$garden)

# Create empty list to be filled in loop
data.boot.rep=list()
id.boot=list()

# Set seed for random sampling to obtain reproducible results
#seed=123

# Set number of bootstrap replicate datasets
n.boot=100

# Create loop to obtain replicate bootstrap datasets
for (i in 1:length(site)) {
  data.garden=subset(aster.dat,garden==site[i]) # select data from site i
  id.site=subset(ID.by.Site,garden==site[i]) # select list of unique individual IDs from site i
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
ID.by.Site=unique(G.dat[,c(5,14)])

# Create a vector of unique Site names for subsetting; note this is sorted by decreasing latitude 
site=unique(G.dat$garden)

# Create empty list to be filled in loop
data.boot.rep=list()
id.boot=list()

# Set seed for random sampling to obtain reproducible results
#seed=123

# Set number of bootstrap replicate datasets
n.boot=100

# Create loop to obtain replicate bootstrap datasets
for (i in 1:length(site)) {
  data.garden=subset(G.dat,garden==site[i]) # select data from site i
  id.site=subset(ID.by.Site,garden==site[i]) # select list of unique individual IDs from site i
  id.boot <- lapply(1:n.boot, function(j) { 
    #set.seed(j+seed)
    sample_n(id.site,size=nrow(id.site), replace = T)}) %>% ldply() # resample rows of site i's data with replacement and size=number of unique individuals in original dataset for each site and convert list to data frame
  
  id.boot$Replicate=rep(seq(1:n.boot),each=nrow(id.site)) # create a column in data frame that corresponds to bootstrap replicate
  data.boot=join(id.boot,data.garden,type="left",match="all") # merge bootstrapped list of unique IDs to full dataset
  data.boot.rep[[i]]=data.boot # add each site's dataframe of n.boot bootstrap replicates to list
}

# Convert list to data frame
G.bootstrapped.data <- do.call(rbind, data.boot.rep)



#************************************
# Same but for seed data
#************************************
# Obtain a data frame of unique IDs from each Site
ID.by.Site=unique(S.dat.garden[,c(1,3)])

# Create a vector of unique Site names for subsetting; note this is sorted by decreasing latitude 
site=unique(S.dat.garden$garden)

# Create empty list to be filled in loop
data.boot.rep=list()
id.boot=list()

# Set seed for random sampling to obtain reproducible results
#seed=123

# Set number of bootstrap replicate datasets
n.boot=100

# Create loop to obtain replicate bootstrap datasets
for (i in 1:length(site)) {
  data.garden=subset(S.dat.garden,garden==site[i]) # select data from site i
  id.site=subset(ID.by.Site,garden==site[i]) # select list of unique individual IDs from site i
  id.boot <- lapply(1:n.boot, function(j) { 
    #set.seed(j+seed)
    sample_n(id.site,size=nrow(id.site), replace = T)}) %>% ldply() # resample rows of site i's data with replacement and size=number of unique individuals in original dataset for each site and convert list to data frame
  
  id.boot$Replicate=rep(seq(1:n.boot),each=nrow(id.site)) # create a column in data frame that corresponds to bootstrap replicate
  data.boot=join(id.boot,data.garden,type="left",match="all") # merge bootstrapped list of unique IDs to full dataset
  data.boot.rep[[i]]=data.boot # add each site's dataframe of n.boot bootstrap replicates to list
}

# Convert list to data frame
S.bootstrapped.data <- do.call(rbind, data.boot.rep)
