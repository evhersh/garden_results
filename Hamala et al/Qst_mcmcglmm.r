#Local adaptation and ecological differentiation under selection, migration and drift in Arabidopsis lyrata
#Tuomas Hämälä, Tiina M. Mattila & Outi Savolainen

#Example script for Qst estimation with MCMCglmm.

library(MCMCglmm)

#The Oulu data is analysed.
data <- read.table("Oulu_phenotypes.txt", header=TRUE)

#For convenience, the analysed trait is copied into a new variable.
data$X <- data$Fstart_15

data <- data[!is.na(data$X),]

#Inverse Wishart priors for R and G.
prior.1 <- list(R = list(V=1, nu=0.002), G = list(G1=list(V=1, nu=0.002), G2=list(V=1, nu=0.002)))

#Fitting the model. The response variable is log transformed.
model.mcmc <- MCMCglmm(log(X) ~ 1, random = ~ Pop + Pop:Family, nitt=15000, burnin=5000, data=data, verbose=F, prior=prior.1)

#Checking the model fit.
plot(model.mcmc)

#Variance components from the point estimates.
var <- posterior.mode(model.mcmc$VCV)

#Qst.
var[[1]]/(var[[1]]+4*var[[2]])

#Full posterior distribution of the model variances.
var.dist <- data.frame(model.mcmc$VCV)

ndist <- length(var.dist[,1])

qst.dist <- 0

for(i in 1:ndist) qst.dist[i] <- var.dist[i,1]/(var.dist[i,1]+4*var.dist[i,2])

#95% CI.
quantile(qst.dist, c(0.025, 0.975))