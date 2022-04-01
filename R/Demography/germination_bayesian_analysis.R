#### Drought germination models ####

#load(Germination_clean_data.R)

#Load libraries
library(rjags)
library(R2jags)
library(ggmcmc)
library(broom.mixed)

#Jags hates NA in independent variables
#Take out NAs in the data that you don't want to model

# group level effects
 site <- factor(Ver_alp_germination_traits$siteID)
 petridish <- factor(Ver_alp_germination_traits$petri_dish)

# independent variables
WP <- as.numeric(Ver_alp_germination_traits$water_potential)
Precip <- as.numeric(Ver_alp_germination_traits$precip)

# dependent variables
GermN <- as.numeric(Ver_alp_germination_traits$n_germinated) #How is this supposed to be: a column with the number of seeds that did germinate? meaning it will be replicated for all the seeds in each petridish. Or 1 if it gerinated and 0 if it didn't?
NumSeedDish <- as.numeric(Ver_alp_germination_traits$seeds_in_dish)  #Viability test information needs to be inculded here
N <- as.numeric(length(GermN))

treatmat <- model.matrix(~site*WP)

jags.data <- list("site", "treatmat", "GermN", "N", "NumSeedDish")
jags.param <- c("b", "prec1", "sig1", "rss", "rss_new")

model_GermN <- function(){
  #group effects
  for (j in 1:4){lokaliteter[j]~dnorm(0, prec1)}
  
  #likelihood
  for (i in 1:N){
    GermN[i] ~ dbinom(mu[i], NumSeedDish[i])
    logit(mu[i]) <- inprod(b, treatmat[i,])
    
    #posterior predictive checks (PPC)
    res[i] <- pow(GermN[i] - (mu[i]), 2)
    GermN_new[i] ~ dbinom(mu[i], NumSeedDish[i])
    res_new[i] <- pow(GermN_new[i] - (mu[i]), 2)
  }
  
  #priors
  for(i in 1:8){b[i] ~ dnorm(0,1.0E-6)} #dnorm in JAGS uses mean and precision (0 = mean and 1.0E-6 = precision) different from dnorm in R that has variance and not precision.
  prec1 ~ dgamma(0.001, 0.001) 
  sig <- 1/sqrt(prec1) #getting variance of the random effect
  
  #derived params
  rss <- sum(res[])
  rss_new <- sum(res_new[])
}

results_GermN <- jags.parallel(data = jags.data,
                               inits = NULL,
                               parameters.to.save = jags.param,
                               n.iter = 50000,
                               model.file = model_GermN,
                               n.thin = 5,
                               n.chains = 3)
results_GermN

# traceplots
s <- ggs(as.mcmc(results_GermN))
ggs_traceplot(s, family="b") 

# posterior check
apdif.paramlist <- results_GermN$BUGSoutput$sims.list

plot(apdif.paramlist$rss,apdif.paramlist$rss_new,main="AP dif Cover",
     xlab="SSQ observed", ylab="SSQ simulated")
abline(0,1)

mean(apdif.paramlist$rss>apdif.paramlist$rss_new) 


# use waic() function from loo package
ap.waic <- waic(ap.loglik)
ap_eff <- relative_eff(exp(ap.loglik), chain_id=rep(1:1, each=15000))
ap.loo <- loo(ap.loglik, r_eff=ap_eff)

ap.waic
ap.loo
