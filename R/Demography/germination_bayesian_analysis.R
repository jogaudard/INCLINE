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
# site <- factor(data$siteID)
# petridish <- factor(data$petridish)

# independent variables
#WP <- as.numeric(data$WP)
#Precip <- as.numeric(data$precip)

# dependent variables
#GermN <- as.numeric(data$GermN)
#NumSeedDish <- as.numeric(data$NumSeedDish)  Viability test information needs to be inculded here
#N <- as.numeric(length(GermMax))

#jags.data <- list("site", "WP", "Precip", "GermMax", "N")
jags.param <- c("b", "prec1", "sig1", "rss", "rss_new")

model_GermN <- function(){
  #group effects
  for (j in 1:4){lokaliteter[j]~dnorm(0, prec1)}
  
  #likelihood
  for (i in 1:N){
    GermN[i] ~ dbinom(mu[i], NumSeedDish[i])
    logit(mu[i]) <- b[1] + b[2] * WP[i] + b[3] * Precip[i] + 
      b[4] * WP[i] * Precip[i] + lokaliteter[site[i]]
    
    #posterior predictive checks (PPC)
    res[i] <- pow(GermN[i] - mu[i], 2)
    GermN_new[i] ~ dbinom(mu[i], NumSeedDish[i])
    res_new[i] <- pow(GermN_new[i] - mu[i], 2)
  }
  
  #priors
  for(i in 1:4){b[i] ~ dnorm(0,1.0E-6)} #dnorm in JAGS uses mean and precision (0 = mean and 1.0E-6 = precision) different from dnorm in R that has variance and not precision.
  prec1 ~ dgamma(0.001, 0.001) 
  sig <- 1/sqrt(prec1) #getting variance of the random effect
  
  #derived params
  rss <- sum(res[])
  rss_new <- sum(res_new[])
}

results_GermN <- jags.parallel(data = jags.data,
                               inits = NULL,
                               parameters.to.save = jags.params,
                               n.itter = 10000,
                               model.file = model_GermN,
                               n.thin = 5,
                               n.chains = 3)
results_GermN

