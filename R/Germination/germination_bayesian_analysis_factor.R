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
WP <- as.factor(Ver_alp_germination_traits$water_potential)
Precip <- as.numeric(Ver_alp_germination_traits$precip)

WPsite <- paste(site,WP,sep='_')

# dependent variables
GermN <- as.numeric(Ver_alp_germination_traits$n_germinated)
NumSeedDish <- as.numeric(Ver_alp_germination_traits$seeds_in_dish)  #Viability test information needs to be inculded here
N <- as.numeric(length(GermN))

treatmat <- model.matrix(~site*WP)

jags.data <- list("site", "WPsite", "GermN", "N", "NumSeedDish")
jags.param <- c("alpha", "Presi", "fit", "fit.new") #, "prec1", "sig1", "b"

model_GermN <- function(){
  #group effects
  #  for (j in 1:4){lokaliteter[j]~dnorm(0, prec1)}
  
  #likelihood
  for (i in 1:N){
    GermN[i] ~ dbinom(mu[i], NumSeedDish[i])
    
    # linear predictor
    logit(mu[i]) <- alpha[WPsite[i]]
    #  logit(mu[i]) <- inprod(b, treatmat[i,])
    
    # pearson residuals and posterior predictive check
    Presi[i] <- GermN[i]- NumSeedDish[i]*mu[i] / sqrt(NumSeedDish[i]*mu[i]*(1-mu[i]))
    #res[i] <- pow(GermN[i] - (mu[i]), 2)
    GermN_new[i] ~ dbinom(mu[i], NumSeedDish[i])
    Presi_new[i] <- GermN_new[i]- NumSeedDish[i]*mu[i] / sqrt(NumSeedDish[i]*mu[i]*(1-mu[i]))
    #res_new[i] <- pow(GermN_new[i] - (mu[i]), 2)
    D[i] <- pow(Presi[i], 2) #Squared Pearson residuals
    D.new[i] <- pow(Presi_new[i], 2)
  }
  
  #Add up discrepancy measures
#  fit <- sum(D[])
#  fit.new <- sum(D.new[])
  
  #priors
  for (i in 1:40){
    alpha[i] ~ dnorm(0,0.01) # Intercepts
#    beta[i] ~ dnorm(0,0.01) # Slopes
  }
  
#  for(i in 1:8){b[i] ~ dnorm(0,1.0E-6)} #dnorm in JAGS uses mean and precision (0 = mean and 1.0E-6 = precision) different from dnorm in R that has variance and not precision.
#  prec1 ~ dgamma(0.001, 0.001) 
#  sig <- 1/sqrt(prec1) #getting variance of the random effect
  
  # #derived params
  # rss <- sum(res[])
  # rss_new <- sum(res_new[])
}

results_GermN <- jags.parallel(data = jags.data,
                               inits = NULL,
                               parameters.to.save = jags.param,
                               n.iter = 5000,
                               model.file = model_GermN,
                               n.thin = 5,
                               n.chains = 3)
results_GermN