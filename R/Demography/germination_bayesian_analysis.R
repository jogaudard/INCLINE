#### Drought germination models ####

#load(Germination_clean_data.R)

#Load libraries
library(rjags)
library(R2jags)
library(ggmcmc)
library(broom.mixed)

logit <- function(p) log( p / (1-p) )
expit <- function(L) exp(L) / (1+exp(L))

#Jags hates NA in independent variables
#Take out NAs in the data that you don't want to model

# group level effects
 site <- factor(Ver_alp_germination_traits$siteID)
 petridish <- factor(Ver_alp_germination_traits$petri_dish)

# independent variables
WP <- as.numeric(Ver_alp_germination_traits$water_potential)
Precip <- as.numeric(Ver_alp_germination_traits$precip)

# dependent variables
GermN <- as.numeric(Ver_alp_germination_traits$n_germinated)
NumSeedDish <- as.numeric(Ver_alp_germination_traits$seeds_in_dish)  #Viability test information needs to be inculded here
N <- as.numeric(length(GermN))

treatmat <- model.matrix(~site*WP)

jags.data <- list("site", "WP", "GermN", "N", "NumSeedDish")
jags.param <- c("alpha", "beta", "alpha.Inc", "beta.Inc", "Presi", "fit", "fit.new") #, "prec1", "sig1", "b", "Presi", "fit", "fit.new"

model_GermN <- function(){
  #group effects
#  for (j in 1:4){lokaliteter[j]~dnorm(0, prec1)}
  
  #likelihood
  for (i in 1:N){
    GermN[i] ~ dbinom(mu[i]*Inc[i], NumSeedDish[i])
    
    
    # linear predictor
    logit(mu[i]) <- alpha[site[i]]+beta[site[i]]*WP[i]
    #  logit(mu[i]) <- inprod(b, treatmat[i,])
    
    #binomial p.Inc
    Inc[i] ~ dbern(p.Inc[i])
    
    #linear predictor of zero inflation
    logit(p.Inc[i]) <- alpha.Inc + beta.Inc*WP[i] + beta2.Inc*pow(WP[i], 2) + beta3.Inc*pow(WP[i], 3)
    
     # pearson residuals and posterior predictive check
     Presi[i] <- (GermN[i]- NumSeedDish[i]*mu[i]) / sqrt(NumSeedDish[i]*mu[i]*(1-mu[i]))
       #res[i] <- pow(GermN[i] - (mu[i]), 2)
     GermN_new[i] ~ dbinom(mu[i], NumSeedDish[i])
     Presi_new[i] <- (GermN_new[i]- NumSeedDish[i]*mu[i]) / sqrt(NumSeedDish[i]*mu[i]*(1-mu[i]))
     #res_new[i] <- pow(GermN_new[i] - (mu[i]), 2)
     D[i] <- pow(Presi[i], 2) #Squared Pearson residuals
     D.new[i] <- pow(Presi_new[i], 2)
   }
   
   #Add up discrepancy measures
   fit <- sum(D[])
   fit.new <- sum(D.new[])
  
  #priors
   for (i in 1:4){
     alpha[i] ~ dnorm(0,0.001) # Intercepts
     beta[i] ~ dnorm(0,0.001) # Slopes
   }
  
  alpha.Inc ~ dnorm(0, 0.001)
  beta.Inc ~ dnorm(0, 0.001)
  beta2.Inc ~ dnorm(0, 0.001)
  beta3.Inc ~ dnorm(0, 0.001)
  
   # for(i in 1:8){b[i] ~ dnorm(0,1.0E-6)} #dnorm in JAGS uses mean and precision (0 = mean and 1.0E-6 = precision) different from dnorm in R that has variance and not precision.
   # prec1 ~ dgamma(0.001, 0.001) 
   # sig <- 1/sqrt(prec1) #getting variance of the random effect
  
  # #derived params
  # rss <- sum(res[])
  # rss_new <- sum(res_new[])
}

inits.fn <- function() list(
  alpha = rnorm(4,0,2),
  beta = rnorm (4,-1,1),
  alpha.Inc = rnorm(1),
  beta.Inc = rnorm(1),
  beta.Inc2 = rnorm(1),
  beta.Inc3 = rnorm(1),
  Inc = rep(1,N)
)

results_GermN <- jags.parallel(data = jags.data,
                               inits = inits.fn,
                               parameters.to.save = jags.param,
                               n.iter = 5000,
                               model.file = model_GermN,
                               n.thin = 5,
                               n.chains = 3)
results_GermN

# traceplots
s <- ggs(as.mcmc(results_GermN))
ggs_traceplot(s, family="b") 

# Homoscedasicity

par(mfrow = c(1,2), cex = 1.5)
plot(results_GermN$BUGSoutput$mean$Presi, ylab = "Residual", las = 1)
abline(h = 0)
plot(WP, results_GermN$BUGSoutput$mean$Presi, ylab = "Residual", las = 1)
abline(h = 0)

# Plotting data

Ver_alp_germination_traits %>% 
  ggplot(aes(x = water_potential, y = logit(n_germinated/seeds_in_dish))) +
  geom_point() +
  facet_wrap(~siteID)


# Posteriori predictive check
par(mfrow = c(1,1))
plot(results_GermN$BUGSoutput$sims.list$fit.new, main = "", xlab = "Discrepancy actual data", ylab = "Discrepancy ideal data")
abline(0,1, lwd = 2, col = "black")

mean(results_GermN$BUGSoutput$sims.list$fit.new > results_GermN$BUGSoutput$sims.list$fit)

# # posterior check
# apdif.paramlist <- results_GermN$BUGSoutput$sims.list
# 
# plot(apdif.paramlist$rss, apdif.paramlist$rss_new, main="AP dif Cover",
#      xlab="SSQ observed", ylab="SSQ simulated")
# abline(0,1)
# 
# mean(apdif.paramlist$rss>apdif.paramlist$rss_new) 


# use waic() function from loo package
ap.waic <- waic(ap.loglik)
ap_eff <- relative_eff(exp(ap.loglik), chain_id=rep(1:1, each=15000))
ap.loo <- loo(ap.loglik, r_eff=ap_eff)

ap.waic
ap.loo
