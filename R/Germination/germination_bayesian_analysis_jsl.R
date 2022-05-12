#### Drought germination models ####

#load(Germination_clean_data.R)

#Load libraries
library(rjags)
library(R2jags)
library(ggmcmc)
library(broom.mixed)
library(car) # use car package for logit function- adjusts to avoid infinity
library("patchwork")

# load data 
source("R/Germination/cleaning_germination_lab_data.R")

### Testing time to max gemrination by visually inspecting the data to see if there actually is a problem to fix
#Veronica looks mostly ok. There is one replicate in SKJ that has T50 of 7 or someting and days to max germination at 90. Otherwise they look ok.
ggplot(aes(x = as.numeric(T50), y = as.numeric(days_to_max_germination), color = germ_percent), data = Ver_alp_germination_traits) + 
  geom_point() + 
  facet_wrap(~siteID) + 
  geom_smooth(method = "lm") + 
  geom_abline()

#Sibbaldia at LAV is crazy. I checked, and this is true data. Almost everything germinated in the first weeks, so the T50 is there, and then one after another more seeds germinated, so that they kept increasing until max germination time. This is a very different germination strategy than the other populations and the other species. Not sure what to do with it.
#Many more points on the one-to-one line, I assume that is because there were only one or two germination time(s).
ggplot(aes(x = as.numeric(T50), y = as.numeric(days_to_max_germination), color = germ_percent), data = Sib_pro_germination_traits) + 
  geom_point() + 
  facet_wrap(~siteID) + 
  geom_smooth(method = "lm") + 
  geom_abline()


# Smaller changes to the data sets
Sib_pro_germination_traits <- Sib_pro_germination_traits %>% 
  mutate(water_potential = as.numeric(water_potential), #Make water potential numeric
         seeds_in_dish = as.numeric(seeds_in_dish)) %>% #Make seeds_in_dish numeric so that the case_when works
  mutate(seeds_in_dish = case_when(n_germinated < seeds_in_dish ~ (seeds_in_dish - 1),
                                   n_germinated == seeds_in_dish ~ (seeds_in_dish),
                                   is.na(n_germinated) ~ (seeds_in_dish - 1)))#Remove unviable seeds. The averages across species and populations were closest to 1, so therefore we are making a generic decision to remove one across all petri dishes

Ver_alp_germination_traits <- Ver_alp_germination_traits %>% 
  mutate(water_potential = as.numeric(water_potential), #Make water potential numeric
         seeds_in_dish = as.numeric(seeds_in_dish)) %>% #Make seeds_in_dish numeric so that the case_when works
  mutate(seeds_in_dish = case_when(n_germinated < seeds_in_dish ~ (seeds_in_dish - 1),
                                   n_germinated == seeds_in_dish ~ (seeds_in_dish),
                                   is.na(n_germinated) ~ (seeds_in_dish - 1))) #Remove unviable seeds. The averages across species and populations were closest to 1, so therefore we are making a generic decision to remove one across all petri dishes

# inverse logit function
invlogit<-function(x){a <- exp(x)/(1+exp(x))
a[is.nan(a)]=1
return(a)
}

# function to standardize dpe var
standard <- function(x) round((x - mean(x, na.rm=T)) / ((sd(x, na.rm=T))), digits = 2)

# Palette for plotting
Precip_palette <- c("#BAD8F7", "#89B7E1", "#2E75B6", "#213964")

#Jags hates NA in independent variables
#Take out NAs in the data that you don't want to model

#### Germination percentage Veronica alpina ####
# take out the too many zer0s
guddat <-  Ver_alp_germination_traits %>% filter (siteID == "GUD") %>% filter(water_potential < 8)
lavdat <- Ver_alp_germination_traits %>% filter (siteID == "LAV") %>% filter(water_potential < 9)
skjdat <- Ver_alp_germination_traits %>% filter (siteID == "SKJ") %>% filter(water_potential < 8)
ulvdat <- Ver_alp_germination_traits %>% filter (siteID == "ULV") 

dat <- bind_rows(guddat, lavdat, skjdat, ulvdat) 
# group level effects
#site <- factor(dat$siteID)
petridish <- factor(dat$petri_dish)
n_petri <- length(levels(petridish))

# independent variables
WP <- as.numeric(standard(as.numeric(dat$water_potential)))
WP_MPa <- as.numeric(standard(dat$WP_MPa))
#Precip <- as.numeric(standard(dat$precip))
Precip <- as.factor(dat$precip)

# dependent variables
GermN <- as.numeric(dat$n_germinated)
NumSeedDish <- as.numeric(dat$seeds_in_dish)
N <- as.numeric(length(GermN))


treatmat <- model.matrix(~Precip*WP_MPa) 
n_parm <- as.numeric(ncol(treatmat))

# look at the data before the analysis
ggplot(dat, aes(x=water_potential,y = logit(n_germinated/seeds_in_dish, adjust = 0.01)))+
  geom_point()+facet_wrap(~siteID)

jags.data <- list("treatmat", "GermN", "N", "NumSeedDish", "n_parm")
jags.param <- c("b",  "Presi", "rss", "rss_new", "sig1") 

model_GermN <- function(){
  #group effects
  #for (j in 1:4){lokaliteter[j]~dnorm(0, prec1)}
  
  #likelihood
  for (i in 1:N){
    GermN[i] ~ dbinom(mu[i], NumSeedDish[i])
    
    # linear predictor
      logit(mu[i]) <- inprod(b, treatmat[i,]) #+ lokaliteter[site[i]]

     # residual sum of squares
     res[i] <- pow((GermN[i]/NumSeedDish[i]) - (mu[i]), 2)
     GermN_new[i] ~ dbinom(mu[i], NumSeedDish[i])
     res_new[i] <- pow((GermN_new[i]/NumSeedDish[i]) - (mu[i]), 2)
   }
   
    for(i in 1:n_parm){b[i] ~ dnorm(0,1.0E-6)} #dnorm in JAGS uses mean and precision (0 = mean and 1.0E-6 = precision) different from dnorm in R that has variance and not precision.
    #prec1 ~ dgamma(0.001, 0.001) 
    #sig1 <- 1/sqrt(prec1) #getting variance of the random effect
  
  # #derived params
   rss <- sum(res[])
   rss_new <- sum(res_new[])
}

results_Germ_percent_Ver_alp <- jags.parallel(data = jags.data,
                               #inits = inits.fn,
                               parameters.to.save = jags.param,
                               n.iter = 100000,
                               model.file = model_GermN,
                               n.thin = 5,
                               n.chains = 3,
                               n.burnin = 35000)
results_Germ_percent_Ver_alp

# traceplots
s <- ggs(as.mcmc(results_Germ_percent_Ver_alp))
ggs_traceplot(s, family="b") 

# check Gelman Rubin Statistics
gelman.diag(as.mcmc(results_Germ_percent_Ver_alp))

# Posterior predictive check
plot(results_Germ_percent_Ver_alp$BUGSoutput$sims.list$rss_new, results_Germ_percent_Ver_alp$BUGSoutput$sims.list$rss,
     main = "",)
abline(0,1, lwd = 2, col = "black")

mean(results_Germ_percent_Ver_alp$BUGSoutput$sims.list$rss_new > results_Germ_percent_Ver_alp$BUGSoutput$sims.list$rss)

## put together for figure  and r^2
mcmc <- results_Germ_percent_Ver_alp$BUGSoutput$sims.matrix
coefs = mcmc[, c("b[1]", "b[2]", "b[3]", "b[4]", "b[5]", "b[6]", "b[7]", "b[8]")]
fit = coefs %*% t(treatmat)
resid = sweep(fit, 2, logit(GermN/NumSeedDish, adjust = 0.01), "-")
var_f = apply(fit, 1, var)
var_e = apply(resid, 1, var)
R2 = var_f/(var_f + var_e)
tidyMCMC(as.mcmc(R2), conf.int = TRUE, conf.method = "HPDinterval")

#residuals
coefs2 = apply(coefs, 2, median)
fit2 = as.vector(coefs2 %*% t(treatmat))
resid2 <- (GermN/NumSeedDish) - invlogit(fit2)
sresid2 <- resid2/sd(resid2)
ggplot() + geom_point(data = NULL, aes(y = resid2, x = invlogit(fit2)))
hist(resid2)

# check predicted versus observed
yRep = sapply(1:nrow(mcmc), function(i) rbinom(nrow(dat), NumSeedDish[i], invlogit(fit[i,])))
ggplot() + geom_density(data = NULL, aes(x = (as.vector(yRep)/NumSeedDish),
                               fill = "Model"), alpha = 0.5) + 
  geom_density(data = dat, aes(x = (GermN/NumSeedDish), fill = "Obs"), alpha = 0.5)

# generate plots
newdat <- expand.grid(WP_MPa = seq(min(WP_MPa), max(WP_MPa), length = 50),
                      #Precip = unique(as.numeric(standard(dat$precip))))
                      Precip = unique(as.factor(dat$precip)))

xmat <- model.matrix(~Precip*WP_MPa, newdat)
fit = coefs %*% t(xmat)
newdat <- newdat %>% cbind(tidyMCMC(fit, conf.int = TRUE))

graphdat <- dat %>% mutate(estimate = (n_germinated/seeds_in_dish)) %>%
  rename(site = siteID)
graphdat$WP_MPa <- standard(graphdat$WP_MPa)                   
graphdat$Precip <- as.factor(dat$precip)
#graphdat$Precip <- as.numeric(standard(dat$precip))

Germ_percent_Ver_alp_main_plot <- ggplot()+ 
  geom_point(data=graphdat, aes(x=WP_MPa, y=estimate, colour = factor(Precip)),alpha=.15)+
  geom_ribbon(data=newdat, aes(ymin=invlogit(conf.low), ymax=invlogit(conf.high), x=WP_MPa, 
                               fill = factor(Precip)), alpha=0.35)+
  geom_line(data=newdat, aes(y = invlogit(estimate), x = WP_MPa, colour = factor(Precip)))+
  #facet_wrap(~Precip, nrow = 1)+
  scale_x_continuous("Standardized WP") + 
  scale_y_continuous("Germination %")+ 
  theme(panel.background = element_rect(fill='white', colour='black'))+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_colour_manual(values = Precip_palette)+
  scale_fill_manual(values = Precip_palette)

Germ_percent_Ver_alp_four_panels_plot <- ggplot()+ 
  geom_point(data=graphdat, aes(x=WP_MPa, y=estimate, colour = factor(Precip)),alpha=.15)+
  geom_ribbon(data=newdat, aes(ymin=invlogit(conf.low), ymax=invlogit(conf.high), x=WP_MPa, 
                               fill = factor(Precip)), alpha=0.35)+
  geom_line(data=newdat, aes(y = invlogit(estimate), x = WP_MPa, colour = factor(Precip)))+
  facet_wrap(~Precip, nrow = 1)+
  scale_x_continuous("Standardized WP") + 
  scale_y_continuous("Germination %")+ 
  theme(panel.background = element_rect(fill='white', colour='black'))+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_colour_manual(values = Precip_palette)+
  scale_fill_manual(values = Precip_palette)

Germ_percent_Ver_alp_main_plot /
  Germ_percent_Ver_alp_four_panels_plot + 
  plot_layout(heights = c(4, 1), guides = "collect") & 
  theme(legend.position='bottom', text = element_text(size = 15))


#### Germination percentage Sibbaldia procumbens ####

# take out the too many zer0s
guddat <- Sib_pro_germination_traits %>% filter (siteID == "GUD") %>% filter(water_potential < 6)
lavdat <- Sib_pro_germination_traits %>% filter (siteID == "LAV") %>% filter(water_potential < 10)
skjdat <- Sib_pro_germination_traits %>% filter (siteID == "SKJ") %>% filter(water_potential < 9)
ulvdat <- Sib_pro_germination_traits %>% filter (siteID == "ULV") %>% filter(water_potential < 10)

dat <- bind_rows(guddat, lavdat, skjdat, ulvdat)
dat <- dat %>% filter(!is.na(n_germinated))
# group level effects
#site <- factor(dat$siteID)
petridish <- factor(dat$petri_dish)

# independent variables
WP <- as.numeric(standard(as.numeric(dat$water_potential)))
WP_MPa <- as.numeric(standard(dat$WP_MPa))
#Precip <- as.numeric(standard(dat$precip))
Precip <- as.factor(dat$precip)

# dependent variables
GermN <- as.numeric(dat$n_germinated)
NumSeedDish <- as.numeric(dat$seeds_in_dish)
N <- as.numeric(length(GermN))

treatmat <- model.matrix(~Precip*WP_MPa)
n_parm <- as.numeric(ncol(treatmat))

# look at the data before the analysis
ggplot(dat, aes(x=water_potential,y = logit(n_germinated/seeds_in_dish,
                                                                   adjust = 0.01)))+
  geom_point()+facet_wrap(~siteID)

jags.data <- list("treatmat", "GermN", "N", "NumSeedDish", "n_parm")
jags.param <- c("b",  "Presi", "rss", "rss_new", "sig1") 

#Run the model 
results_Germ_percent_Sib_pro <- jags.parallel(data = jags.data,
                               #inits = inits.fn,
                               parameters.to.save = jags.param,
                               n.iter = 100000,
                               model.file = model_GermN,
                               n.thin = 5,
                               n.chains = 3,
                               n.burnin = 35000)
results_Germ_percent_Sib_pro

# traceplots
s <- ggs(as.mcmc(results_Germ_percent_Sib_pro))
ggs_traceplot(s, family="b") 

# check Gelman Rubin Statistics
gelman.diag(as.mcmc(results_Germ_percent_Sib_pro))

# Posterior predictive check
plot(results_Germ_percent_Sib_pro$BUGSoutput$sims.list$rss_new, results_Germ_percent_Sib_pro$BUGSoutput$sims.list$rss,
     main = "",)
abline(0,1, lwd = 2, col = "black")

mean(results_Germ_percent_Sib_pro$BUGSoutput$sims.list$rss_new > results_Germ_percent_Sib_pro$BUGSoutput$sims.list$rss)

## put together for figure  and r^2
mcmc <- results_Germ_percent_Sib_pro$BUGSoutput$sims.matrix 
coefs = mcmc[, c("b[1]", "b[2]", "b[3]", "b[4]", "b[5]", "b[6]", "b[7]", "b[8]")] # "b[5]", "b[6]", "b[7]", "b[8]"
fit = coefs %*% t(treatmat)
resid = sweep(fit, 2, logit(GermN/NumSeedDish, adjust = 0.01), "-")
var_f = apply(fit, 1, var)
var_e = apply(resid, 1, var)
R2 = var_f/(var_f + var_e)
tidyMCMC(as.mcmc(R2), conf.int = TRUE, conf.method = "HPDinterval")

#residuals
coefs2 = apply(coefs, 2, median)
fit2 = as.vector(coefs2 %*% t(treatmat))
resid2 <- (GermN/NumSeedDish) - invlogit(fit2)
sresid2 <- resid2/sd(resid2)
ggplot() + geom_point(data = NULL, aes(y = resid2, x = invlogit(fit2)))
hist(resid2)

# check predicted versus observed
yRep = sapply(1:nrow(mcmc), function(i) rbinom(nrow(dat), NumSeedDish[i], invlogit(fit[i,])))
ggplot() + geom_density(data = NULL, aes(x = (as.vector(yRep)/NumSeedDish),
                                         fill = "Model"), alpha = 0.5) + 
  geom_density(data = dat, aes(x = (GermN/NumSeedDish), fill = "Obs"), alpha = 0.5)

# generate plots
newdat <- expand.grid(WP_MPa = seq(min(WP_MPa), max(WP_MPa), length = 50),
                      #Precip = c(unique(as.numeric(standard(dat$precip)))))
                      Precip = c(unique(as.factor(standard(dat$precip)))))

xmat <- model.matrix(~Precip*WP_MPa, newdat)
fit = coefs %*% t(xmat)
newdat <- newdat %>% cbind(tidyMCMC(fit, conf.int = TRUE))

graphdat <- dat %>% mutate(estimate = (n_germinated/seeds_in_dish)) %>%
  rename(site = siteID)
graphdat$WP_MPa <- standard(graphdat$WP_MPa)                   
#graphdat$Precip <- as.numeric(standard((dat$precip)))
graphdat$Precip <- as.factor(standard((dat$precip)))

# ggplot()+ 
#   geom_point(data=graphdat, aes(x=WP_MPa, y=estimate, colour = factor(Precip)),alpha=.15)+
#   geom_ribbon(data=newdat, aes(ymin=invlogit(conf.low), ymax=invlogit(conf.high), x=WP_MPa, 
#                                fill = factor(Precip)), alpha=0.35)+
#   geom_line(data=newdat, aes(y = invlogit(estimate), x = WP_MPa, colour = factor(Precip)))+
#   #facet_wrap(~Precip)+
#   #scale_colour_manual("Treatment", values=c("gray", "red")) + 
#   #scale_fill_manual("Treatment", values=c("dark gray", "red")) + 
#   scale_x_continuous("Standardized WP") + 
#   scale_y_continuous("Germination %")+ 
#   # theme(axis.text.y = element_text(size=7,colour= "black"),
#   #       axis.text.x= element_text(size=7, colour="black"), 
#   #       axis.title=element_text(size=7),strip.text=element_text(size=5),
#   #       plot.title=element_text(size=7),
#   #       legend.title=element_text(size=5), legend.text=element_text(size=4),
#   #       legend.margin=margin(0,0,0,0),legend.position = c(0.2,0.3),
#   #       legend.box.margin=margin(-10,-2,-10,-5),legend.justification="left",
#   #       legend.key.size = unit(0.15, "cm"))+ #labs(title="Colonization probability")+
#   theme(panel.background = element_rect(fill='white', colour='black'))+
#   theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())

Germ_percent_Sib_pro_main_plot <- ggplot()+ 
  geom_point(data=graphdat, aes(x=WP_MPa, y=estimate, colour = factor(Precip)),alpha=.25)+
  geom_ribbon(data=newdat, aes(ymin=invlogit(conf.low), ymax=invlogit(conf.high), x=WP_MPa, 
                               fill = factor(Precip)), alpha=0.35)+
  geom_line(data=newdat, aes(y = invlogit(estimate), x = WP_MPa, colour = factor(Precip)))+
  #facet_wrap(~Precip, nrow = 1)+
  scale_x_continuous("Standardized WP") + 
  scale_y_continuous("Germination %")+ 
  theme(panel.background = element_rect(fill='white', colour='black'))+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_colour_manual(values = Precip_palette)+
  scale_fill_manual(values = Precip_palette)

Germ_percent_Sib_pro_four_panels_plot <- ggplot()+ 
  geom_point(data=graphdat, aes(x=WP_MPa, y=estimate, colour = factor(Precip)),alpha=.25)+
  geom_ribbon(data=newdat, aes(ymin=invlogit(conf.low), ymax=invlogit(conf.high), x=WP_MPa, 
                               fill = factor(Precip)), alpha=0.35)+
  geom_line(data=newdat, aes(y = invlogit(estimate), x = WP_MPa, colour = factor(Precip)))+
  facet_wrap(~Precip, nrow = 1)+
  scale_x_continuous("Standardized WP") + 
  scale_y_continuous("Germination %")+ 
  theme(panel.background = element_rect(fill='white', colour='black'))+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_colour_manual(values = Precip_palette)+
  scale_fill_manual(values = Precip_palette)

Germ_percent_Sib_pro_main_plot /
  Germ_percent_Sib_pro_four_panels_plot + 
  plot_layout(heights = c(4, 1), guides = "collect") & 
  theme(legend.position='bottom', text = element_text(size = 15))

#### Days to max germination Veronica alpina ####
# take out the too many zer0s
guddat <-  Ver_alp_germination_traits %>% filter (siteID == "GUD") %>% filter(water_potential < 8)
lavdat <- Ver_alp_germination_traits %>% filter (siteID == "LAV") %>% filter(water_potential < 9)
skjdat <- Ver_alp_germination_traits %>% filter (siteID == "SKJ") %>% filter(water_potential < 8)
ulvdat <- Ver_alp_germination_traits %>% filter (siteID == "ULV") 

dat <- bind_rows(guddat, lavdat, skjdat, ulvdat)
dat <- dat %>% filter(!is.na(days_to_max_germination)) %>% filter(water_potential<6) # Remove water potentials with less than 10 data points across all populations

# group level effects
#site <- factor(dat$siteID)
petridish <- factor(dat$petri_dish)

# independent variables
WP <- as.numeric(standard(as.numeric(dat$water_potential)))
WP_MPa <- as.numeric(standard(dat$WP_MPa))
#Precip <- as.numeric(standard(dat$precip))
Precip <- as.factor(dat$precip)

# dependent variables
DtoM <- as.numeric(dat$days_to_max_germination)
N <- as.numeric(length(DtoM))

treatmat <- model.matrix(~Precip*WP_MPa)
n_parm <- as.numeric(ncol(treatmat))

# look at the data before the analysis
ggplot(dat, aes(x=water_potential, y = days_to_max_germination), adjust = 0.01)+
  geom_point()+facet_wrap(~siteID)

jags.data <- list("treatmat", "DtoM", "N", "n_parm")
jags.param <- c("b", "rss", "rss_new", "r") 

model_DtoM<- function(){
  #group effects
  #for (j in 1:4){lokaliteter[j]~dnorm(0, prec1)}
  #likelihood
  for (i in 1:N){
    DtoM[i] ~ dnegbin(p[i], r)
    # linear predictor
    log(mu[i]) <- inprod(b, treatmat[i,]) #+lokaliteter[site[i]]
    p[i] <- r/(r+mu[i])
    
    # residual sum of squares
    res[i] <- pow(DtoM[i] - mu[i], 2)
    DtoM_new[i] ~ dnegbin(p[i], r)
    res_new[i] <- pow(DtoM_new[i] - mu[i], 2)
  }
  
  for(i in 1:n_parm){b[i] ~ dnorm(0,1.0E-6)} #dnorm in JAGS uses mean and precision (0 = mean and 1.0E-6 = precision) different from dnorm in R that has variance and not precision.
  prec1 ~ dgamma(0.001, 0.001) 
  #sig1 <- 1/sqrt(prec1) #getting variance of the random effect
  r~ dunif(0,50)
  # #derived params
  rss <- sum(res[])
  rss_new <- sum(res_new[])
}

results_DtoM_VA <- jags.parallel(data = jags.data,
                               #inits = inits.fn,
                               parameters.to.save = jags.param,
                               n.iter = 100000,
                               model.file = model_DtoM,
                               n.thin = 5,
                               n.chains = 3,
                               n.burnin = 35000)
results_DtoM_VA

# traceplots
s <- ggs(as.mcmc(results_DtoM_VA))
ggs_traceplot(s, family="b") 

# check Gelman Rubin Statistics
gelman.diag(as.mcmc(results_DtoM_VA))

# Posterior predictive check
plot(results_DtoM_VA$BUGSoutput$sims.list$rss_new, results_DtoM_VA$BUGSoutput$sims.list$rss,
     main = "",)
abline(0,1, lwd = 2, col = "black")

mean(results_DtoM_VA$BUGSoutput$sims.list$rss_new > results_DtoM_VA$BUGSoutput$sims.list$rss)

## put together for figure  and r^2
mcmc <- results_DtoM_VA$BUGSoutput$sims.matrix
coefs = mcmc[, c("b[1]", "b[2]", "b[3]", "b[4]", "b[5]", "b[6]", "b[7]", "b[8]")]
fit = coefs %*% t(treatmat)
resid = sweep(fit, 2, log(DtoM), "-")
var_f = apply(fit, 1, var)
var_e = apply(resid, 1, var)
R2 = var_f/(var_f + var_e)
tidyMCMC(as.mcmc(R2), conf.int = TRUE, conf.method = "HPDinterval")

#residuals
coefs2 = apply(coefs, 2, median)
fit2 = as.vector(coefs2 %*% t(treatmat))
resid2 <- log(DtoM) - (fit2)
sresid2 <- resid2/sd(resid2)
ggplot() + geom_point(data = NULL, aes(y = resid2, x = (fit2)))
hist(resid2)

# check predicted versus observed
yRep = sapply(1:nrow(mcmc), function(i) rpois(nrow(dat), exp(fit[i,])))
ggplot() + geom_density(data = NULL, aes(x = (as.vector(yRep)),
                                         fill = "Model"), alpha = 0.5) + 
  geom_density(data = dat, aes(x = (DtoM), fill = "Obs"), alpha = 0.5)

# generate plots
newdat <- expand.grid(WP_MPa = seq(min(WP_MPa), max(WP_MPa), length = 50),
                      #Precip = c(unique(standard(dat$precip))))
                      Precip = unique(as.factor(dat$precip)))

xmat <- model.matrix(~Precip*WP_MPa, newdat)
fit = coefs %*% t(xmat)
newdat <- newdat %>% cbind(tidyMCMC(fit, conf.int = TRUE))

graphdat <- dat %>% mutate(estimate = days_to_max_germination) %>%
  rename(site = siteID)
graphdat$WP_MPa <- standard(graphdat$WP_MPa)                   
graphdat$Precip <- as.factor(dat$precip)

ggplot()+ 
  geom_point(data=graphdat, aes(x=WP_MPa, y=estimate, colour = factor(Precip)),alpha=.15)+
  geom_ribbon(data=newdat, aes(ymin=exp(conf.low), ymax=exp(conf.high), x=WP_MPa, 
                               fill = factor(Precip)), alpha=0.35)+
  geom_line(data=newdat, aes(y = exp(estimate), x = WP_MPa, colour = factor(Precip)))+
  #facet_wrap(~Precip)+
  #scale_colour_manual("Treatment", values=c("gray", "red")) + 
  #scale_fill_manual("Treatment", values=c("dark gray", "red")) + 
  scale_x_continuous("Standardized WP") + 
  scale_y_continuous("Days to max germination")+ 
  # theme(axis.text.y = element_text(size=7,colour= "black"),
  #       axis.text.x= element_text(size=7, colour="black"), 
  #       axis.title=element_text(size=7),strip.text=element_text(size=5),
  #       plot.title=element_text(size=7),
  #       legend.title=element_text(size=5), legend.text=element_text(size=4),
  #       legend.margin=margin(0,0,0,0),legend.position = c(0.2,0.3),
  #       legend.box.margin=margin(-10,-2,-10,-5),legend.justification="left",
  #       legend.key.size = unit(0.15, "cm"))+ #labs(title="Colonization probability")+
  theme(panel.background = element_rect(fill='white', colour='black'))+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_colour_manual(values = Precip_palette) +
  scale_fill_manual(values = Precip_palette) +
  theme(text = element_text(size = 15))


#### Days to max germination Sibbaldia procumbens ####
# Sib.pro - days to germination
# take out the too many zer0s
guddat <- Sib_pro_germination_traits %>% filter (siteID == "GUD") %>% filter(water_potential < 6)
lavdat <- Sib_pro_germination_traits %>% filter (siteID == "LAV") %>% filter(water_potential < 9)
skjdat <- Sib_pro_germination_traits %>% filter (siteID == "SKJ") %>% filter(water_potential < 8)
ulvdat <- Sib_pro_germination_traits %>% filter (siteID == "ULV") %>% filter(water_potential < 9)

dat <- bind_rows(guddat, lavdat, skjdat, ulvdat)
dat <- dat %>% filter(!is.na(days_to_max_germination)) %>% filter(water_potential<6)
# group level effects
#site <- factor(dat$siteID)
#petridish <- factor(dat$petri_dish)

# independent variables
#WP <- as.numeric(standard(as.numeric(dat$water_potential)))
WP_MPa <- as.numeric(standard(dat$WP_MPa))
#Precip <- as.numeric(standard(dat$precip))
Precip <- as.factor(dat$precip)

# dependent variables
DtoM <- as.numeric(dat$days_to_max_germination)
N <- as.numeric(length(DtoM))

treatmat <- model.matrix(~Precip*WP_MPa)
n_parm <- as.numeric(ncol(treatmat))

# look at the data before the analysis
ggplot(dat, aes(x=water_potential, y = days_to_max_germination), adjust = 0.01)+
  geom_point()+facet_wrap(~siteID)

jags.data <- list("treatmat", "DtoM", "N", "n_parm")
jags.param <- c("b", "rss", "rss_new", "r") 

results_DtoM_SP <- jags.parallel(data = jags.data,
                              #inits = inits.fn,
                              parameters.to.save = jags.param,
                              n.iter = 10000,
                              model.file = model_DtoM,
                              n.thin = 5,
                              n.chains = 3,
                              n.burnin = 5000)
results_DtoM_SP

# traceplots
s <- ggs(as.mcmc(results_DtoM_SP))
ggs_traceplot(s, family="b") 

# check Gelman Rubin Statistics
gelman.diag(as.mcmc(results_DtoM_SP))

# Posterior predictive check
plot(results_DtoM_SP$BUGSoutput$sims.list$rss_new, results_DtoM_SP$BUGSoutput$sims.list$rss,
     main = "",)
abline(0,1, lwd = 2, col = "black")

mean(results_DtoM_SP$BUGSoutput$sims.list$rss_new > results_DtoM_SP$BUGSoutput$sims.list$rss)

## put together for figure  and r^2
mcmc <- results_DtoM_SP$BUGSoutput$sims.matrix
coefs = mcmc[, c("b[1]", "b[2]", "b[3]", "b[4]", "b[5]", "b[6]", "b[7]", "b[8]")]
fit = coefs %*% t(treatmat)
resid = sweep(fit, 2, log(DtoM), "-")
var_f = apply(fit, 1, var)
var_e = apply(resid, 1, var)
R2 = var_f/(var_f + var_e)
tidyMCMC(as.mcmc(R2), conf.int = TRUE, conf.method = "HPDinterval")

#residuals
coefs2 = apply(coefs, 2, median)
fit2 = as.vector(coefs2 %*% t(treatmat))
resid2 <- log(DtoM) - (fit2)
sresid2 <- resid2/sd(resid2)
ggplot() + geom_point(data = NULL, aes(y = resid2, x = fit2))
hist(resid2)

# check predicted versus observed
yRep = sapply(1:nrow(mcmc), function(i) rpois(nrow(dat), exp(fit[i,])))
ggplot() + geom_density(data = NULL, aes(x = (as.vector(yRep)),
                                         fill = "Model"), alpha = 0.5) + 
  geom_density(data = dat, aes(x = (DtoM), fill = "Obs"), alpha = 0.5)

# generate plots
newdat <- expand.grid(WP_MPa = seq(min(WP_MPa), max(WP_MPa), length = 50),
                      #Precip = c(unique(standard(dat$precip))))
                      Precip = as.factor(dat$precip))

xmat <- model.matrix(~Precip*WP_MPa, newdat)
fit = coefs %*% t(xmat)
newdat <- newdat %>% cbind(tidyMCMC(fit, conf.int = TRUE))

graphdat <- dat %>% mutate(estimate = days_to_max_germination) %>%
  rename(site = siteID)
graphdat$WP_MPa <- standard(graphdat$WP_MPa)                   
#graphdat$Precip <- as.numeric(standard(dat$precip))                   
graphdat$Precip <- as.factor(dat$precip)

ggplot()+ 
  geom_point(data=graphdat, aes(x=WP_MPa, y=estimate, colour = factor(Precip)))+
  geom_ribbon(data=newdat, aes(ymin=exp(conf.low), ymax=exp(conf.high), x=WP_MPa, 
                               fill = factor(Precip)), alpha=0.35)+
  geom_line(data=newdat, aes(y = exp(estimate), x = WP_MPa, colour = factor(Precip)))+
  #facet_wrap(~Precip, nrow = 1)+
  #scale_colour_manual("Treatment", values=c("gray", "red")) + 
  #scale_fill_manual("Treatment", values=c("dark gray", "red")) + 
  scale_x_continuous("Standardized WP") + 
  scale_y_continuous("Days to max germination")+ 
  # theme(axis.text.y = element_text(size=7,colour= "black"),
  #       axis.text.x= element_text(size=7, colour="black"), 
  #       axis.title=element_text(size=7),strip.text=element_text(size=5),
  #       plot.title=element_text(size=7),
  #       legend.title=element_text(size=5), legend.text=element_text(size=4),
  #       legend.margin=margin(0,0,0,0),legend.position = c(0.2,0.3),
  #       legend.box.margin=margin(-10,-2,-10,-5),legend.justification="left",
  #       legend.key.size = unit(0.15, "cm"))+ #labs(title="Colonization probability")+
  theme(panel.background = element_rect(fill='white', colour='black'))+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  scale_colour_manual(values = Precip_palette)+
  scale_fill_manual(values = Precip_palette) +
  theme(text = element_text(size = 15))


#### T50 Sibbaldia procumbens ####
Sib_pro_germination_traits %>% 
  ggplot(aes(x = as.factor(water_potential), y = T50, fill = as.factor(water_potential))) +
  geom_violin()+
  geom_jitter() #+
  #facet_wrap(~siteID)

# Sib.pro 
# take out the water potentials with less than 10 data points
guddat <- Sib_pro_germination_traits %>% filter (siteID == "GUD") %>% filter(water_potential < 6)
lavdat <- Sib_pro_germination_traits %>% filter (siteID == "LAV") %>% filter(water_potential < 6)
skjdat <- Sib_pro_germination_traits %>% filter (siteID == "SKJ") %>% filter(water_potential < 6)
ulvdat <- Sib_pro_germination_traits %>% filter (siteID == "ULV") %>% filter(water_potential < 6)

dat <- bind_rows(guddat, lavdat, skjdat, ulvdat)
dat <- dat %>% filter(!is.na(T50)) 
# group level effects
#site <- factor(dat$siteID)
#petridish <- factor(dat$petri_dish)

# independent variables
WP <- as.numeric(standard(as.numeric(dat$water_potential)))
WP_MPa <- as.numeric(standard(dat$WP_MPa))
Precip <- as.factor(dat$precip)
#Precip <- as.numeric(standard(dat$precip))

# dependent variables
T50 <- as.numeric(dat$T50)
N <- as.numeric(length(T50))

treatmat <- model.matrix(~Precip*WP_MPa)
n_parm <- as.numeric(ncol(treatmat))

# look at the data before the analysis
ggplot(dat, aes(x=water_potential, y = T50), adjust = 0.01)+
  geom_point()+facet_wrap(~siteID)

jags.data <- list("treatmat", "T50", "N",  "n_parm")
jags.param <- c("b", "rss", "rss_new", "r", "sig1") 


model_T50 <- function(){
  #group effects
  #for (j in 1:4){lokaliteter[j]~dnorm(0, prec1)}
  #likelihood
  for (i in 1:N){
    T50[i] ~ dnegbin(p[i], r)
    # linear predictor
    log(mu[i]) <- inprod(b, treatmat[i,]) #+ lokaliteter[site[i]]
    p[i] <- r/(r+mu[i])
    
    # residual sum of squares
    res[i] <- pow(T50[i] - mu[i], 2)
    T50_new[i] ~ dnegbin(p[i], r)
    res_new[i] <- pow(T50_new[i] - mu[i], 2)
  }
  
  for(i in 1:n_parm){b[i] ~ dnorm(0,1.0E-6)} #dnorm in JAGS uses mean and precision (0 = mean and 1.0E-6 = precision) different from dnorm in R that has variance and not precision.
  prec1 ~ dgamma(0.001, 0.001) 
  #sig1 <- 1/sqrt(prec1) #getting variance of the random effect
  r~ dunif(0,50)
  # #derived params
  rss <- sum(res[])
  rss_new <- sum(res_new[])
}

results_T50_SP <- jags.parallel(data = jags.data,
                              #inits = inits.fn,
                              parameters.to.save = jags.param,
                              n.iter = 10000,
                              model.file = model_T50,
                              n.thin = 5,
                              n.chains = 3,
                              n.burnin = 3000)
results_T50_SP

# traceplots
s <- ggs(as.mcmc(results_T50_SP))
ggs_traceplot(s, family="b") 

# check Gelman Rubin Statistics
gelman.diag(as.mcmc(results_T50_SP))

# Posterior predictive check
plot(results_T50_SP$BUGSoutput$sims.list$rss_new, results_T50_SP$BUGSoutput$sims.list$rss,
     main = "",)
abline(0,1, lwd = 2, col = "black") # these are still not great but not sure what to do 

mean(results_T50_SP$BUGSoutput$sims.list$rss_new > results_T50_SP$BUGSoutput$sims.list$rss)

## put together for figure  and r^2
mcmc <- results_T50_SP$BUGSoutput$sims.matrix
coefs = mcmc[, c("b[1]", "b[2]", "b[3]", "b[4]", "b[5]", "b[6]", "b[7]", "b[8]")]
fit = coefs %*% t(treatmat)
resid = sweep(fit, 2, log(T50), "-")
var_f = apply(fit, 1, var)
var_e = apply(resid, 1, var)
R2 = var_f/(var_f + var_e)
tidyMCMC(as.mcmc(R2), conf.int = TRUE, conf.method = "HPDinterval")

#residuals
coefs2 = apply(coefs, 2, median)
fit2 = as.vector(coefs2 %*% t(treatmat))
resid2 <- log(T50) - (fit2)
sresid2 <- resid2/sd(resid2)
ggplot() + geom_point(data = NULL, aes(y = resid2, x = (fit2)))
hist(resid2)


# check predicted versus observed
yRep = sapply(1:nrow(mcmc), function(i) rpois(nrow(dat), exp(fit[i,])))
ggplot() + geom_density(data = NULL, aes(x = (as.vector(yRep)),
                                         fill = "Model"), alpha = 0.5) + 
  geom_density(data = dat, aes(x = (T50), fill = "Obs"), alpha = 0.5)

# generate plots
newdat <- expand.grid(WP_MPa = seq(min(WP_MPa), max(WP_MPa), length = 50),
                      Precip = c(unique(as.factor(dat$precip))))

xmat <- model.matrix(~Precip*WP_MPa, newdat)
fit = coefs %*% t(xmat)
newdat <- newdat %>% cbind(tidyMCMC(fit, conf.int = TRUE))

graphdat <- dat %>% mutate(estimate = T50) %>%
  rename(site = siteID)
graphdat$WP_MPa <- standard(graphdat$WP_MPa)                   
graphdat$Precip <- as.factor(dat$precip)

T50_SP_full_plot <- ggplot()+ 
  geom_point(data=graphdat, aes(x=WP_MPa, y=estimate, colour = factor(Precip)))+
  geom_ribbon(data=newdat, aes(ymin=exp(conf.low), ymax=exp(conf.high), x=WP_MPa, 
                               fill = factor(Precip)), alpha=0.35)+
  geom_line(data=newdat, aes(y = exp(estimate), x = WP_MPa, colour = factor(Precip)))+
  #facet_wrap(~Precip)+
  scale_x_continuous("Standardized WP") + 
  scale_y_continuous("Time to 50 % germination")+ 
  theme(panel.background = element_rect(fill='white', colour='black'))+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  scale_colour_manual(values = Precip_palette)+
  scale_fill_manual(values = Precip_palette)

T50_SP_panel_plot <- ggplot()+ 
  geom_point(data=graphdat, aes(x=WP_MPa, y=estimate, colour = factor(Precip)))+
  geom_ribbon(data=newdat, aes(ymin=exp(conf.low), ymax=exp(conf.high), x=WP_MPa, 
                               fill = factor(Precip)), alpha=0.35)+
  geom_line(data=newdat, aes(y = exp(estimate), x = WP_MPa, colour = factor(Precip)))+
  facet_wrap(~Precip, nrow = 1)+
  scale_x_continuous("Standardized WP") + 
  scale_y_continuous("Time to 50 % germination")+ 
  theme(panel.background = element_rect(fill='white', colour='black'))+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  scale_colour_manual(values = Precip_palette)+
  scale_fill_manual(values = Precip_palette)

T50_SP_full_plot /
  T50_SP_panel_plot + 
  plot_layout(heights = c(4, 1), guides = "collect") & 
  theme(legend.position='bottom', text = element_text(size = 15))

#### T50 Veronica alpina ####
Ver_alp_germination_traits %>% 
  filter(water_potential < 6) %>% 
  ggplot(aes(x = as.factor(water_potential), y = T50, fill = as.factor(water_potential))) +
  geom_violin()+
  geom_jitter() +
facet_wrap(~siteID)

# Veronica alpina
# take out the water potentials with less than 10 data points

dat <- Ver_alp_germination_traits %>% filter(!is.na(T50)) %>% filter(water_potential < 6)
# group level effects
#site <- factor(dat$siteID)
#petridish <- factor(dat$petri_dish)

# independent variables
WP <- as.numeric(standard(as.numeric(dat$water_potential)))
WP_MPa <- as.numeric(standard(dat$WP_MPa))
Precip <- as.factor(dat$precip)
#Precip <- as.numeric(standard(dat$precip))

# dependent variables
T50 <- as.numeric(dat$T50)
N <- as.numeric(length(T50))

treatmat <- model.matrix(~Precip*WP_MPa)
n_parm <- as.numeric(ncol(treatmat))

# look at the data before the analysis
ggplot(dat, aes(x=water_potential, y = T50), adjust = 0.01)+
  geom_point()+facet_wrap(~siteID)

jags.data <- list("treatmat", "T50", "N",  "n_parm")
jags.param <- c("b", "rss", "rss_new", "r", "sig1") 

results_T50_VA <- jags.parallel(data = jags.data,
                                #inits = inits.fn,
                                parameters.to.save = jags.param,
                                n.iter = 10000,
                                model.file = model_T50,
                                n.thin = 5,
                                n.chains = 3,
                                n.burnin = 3000)
results_T50_VA

# traceplots
s <- ggs(as.mcmc(results_T50_VA))
ggs_traceplot(s, family="b") 

# check Gelman Rubin Statistics
gelman.diag(as.mcmc(results_T50_VA))

# Posterior predictive check
plot(results_T50_VA$BUGSoutput$sims.list$rss_new, results_T50_VA$BUGSoutput$sims.list$rss,
     main = "",)
abline(0,1, lwd = 2, col = "black") # these are still not great but not sure what to do 

mean(results_T50_VA$BUGSoutput$sims.list$rss_new > results_T50_VA$BUGSoutput$sims.list$rss)

## put together for figure  and r^2
mcmc <- results_T50_VA$BUGSoutput$sims.matrix
coefs = mcmc[, c("b[1]", "b[2]", "b[3]", "b[4]", "b[5]", "b[6]", "b[7]", "b[8]")]
fit = coefs %*% t(treatmat)
resid = sweep(fit, 2, log(T50), "-")
var_f = apply(fit, 1, var)
var_e = apply(resid, 1, var)
R2 = var_f/(var_f + var_e)
tidyMCMC(as.mcmc(R2), conf.int = TRUE, conf.method = "HPDinterval")

#residuals
coefs2 = apply(coefs, 2, median)
fit2 = as.vector(coefs2 %*% t(treatmat))
resid2 <- log(T50) - (fit2)
sresid2 <- resid2/sd(resid2)
ggplot() + geom_point(data = NULL, aes(y = resid2, x = (fit2)))
hist(resid2)

# check predicted versus observed
yRep = sapply(1:nrow(mcmc), function(i) rpois(nrow(dat), exp(fit[i,])))
ggplot() + geom_density(data = NULL, aes(x = (as.vector(yRep)),
                                         fill = "Model"), alpha = 0.5) + 
  geom_density(data = dat, aes(x = (T50), fill = "Obs"), alpha = 0.5)


# generate plots
newdat <- expand.grid(WP_MPa = seq(min(WP_MPa), max(WP_MPa), length = 50),
                      Precip = c(unique(as.factor(dat$precip))))

xmat <- model.matrix(~Precip*WP_MPa, newdat)
fit = coefs %*% t(xmat)
newdat <- newdat %>% cbind(tidyMCMC(fit, conf.int = TRUE))

graphdat <- dat %>% mutate(estimate = T50) %>%
  rename(site = siteID)
graphdat$WP_MPa <- standard(graphdat$WP_MPa)                   
graphdat$Precip <- as.factor(dat$precip)

T50_VA_full_plot <- ggplot()+ 
  geom_point(data=graphdat, aes(x=WP_MPa, y=estimate, colour = factor(Precip)))+
  geom_ribbon(data=newdat, aes(ymin=exp(conf.low), ymax=exp(conf.high), x=WP_MPa, 
                               fill = factor(Precip)), alpha=0.35)+
  geom_line(data=newdat, aes(y = exp(estimate), x = WP_MPa, colour = factor(Precip)))+
  #facet_wrap(~Precip)+
  scale_x_continuous("Standardized WP") + 
  scale_y_continuous("Time to 50 % germination")+ 
  theme(panel.background = element_rect(fill='white', colour='black'))+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  scale_colour_manual(values = Precip_palette)+
  scale_fill_manual(values = Precip_palette)

T50_VA_panel_plot <- ggplot()+ 
  geom_point(data=graphdat, aes(x=WP_MPa, y=estimate, colour = factor(Precip)))+
  geom_ribbon(data=newdat, aes(ymin=exp(conf.low), ymax=exp(conf.high), x=WP_MPa, 
                               fill = factor(Precip)), alpha=0.35)+
  geom_line(data=newdat, aes(y = exp(estimate), x = WP_MPa, colour = factor(Precip)))+
  facet_wrap(~Precip, nrow = 1)+
  scale_x_continuous("Standardized WP") + 
  scale_y_continuous("Time to 50 % germination")+ 
  theme(panel.background = element_rect(fill='white', colour='black'))+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  scale_colour_manual(values = Precip_palette)+
  scale_fill_manual(values = Precip_palette)

T50_VA_full_plot /
  T50_VA_panel_plot + 
  plot_layout(heights = c(4, 1), guides = "collect") & 
  theme(legend.position='bottom', text = element_text(size = 15))



##### Seedlings Veronica alpina #####
seedlings_VA <- Ver_alp_germ %>% 
  select(unique_ID, species, siteID, water_potential, replicate, seed_nr, dry_mass_g_root, dry_mass_g_above_ground, dry_mass_g_total, petri_dish) %>% 
  mutate(dry_mass_g_root = dry_mass_g_root + 7.0e-06,
         dry_mass_g_above_ground = dry_mass_g_above_ground + 7.0e-06,
         dry_mass_g_total = dry_mass_g_total + 7.0e-06) %>% 
  mutate(root_shoot_ratio = dry_mass_g_root/dry_mass_g_above_ground) %>% 
  filter(!is.na(dry_mass_g_above_ground)) %>% 
  mutate(precip = case_when(siteID == "SKJ" ~ 3402,
                            siteID == "GUD" ~ 2130,
                            siteID == "LAV" ~ 1561,
                            siteID == "ULV" ~ 1226)) %>% 
  mutate(precip = precip/1000) %>% 
  mutate(WP_MPa = case_when(water_potential == 1 ~ -0.25,
                            water_potential == 2 ~ -0.33,
                            water_potential == 3 ~ -0.42,
                            water_potential == 4 ~ -0.50,
                            water_potential == 5 ~ -0.57,
                            water_potential == 6 ~ -0.70,
                            water_potential == 7 ~ -0.95,
                            water_potential == 8 ~ -1.20,
                            water_potential == 9 ~ -1.45,
                            water_potential == 10 ~ -1.70)) %>% 
  mutate(water_potential = as.numeric(water_potential))


  
#### Root:shoot ratio Veronica alpina ####

seedlings_VA %>% 
  #filter(water_potential < 6) %>% 
  ggplot(aes(x = as.factor(water_potential), y = root_shoot_ratio, fill = as.factor(water_potential))) +
  geom_violin()+
  geom_jitter()

dat_root_shoot <- seedlings_VA %>% filter(!is.na(root_shoot_ratio)) %>% filter(water_potential < 6)
  # group level effects
#site <- factor(dat_root_shoot$siteID)
petridish <- factor(dat_root_shoot$petri_dish)
n_petri <- length(levels(petridish))
  
# independent variables
WP <- as.numeric(standard(as.numeric(dat_root_shoot$water_potential)))
WP_MPa <- as.numeric(standard(dat_root_shoot$WP_MPa))
Precip <- as.factor(dat_root_shoot$precip)
  
# dependent variables
traits <- as.numeric(log(dat_root_shoot$root_shoot_ratio))
N <- as.numeric(length(traits))
  
treatmat <- model.matrix(~Precip*WP_MPa)
n_parm <- as.numeric(ncol(treatmat))
  
# look at the data before the analysis
ggplot(dat_root_shoot, aes(x=water_potential, y = log(root_shoot_ratio)), adjust = 0.01)+
    geom_jitter()+facet_wrap(~siteID)
  
jags.data <- list("treatmat", "traits", "N",  "petridish", "n_petri", "n_parm")
jags.param <- c("b", "rss", "rss_new", "r", "sig1", "sig_t") 
  
  
model_traits<- function(){
    #group effects
    for (j in 1:n_petri){dish[j]~dnorm(0, prec1)}
    #likelihood
    for (i in 1:N){
      traits[i] ~ dnorm(mu[i], prec_t)
      # linear predictor
      mu[i] <- inprod(b, treatmat[i,]) + dish[petridish[i]]
      
      # residual sum of squares
      res[i] <- pow(traits[i] - mu[i], 2)
      traits_new[i] ~ dnorm(mu[i], prec_t)
      res_new[i] <- pow(traits_new[i] - mu[i], 2)
    }
    
    for(i in 1:n_parm){b[i] ~ dnorm(0,1.0E-6)} #dnorm in JAGS uses mean and precision (0 = mean and 1.0E-6 = precision) different from dnorm in R that has variance and not precision.
    prec1 ~ dgamma(0.001, 0.001) 
    sig1 <- 1/sqrt(prec1) #getting variance of the random effect
    prec_t ~ dgamma(0.001, 0.001) 
    sig_t <- 1/sqrt(prec_t) #getting variance of the trait
    # #derived params
    rss <- sum(res[])
    rss_new <- sum(res_new[])
  }
  
  
results_root_shoot_VA <- jags.parallel(data = jags.data,
                                #inits = inits.fn,
                                parameters.to.save = jags.param,
                                n.iter = 5000,
                                model.file = model_traits,
                                n.thin = 5,
                                n.chains = 3)
results_root_shoot_VA
  
# traceplots
s <- ggs(as.mcmc(results_root_shoot_VA))
ggs_traceplot(s, family="b") 
  
# check Gelman Rubin Statistics
gelman.diag(as.mcmc(results_root_shoot_VA))
  
# Posterior predictive check
plot(results_root_shoot_VA$BUGSoutput$sims.list$rss_new, results_root_shoot_VA$BUGSoutput$sims.list$rss,
       main = "",)
abline(0,1, lwd = 2, col = "black") 
  
mean(results_root_shoot_VA$BUGSoutput$sims.list$rss_new > results_root_shoot_VA$BUGSoutput$sims.list$rss)
  
  ## put together for figure  and r^2
mcmc <- results_root_shoot_VA$BUGSoutput$sims.matrix
coefs = mcmc[, c("b[1]", "b[2]", "b[3]", "b[4]", "b[5]", "b[6]", "b[7]", "b[8]")]
fit = coefs %*% t(treatmat)
resid = sweep(fit, 2, traits, "-")
var_f = apply(fit, 1, var)
var_e = apply(resid, 1, var)
R2 = var_f/(var_f + var_e)
tidyMCMC(as.mcmc(R2), conf.int = TRUE, conf.method = "HPDinterval")
  
#residuals
coefs2 = apply(coefs, 2, median)
fit2 = as.vector(coefs2 %*% t(treatmat))
resid2 <- traits - fit2
sresid2 <- resid2/sd(resid2)
ggplot() + geom_point(data = NULL, aes(y = resid2, x = (fit2)))
hist(resid2)
  
# check predicted versus observed
yRep = sapply(1:nrow(mcmc), function(i) rnorm(nrow(dat_root_shoot), fit[i,]))
ggplot() + geom_density(data = NULL, aes(x = (as.vector(yRep)),
                                           fill = "Model"), alpha = 0.5) + 
    geom_density(data = dat_root_shoot, aes(x = (traits), fill = "Obs"), alpha = 0.5)
  
# generate plots
newdat <- expand.grid(WP_MPa = seq(min(WP_MPa), max(WP_MPa), length = 50),
                        Precip = c(unique(as.factor(dat$precip))))
  
xmat <- model.matrix(~Precip*WP_MPa, newdat)
fit = coefs %*% t(xmat)
newdat <- newdat %>% cbind(tidyMCMC(fit, conf.int = TRUE))
  
graphdat <- dat_root_shoot 
graphdat$WP_MPa <- standard(graphdat$WP_MPa)                   
graphdat$Precip <- as.factor(dat_root_shoot$precip)
  
root_shoot_VA_full_plot <- ggplot()+ 
    geom_point(data=graphdat, aes(x=WP_MPa, y=root_shoot_ratio, colour = factor(Precip)))+
    geom_ribbon(data=newdat, aes(ymin=exp(conf.low), ymax=exp(conf.high), x=WP_MPa, 
                                 fill = factor(Precip), alpha=0.35))+
    geom_line(data=newdat, aes(y = exp(estimate), x = WP_MPa, colour = factor(Precip)))+
    xlab("Standardized WP") + 
    ylab("Root:shoot ratio")+ 
    theme(panel.background = element_rect(fill='white', colour='black'))+
    theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
     scale_colour_manual(values = Precip_palette)+
     scale_fill_manual(values = Precip_palette)
  
root_shoot_VA_zoomed_in <- ggplot()+ 
  geom_jitter(data=graphdat, aes(x=WP_MPa, y=root_shoot_ratio, colour = factor(Precip)), width = 0.1)+
  geom_ribbon(data=newdat, aes(ymin=exp(conf.low), ymax=exp(conf.high), x=WP_MPa, 
                               fill = factor(Precip), alpha=0.35))+
  geom_line(data=newdat, aes(y = exp(estimate), x = WP_MPa, colour = factor(Precip)))+
  xlab("Standardized WP") + 
  ylab("Root:shoot ratio")+ 
  theme(panel.background = element_rect(fill='white', colour='black'))+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_colour_manual(values = Precip_palette)+
  scale_fill_manual(values = Precip_palette) +
  ylim(0,7.5)

root_shoot_VA_panels <- ggplot()+ 
  geom_jitter(data=graphdat, aes(x=WP_MPa, y=root_shoot_ratio, colour = factor(Precip)), width = 0.1)+
  geom_ribbon(data=newdat, aes(ymin=exp(conf.low), ymax=exp(conf.high), x=WP_MPa, 
                               fill = factor(Precip), alpha=0.35))+
  geom_line(data=newdat, aes(y = exp(estimate), x = WP_MPa, colour = factor(Precip)))+
  xlab("Standardized WP") + 
  ylab("Root:shoot ratio")+ 
  theme(panel.background = element_rect(fill='white', colour='black'))+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_colour_manual(values = Precip_palette)+
  scale_fill_manual(values = Precip_palette) +
  ylim(0,7.5) +
  facet_wrap(~factor(Precip), nrow = 1)
  
  
 (root_shoot_VA_zoomed_in + inset_element(
    root_shoot_VA_full_plot, 
    left = 0.7, 
    bottom = 0.5, 
    right = 0.99, 
    top = 0.99
  )) /
    root_shoot_VA_panels +
    plot_layout(heights = c(4, 1), guides = "collect") & 
    theme(legend.position='bottom', text = element_text(size = 15))
  
#### Root biomass Veronica alpina ####

dat_root <- seedlings_VA %>% filter(!is.na(dry_mass_g_root)) 
  # group level effects
site <- factor(dat_root$siteID)
petridish <- factor(dat_root$petri_dish)
  
  # independent variables
WP <- as.numeric(standard(as.numeric(dat_root$water_potential)))
WP_MPa <- as.numeric(standard(dat_root$WP_MPa))
Precip <- as.numeric(standard(dat_root$precip))
  
  # dependent variables
traits <- as.numeric(log(dat_root$dry_mass_g_root))
N <- as.numeric(length(traits))
  
treatmat <- model.matrix(~Precip*WP_MPa)
n_parm <- as.numeric(ncol(treatmat))
  
  # look at the data before the analysis
ggplot(dat, aes(x=water_potential, y = log(dry_mass_g_root)), adjust = 0.01) +
  geom_jitter()+facet_wrap(~siteID)
  
jags.data <- list("treatmat", "traits", "N",  "site", "n_parm")
jags.param <- c("b", "rss", "rss_new", "r", "sig1", "sig_t") 
  
results_root_VA <- jags.parallel(data = jags.data,
                                         #inits = inits.fn,
                                         parameters.to.save = jags.param,
                                         n.iter = 10000,
                                         model.file = model_traits,
                                         n.thin = 5,
                                         n.chains = 3,
                                 n.burnin = 5000)
results_root_VA
  
  # traceplots
s <- ggs(as.mcmc(results_root_VA))
ggs_traceplot(s, family="b") 
  
  # check Gelman Rubin Statistics
gelman.diag(as.mcmc(results_root_VA))
  
  # Posterior predictive check
plot(results_root_VA$BUGSoutput$sims.list$rss_new, results_root_VA$BUGSoutput$sims.list$rss,
       main = "",)
abline(0,1, lwd = 2, col = "black") 
  
mean(results_root_VA$BUGSoutput$sims.list$rss_new > results_root_VA$BUGSoutput$sims.list$rss)
  
  ## put together for figure  and r^2
  mcmc <- results_root_VA$BUGSoutput$sims.matrix
  coefs = mcmc[, c("b[1]", "b[2]", "b[3]", "b[4]")]
  fit = coefs %*% t(treatmat)
  resid = sweep(fit, 2, traits, "-")
  var_f = apply(fit, 1, var)
  var_e = apply(resid, 1, var)
  R2 = var_f/(var_f + var_e)
  tidyMCMC(as.mcmc(R2), conf.int = TRUE, conf.method = "HPDinterval")
  
  #residuals
  coefs2 = apply(coefs, 2, median)
  fit2 = as.vector(coefs2 %*% t(treatmat))
  resid2 <- traits - fit2
  sresid2 <- resid2/sd(resid2)
  ggplot() + geom_point(data = NULL, aes(y = resid2, x = invlogit(fit2)))
  hist(resid2)
  
  # check predicted versus observed
  yRep = sapply(1:nrow(mcmc), function(i) rnorm(nrow(dat_root), fit[i,]))
  ggplot() + geom_density(data = NULL, aes(x = (as.vector(yRep)),
                                           fill = "Model"), alpha = 0.5) + 
    geom_density(data = dat_root, aes(x = (traits), fill = "Obs"), alpha = 0.5)
  
  # generate plots
  newdat <- expand.grid(WP_MPa = seq(min(WP_MPa), max(WP_MPa), length = 50),
                        Precip = c(unique(standard(dat$precip))))
  
  xmat <- model.matrix(~Precip*WP_MPa, newdat)
  fit = coefs %*% t(xmat)
  newdat <- newdat %>% cbind(tidyMCMC(fit, conf.int = TRUE))
  
  graphdat <- dat_root 
  graphdat$WP_MPa <- standard(graphdat$WP_MPa)                   
  graphdat$Precip <- as.numeric(standard(dat_root$precip))
  
root_VA_plot <- ggplot()+ 
    geom_jitter(data=graphdat, aes(x=WP_MPa, y=dry_mass_g_root, colour = factor(Precip)))+
    geom_ribbon(data=newdat, aes(ymin=exp(conf.low), ymax=exp(conf.high), x=WP_MPa, 
                                 fill = factor(Precip), alpha=0.35))+
    geom_line(data=newdat, aes(y = exp(estimate), x = WP_MPa, colour = factor(round(Precip, digits = 2))))+
    xlab("Standardized WP") + 
    ylab("Root biomass")+ 
    theme(panel.background = element_rect(fill='white', colour='black'))+
    theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
    scale_colour_manual(values = Precip_palette)+
    scale_fill_manual(values = Precip_palette)

  
#### Above ground biomass Veronica alpina ####
dat <- seedlings_VA
dat_above_ground <- dat %>% filter(!is.na(dry_mass_g_above_ground)) 
# group level effects
site <- factor(dat_above_ground$siteID)
petridish <- factor(dat_above_ground$petri_dish)

# independent variables
WP <- as.numeric(standard(as.numeric(dat_above_ground$water_potential)))
WP_MPa <- as.numeric(standard(dat_above_ground$WP_MPa))
Precip <- as.numeric(standard(dat_above_ground$precip))

# dependent variables
traits <- as.numeric(log(dat_above_ground$dry_mass_g_above_ground))
N <- as.numeric(length(traits))

treatmat <- model.matrix(~Precip*WP_MPa)
n_parm <- as.numeric(ncol(treatmat))

# look at the data before the analysis
ggplot(dat, aes(x=water_potential, y = log(dry_mass_g_above_ground)), adjust = 0.01)+
  geom_jitter()+facet_wrap(~siteID)

jags.data <- list("treatmat", "traits", "N",  "site", "n_parm")
jags.param <- c("b", "rss", "rss_new", "r", "sig1", "sig_t") 

results_above_ground_VA <- jags.parallel(data = jags.data,
                                 #inits = inits.fn,
                                 parameters.to.save = jags.param,
                                 n.iter = 50000,
                                 model.file = model_traits,
                                 n.thin = 5,
                                 n.chains = 3,
                                 n.burnin = 10000)
results_above_ground_VA

# traceplots
s <- ggs(as.mcmc(results_above_ground_VA))
ggs_traceplot(s, family="b") 

# check Gelman Rubin Statistics
gelman.diag(as.mcmc(results_above_ground_VA))

# Posterior predictive check
plot(results_above_ground_VA$BUGSoutput$sims.list$rss_new, results_above_ground_VA$BUGSoutput$sims.list$rss,
     main = "",)
abline(0,1, lwd = 2, col = "black") 

mean(results_above_ground_VA$BUGSoutput$sims.list$rss_new > results_above_ground_VA$BUGSoutput$sims.list$rss)

## put together for figure  and r^2
mcmc <- results_above_ground_VA$BUGSoutput$sims.matrix
coefs = mcmc[, c("b[1]", "b[2]", "b[3]", "b[4]")]
fit = coefs %*% t(treatmat)
resid = sweep(fit, 2, traits, "-")
var_f = apply(fit, 1, var)
var_e = apply(resid, 1, var)
R2 = var_f/(var_f + var_e)
tidyMCMC(as.mcmc(R2), conf.int = TRUE, conf.method = "HPDinterval")

#residuals
coefs2 = apply(coefs, 2, median)
fit2 = as.vector(coefs2 %*% t(treatmat))
resid2 <- traits - fit2
sresid2 <- resid2/sd(resid2)
ggplot() + geom_point(data = NULL, aes(y = resid2, x = invlogit(fit2)))
hist(resid2)

# check predicted versus observed
yRep = sapply(1:nrow(mcmc), function(i) rnorm(nrow(dat_above_ground), fit[i,]))
ggplot() + geom_density(data = NULL, aes(x = (as.vector(yRep)),
                                         fill = "Model"), alpha = 0.5) + 
  geom_density(data = dat_above_ground, aes(x = (traits), fill = "Obs"), alpha = 0.5)

# generate plots
newdat <- expand.grid(WP_MPa = seq(min(WP_MPa), max(WP_MPa), length = 50),
                      Precip = c(unique(standard(dat$precip))))

xmat <- model.matrix(~Precip*WP_MPa, newdat)
fit = coefs %*% t(xmat)
newdat <- newdat %>% cbind(tidyMCMC(fit, conf.int = TRUE))

graphdat <- dat_above_ground 
graphdat$WP_MPa <- standard(graphdat$WP_MPa)                   
graphdat$Precip <- as.numeric(standard(dat_above_ground$precip))

ggplot()+ 
  geom_jitter(data=graphdat, aes(x=WP_MPa, y=dry_mass_g_above_ground, colour = factor(round(Precip, digits = 2))))+
  geom_ribbon(data=newdat, aes(ymin=exp(conf.low), ymax=exp(conf.high), x=WP_MPa, 
                               fill = factor(round(Precip, digits = 2)), alpha=0.35))+
  geom_line(data=newdat, aes(y = exp(estimate), x = WP_MPa, colour = factor(round(Precip, digits = 2))))+
  xlab("Standardized WP") + 
  ylab("Above ground biomass")+ 
  theme(panel.background = element_rect(fill='white', colour='black'))+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_colour_manual(values = Precip_palette)+
  scale_fill_manual(values = Precip_palette)

##### Seedlings Sibbaldia procumbens #####
seedlings_SP <- Sib_pro_germ %>% 
  select(ID, species, siteID, water_potential, replicate, seed_nr, dry_mass_g_root, dry_mass_g_above_ground, dry_mass_g_total, petri_dish) %>% 
  mutate(dry_mass_g_root = dry_mass_g_root + 7.0e-06,
         dry_mass_g_above_ground = dry_mass_g_above_ground + 7.0e-06,
         dry_mass_g_total = dry_mass_g_total + 7.0e-06) %>% 
  mutate(root_shoot_ratio = dry_mass_g_root/dry_mass_g_above_ground) %>% 
  filter(!is.na(dry_mass_g_above_ground)) %>% 
  mutate(precip = case_when(siteID == "SKJ" ~ 3402,
                            siteID == "GUD" ~ 2130,
                            siteID == "LAV" ~ 1561,
                            siteID == "ULV" ~ 1226)) %>% 
  mutate(precip = precip/1000) %>% 
  mutate(WP_MPa = case_when(water_potential == 1 ~ -0.25,
                            water_potential == 2 ~ -0.33,
                            water_potential == 3 ~ -0.42,
                            water_potential == 4 ~ -0.50,
                            water_potential == 5 ~ -0.57,
                            water_potential == 6 ~ -0.70,
                            water_potential == 7 ~ -0.95,
                            water_potential == 8 ~ -1.20,
                            water_potential == 9 ~ -1.45,
                            water_potential == 10 ~ -1.70))%>% 
  mutate(water_potential = as.numeric(water_potential))

## Looking at seedlings
seedlings_SP %>% 
  #filter(!siteID == "LAV") %>% 
ggplot(aes(x = as.factor(WP_MPa), y = root_shoot_ratio, fill = as.factor(WP_MPa))) +
  geom_violin()+
  geom_jitter(aes(alpha = 0.3)) +
  facet_wrap(~siteID)

#### Root:shoot ratio Sibbaldia procumbens ####

seedlings_SP %>% 
  #filter(water_potential < 7) %>% 
  ggplot(aes(x = as.factor(water_potential), y = root_shoot_ratio, fill = as.factor(water_potential))) +
  geom_violin()+
  geom_jitter()

dat_root_shoot <- seedlings_SP %>% filter(!is.na(root_shoot_ratio)) %>% filter(water_potential < 7)
# group level effects
#site <- factor(dat_root_shoot$siteID)
petridish <- factor(dat_root_shoot$petri_dish)
n_petri <- length(levels(petridish))

# independent variables
WP <- as.numeric(standard(as.numeric(dat_root_shoot$water_potential)))
WP_MPa <- as.numeric(standard(dat_root_shoot$WP_MPa))
#Precip <- as.numeric(standard(dat_root_shoot$precip))
Precip <- as.factor(dat_root_shoot$precip)

# dependent variables
traits <- as.numeric(log(dat_root_shoot$root_shoot_ratio))
N <- as.numeric(length(traits))

treatmat <- model.matrix(~Precip*WP_MPa)
n_parm <- as.numeric(ncol(treatmat))

# look at the data before the analysis
ggplot(dat, aes(x=water_potential, y = log(root_shoot_ratio)), adjust = 0.01)+
  geom_point()+facet_wrap(~siteID)

jags.data <- list("treatmat", "traits", "N", "petridish", "n_petri", "n_parm")
jags.param <- c("b", "rss", "rss_new", "r", "sig1", "sig_t") 

results_root_shoot_SP <- jags.parallel(data = jags.data,
                                       #inits = inits.fn,
                                       parameters.to.save = jags.param,
                                       n.iter = 5000,
                                       model.file = model_traits,
                                       n.thin = 5,
                                       n.chains = 3)
results_root_shoot_SP

# traceplots
s <- ggs(as.mcmc(results_root_shoot_SP))
ggs_traceplot(s, family="b") 

# check Gelman Rubin Statistics
gelman.diag(as.mcmc(results_root_shoot_SP))

# Posterior predictive check
plot(results_root_shoot_SP$BUGSoutput$sims.list$rss_new, results_root_shoot_SP$BUGSoutput$sims.list$rss,
     main = "",)
abline(0,1, lwd = 2, col = "black") # these are still not great but not sure what to do 

mean(results_root_shoot_SP$BUGSoutput$sims.list$rss_new > results_root_shoot_SP$BUGSoutput$sims.list$rss)

## put together for figure  and r^2
mcmc <- results_root_shoot_SP$BUGSoutput$sims.matrix
coefs = mcmc[, c("b[1]", "b[2]", "b[3]", "b[4]", "b[5]", "b[6]", "b[7]", "b[8]")]
fit = coefs %*% t(treatmat)
resid = sweep(fit, 2, traits, "-")
var_f = apply(fit, 1, var)
var_e = apply(resid, 1, var)
R2 = var_f/(var_f + var_e)
tidyMCMC(as.mcmc(R2), conf.int = TRUE, conf.method = "HPDinterval")

#residuals
coefs2 = apply(coefs, 2, median)
fit2 = as.vector(coefs2 %*% t(treatmat))
resid2 <- traits - fit2
sresid2 <- resid2/sd(resid2)
ggplot() + geom_point(data = NULL, aes(y = resid2, x = fit2))
hist(resid2)

# check predicted versus observed
yRep = sapply(1:nrow(mcmc), function(i) rnorm(nrow(dat_root_shoot), fit[i,]))
ggplot() + geom_density(data = NULL, aes(x = (as.vector(yRep)),
                                         fill = "Model"), alpha = 0.5) + 
  geom_density(data = dat_root_shoot, aes(x = (traits), fill = "Obs"), alpha = 0.5)


# generate plots
newdat <- expand.grid(WP_MPa = seq(min(WP_MPa), max(WP_MPa), length = 50),
                      #Precip = c(unique(standard(dat_root_shoot$precip))))
                      Precip = unique(as.factor(dat$precip)))

xmat <- model.matrix(~Precip*WP_MPa, newdat)
fit = coefs %*% t(xmat)
newdat <- newdat %>% cbind(tidyMCMC(fit, conf.int = TRUE))

graphdat <- dat_root_shoot 
graphdat$WP_MPa <- standard(graphdat$WP_MPa)                   
#graphdat$Precip <- as.numeric(standard(dat_root_shoot$precip))
graphdat$Precip <- as.factor(dat_root_shoot$precip)


root_shoot_SP_full_plot <- ggplot()+ 
  geom_jitter(data=graphdat, aes(x=WP_MPa, y=root_shoot_ratio, colour = factor(Precip)))+
  geom_ribbon(data=newdat, aes(ymin=exp(conf.low), ymax=exp(conf.high), x=WP_MPa, fill = factor(Precip), alpha=0.35))+
  geom_line(data=newdat, aes(y = exp(estimate), x = WP_MPa, colour = factor(Precip)))+
  xlab("Standardized WP") + 
  ylab("Root:shoot ratio")+ 
  theme(panel.background = element_rect(fill='white', colour='black'))+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_colour_manual(values = Precip_palette) +
  scale_fill_manual(values = Precip_palette) 

root_shoot_SP_zoomed_in <- ggplot()+ 
  geom_jitter(data=graphdat, aes(x=WP_MPa, y=root_shoot_ratio, colour = factor(Precip)))+
  geom_ribbon(data=newdat, aes(ymin=exp(conf.low), ymax=exp(conf.high), x=WP_MPa, fill = factor(Precip), alpha=0.35))+
  geom_line(data=newdat, aes(y = exp(estimate), x = WP_MPa, colour = factor(Precip)))+
  xlab("Standardized WP") + 
  ylab("Root:shoot ratio")+ 
  theme(panel.background = element_rect(fill='white', colour='black'))+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_colour_manual(values = Precip_palette) +
  scale_fill_manual(values = Precip_palette) +
  ylim(0,2)

root_shoot_SP_panels <- ggplot()+ 
  geom_jitter(data=graphdat, aes(x=WP_MPa, y=root_shoot_ratio, colour = factor(Precip)))+
  geom_ribbon(data=newdat, aes(ymin=exp(conf.low), ymax=exp(conf.high), x=WP_MPa, fill = factor(Precip), alpha=0.35))+
  geom_line(data=newdat, aes(y = exp(estimate), x = WP_MPa, colour = factor(Precip)))+
  xlab("Standardized WP") + 
  ylab("Root:shoot ratio")+ 
  theme(panel.background = element_rect(fill='white', colour='black'))+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_colour_manual(values = Precip_palette) +
  scale_fill_manual(values = Precip_palette) +
  ylim(0,2) +
  facet_wrap(~factor(Precip), nrow = 1)


(root_shoot_SP_zoomed_in + inset_element(
  root_shoot_SP_full_plot, 
  left = 0.7, 
  bottom = 0.5, 
  right = 0.99, 
  top = 0.99
)) /
  root_shoot_SP_panels +
  plot_layout(heights = c(4, 1), guides = "collect") & 
  theme(legend.position='bottom', text = element_text(size = 15))

#### Root biomass Sibbaldia procumbens ####

## Looking at seedlings
seedlings_SP %>% 
  #filter(!siteID == "LAV") %>% 
  ggplot(aes(x = as.factor(WP_MPa), y = dry_mass_g_root, fill = as.factor(WP_MPa))) +
  geom_violin()+
  geom_jitter(aes(alpha = 0.3)) #+
  #facet_wrap(~siteID)

dat <- seedlings_SP %>% 
  filter(WP_MPa > -0.71)


dat_root <- dat %>% filter(!is.na(dry_mass_g_root)) 
# group level effects
site <- factor(dat_root$siteID)
petridish <- factor(dat_root$petri_dish)

# independent variables
WP <- as.numeric(standard(as.numeric(dat_root$water_potential)))
WP_MPa <- as.numeric(standard(dat_root$WP_MPa))
Precip <- as.numeric(standard(dat_root$precip))

# dependent variables
traits <- as.numeric(log(dat_root$dry_mass_g_root))
N <- as.numeric(length(traits))

treatmat <- model.matrix(~Precip*WP_MPa)
n_parm <- as.numeric(ncol(treatmat))

# look at the data before the analysis
ggplot(dat, aes(x=water_potential, y = log(dry_mass_g_root)), adjust = 0.01)+
  geom_jitter()+facet_wrap(~siteID)

jags.data <- list("treatmat", "traits", "N",  "site", "n_parm")
jags.param <- c("b", "rss", "rss_new", "r", "sig1", "sig_t") 

results_root_SP <- jags.parallel(data = jags.data,
                                 #inits = inits.fn,
                                 parameters.to.save = jags.param,
                                 n.iter = 15000,
                                 model.file = model_traits,
                                 n.thin = 5,
                                 n.chains = 3,
                                 n.burnin = 5000)
results_root_SP

# traceplots
s <- ggs(as.mcmc(results_root_SP))
ggs_traceplot(s, family="b") 

# check Gelman Rubin Statistics
gelman.diag(as.mcmc(results_root_SP))

# Posterior predictive check
plot(results_root_SP$BUGSoutput$sims.list$rss_new, results_root_SP$BUGSoutput$sims.list$rss,
     main = "",)
abline(0,1, lwd = 2, col = "black") 

mean(results_root_SP$BUGSoutput$sims.list$rss_new > results_root_SP$BUGSoutput$sims.list$rss)

## put together for figure  and r^2
mcmc <- results_root_SP$BUGSoutput$sims.matrix
coefs = mcmc[, c("b[1]", "b[2]", "b[3]", "b[4]")]
fit = coefs %*% t(treatmat)
resid = sweep(fit, 2, traits, "-")
var_f = apply(fit, 1, var)
var_e = apply(resid, 1, var)
R2 = var_f/(var_f + var_e)
tidyMCMC(as.mcmc(R2), conf.int = TRUE, conf.method = "HPDinterval")

#residuals
coefs2 = apply(coefs, 2, median)
fit2 = as.vector(coefs2 %*% t(treatmat))
resid2 <- traits - fit2
sresid2 <- resid2/sd(resid2)
ggplot() + geom_point(data = NULL, aes(y = resid2, x = invlogit(fit2)))
hist(resid2)

# check predicted versus observed
yRep = sapply(1:nrow(mcmc), function(i) rnorm(nrow(dat_root), fit[i,]))
ggplot() + geom_density(data = NULL, aes(x = (as.vector(yRep)),
                                         fill = "Model"), alpha = 0.5) + 
  geom_density(data = dat_root, aes(x = (traits), fill = "Obs"), alpha = 0.5)

# generate plots
newdat <- expand.grid(WP_MPa = seq(min(WP_MPa), max(WP_MPa), length = 50),
                      Precip = c(unique(standard(dat$precip))))

xmat <- model.matrix(~Precip*WP_MPa, newdat)
fit = coefs %*% t(xmat)
newdat <- newdat %>% cbind(tidyMCMC(fit, conf.int = TRUE))

graphdat <- dat_root 
graphdat$WP_MPa <- standard(graphdat$WP_MPa)                   
graphdat$Precip <- as.numeric(standard(dat_root$precip))

root_SP_plot <- ggplot()+ 
  geom_jitter(data=graphdat, aes(x=WP_MPa, y=dry_mass_g_root, colour = factor(round(Precip, digits = 1))))+
  geom_ribbon(data=newdat, aes(ymin=exp(conf.low), ymax=exp(conf.high), x=WP_MPa, 
                               fill = factor(round(Precip, digits = 1)), alpha=0.35))+
  geom_line(data=newdat, aes(y = exp(estimate), x = WP_MPa, colour = factor(round(Precip, digits = 1))))+
  xlab("Standardized WP") + 
  ylab("Root biomass")+ 
  theme(panel.background = element_rect(fill='white', colour='black'))+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_colour_manual(values = Precip_palette)+
  scale_fill_manual(values = Precip_palette)


#### Above ground biomass Sibbaldia procumbens ####
seedlings_SP %>% 
  #filter(!siteID == "LAV") %>% 
  ggplot(aes(x = as.factor(WP_MPa), y = dry_mass_g_above_ground, fill = as.factor(WP_MPa))) +
  geom_violin()+
  geom_jitter(aes(alpha = 0.3)) #+
#facet_wrap(~siteID)

dat <- seedlings_SP %>% 
  filter(WP_MPa > -0.71)

dat_above_ground <- dat %>% filter(!is.na(dry_mass_g_above_ground)) 
# group level effects
site <- factor(dat_above_ground$siteID)
petridish <- factor(dat_above_ground$petri_dish)

# independent variables
WP <- as.numeric(standard(as.numeric(dat_above_ground$water_potential)))
WP_MPa <- as.numeric(standard(dat_above_ground$WP_MPa))
Precip <- as.numeric(standard(dat_above_ground$precip))

# dependent variables
traits <- as.numeric(log(dat_above_ground$dry_mass_g_above_ground))
N <- as.numeric(length(traits))

treatmat <- model.matrix(~Precip*WP_MPa)
n_parm <- as.numeric(ncol(treatmat))

# look at the data before the analysis
ggplot(dat, aes(x=water_potential, y = log(dry_mass_g_above_ground)), adjust = 0.01)+
  geom_jitter()+facet_wrap(~siteID)

jags.data <- list("treatmat", "traits", "N",  "site", "n_parm")
jags.param <- c("b", "rss", "rss_new", "r", "sig1", "sig_t") 

results_above_ground_SP <- jags.parallel(data = jags.data,
                                         #inits = inits.fn,
                                         parameters.to.save = jags.param,
                                         n.iter = 5000,
                                         model.file = model_traits,
                                         n.thin = 5,
                                         n.chains = 3)
results_above_ground_SP

# traceplots
s <- ggs(as.mcmc(results_above_ground_SP))
ggs_traceplot(s, family="b") 

# check Gelman Rubin Statistics
gelman.diag(as.mcmc(results_above_ground_SP))

# Posterior predictive check
plot(results_above_ground_SP$BUGSoutput$sims.list$rss_new, results_above_ground_SP$BUGSoutput$sims.list$rss,
     main = "",)
abline(0,1, lwd = 2, col = "black") 

mean(results_above_ground_SP$BUGSoutput$sims.list$rss_new > results_above_ground_SP$BUGSoutput$sims.list$rss)

## put together for figure  and r^2
mcmc <- results_above_ground_SP$BUGSoutput$sims.matrix
coefs = mcmc[, c("b[1]", "b[2]", "b[3]", "b[4]")]
fit = coefs %*% t(treatmat)
resid = sweep(fit, 2, traits, "-")
var_f = apply(fit, 1, var)
var_e = apply(resid, 1, var)
R2 = var_f/(var_f + var_e)
tidyMCMC(as.mcmc(R2), conf.int = TRUE, conf.method = "HPDinterval")

#residuals
coefs2 = apply(coefs, 2, median)
fit2 = as.vector(coefs2 %*% t(treatmat))
resid2 <- traits - fit2
sresid2 <- resid2/sd(resid2)
ggplot() + geom_point(data = NULL, aes(y = resid2, x = invlogit(fit2)))
hist(resid2)

# check predicted versus observed
yRep = sapply(1:nrow(mcmc), function(i) rnorm(nrow(dat_above_ground), fit[i,]))
ggplot() + geom_density(data = NULL, aes(x = (as.vector(yRep)),
                                         fill = "Model"), alpha = 0.5) + 
  geom_density(data = dat_above_ground, aes(x = (traits), fill = "Obs"), alpha = 0.5)

# generate plots
newdat <- expand.grid(WP_MPa = seq(min(WP_MPa), max(WP_MPa), length = 50),
                      Precip = c(unique(standard(dat$precip))))

xmat <- model.matrix(~Precip*WP_MPa, newdat)
fit = coefs %*% t(xmat)
newdat <- newdat %>% cbind(tidyMCMC(fit, conf.int = TRUE))

graphdat <- dat_above_ground 
graphdat$WP_MPa <- standard(graphdat$WP_MPa)                   
graphdat$Precip <- as.numeric(standard(dat_above_ground$precip))

ggplot()+ 
  geom_jitter(data=graphdat, aes(x=WP_MPa, y=dry_mass_g_above_ground, colour = factor(round(Precip, digits = 2))))+
  geom_ribbon(data=newdat, aes(ymin=exp(conf.low), ymax=exp(conf.high), x=WP_MPa, 
                               fill = factor(round(Precip, digits = 2)), alpha=0.35))+
  geom_line(data=newdat, aes(y = exp(estimate), x = WP_MPa, colour = factor(round(Precip, digits = 2))))+
  xlab("Standardized WP") + 
  ylab("Above ground biomass")+ 
  theme(panel.background = element_rect(fill='white', colour='black'))+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_colour_manual(values = Precip_palette)+
  scale_fill_manual(values = Precip_palette)
