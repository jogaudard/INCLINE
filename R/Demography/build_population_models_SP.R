########################################################################
### Script for building population models for the INCLINE experiment ###
########################################################################

#### Source files ####
source("R/Demography/cleaning_demogprahy.R")
source("R/Demography/ready_demograhy_for_IPM.R")
#Need functions and seed_bank info from build_population_models_VA - might move that to a seperate script and load it here in both of the build_IPM scripts

#### Libraries ####
library(tidyverse)
library(lme4)
library(lmerTest)
library(IPMpack)
library(fields)
library(conflicted)
library(patchwork)
library(arm)

#### Select preferences for conflicts ####

conflict_prefer("select", "dplyr")
conflict_prefer("lmer", "lmerTest")

#### Downloading data from OSF ####

#Make script for reading in ready made data from OSF

#### Load data ####

#Load data 

INCLINE_metadata <- read_delim("data/INCLINE_metadata.csv", delim = ";", locale = locale(decimal_mark = ","))

INCLINE_metadata <- INCLINE_metadata %>% 
  select(plotID, "precipitation_2009-2019") %>% 
  rename(precip = "precipitation_2009-2019") %>% 
  mutate(precip = precip/1000)

# We define a new size axis for the midpoint evaluation of the IPMs:
minSize_SP <- min(Sib_pro_2018_2021$size, na.rm=T)-0.2
maxSize_SP <- max(Sib_pro_2018_2021$sizeNext, na.rm=T)+0.2
x_SP <- seq(from=round(minSize_SP),to=round(maxSize_SP),length=100)
x0_SP <- data.frame(size=x_SP,size2=x_SP*x_SP)


# # To understand the data, we plot survival, growth/shrinkage/stasis, number of seeds, and size of recruits:
# par(mfrow=c(2,2),mar=c(4,4,2,1))
# plot(Ver_alp_2018_2021$size,jitter(Ver_alp_2018_2021$surv),xlab="Size (t)", ylab="Survival to t+1")
# plot(Ver_alp_2018_2021$size, Ver_alp_2018_2021$sizeNext,xlab="Size (t)",ylab="Size (t+1)") 
# plot(Ver_alp_2018_2021$size,jitter(Ver_alp_2018_2021$flo.if),xlab="Size (t)", ylab="Flowering probability") 
# plot(Ver_alp_2018_2021$size, Ver_alp_2018_2021$fec,xlab="Size (t)",ylab="Number of seeds") 
# 
# Ver_alp_2018_2021 %>% filter(offspringNext == "sexual") %>% ggplot(aes( x = sizeNext)) + geom_histogram() + ylab("Seedling size")
# Ver_alp_2018_2021 %>% filter(offspringNext == "clone") %>% ggplot(aes( x = sizeNext)) + geom_histogram() + ylab("Clone size")

Sib_pro_2018_2021 <- Sib_pro_2018_2021 %>% 
  mutate(stage = as.factor(stage),
         stageNext = as.factor(stageNext)) %>% 
  mutate(number = 1)

Sib_pro_2018_2021 <- Sib_pro_2018_2021 %>% 
  left_join(INCLINE_metadata, by = c("plotID")) %>% 
  mutate(treat = paste0(OTC, treatment),
         site_trans = paste0(siteID, transition),
         block_trans = paste0(blockID, transition))

# ungroup() %>%
# as.data.frame() %>%
# mutate(stage = case_when(!is.na(size) ~ "continuous",
#                          is.na(size) ~ NA_character_),
#        stageNext = case_when(!is.na(size) & !is.na(sizeNext) ~ "continuous",
#                              is.na(size) & !is.na(sizeNext) ~ "continuous",
#                              !is.na(size) & is.na(sizeNext) ~ "dead",
#                              TRUE ~ NA_character_))

SP_CC <- Sib_pro_2018_2021 %>% filter(OTC == "C" & treatment == "C")
SP_CR <- Sib_pro_2018_2021 %>% filter(OTC == "C" & treatment == "R")
SP_CE <- Sib_pro_2018_2021 %>% filter(OTC == "C" & treatment == "E")
SP_CN <- Sib_pro_2018_2021 %>% filter(OTC == "C" & treatment == "N")
SP_WC <- Sib_pro_2018_2021 %>% filter(OTC == "W" & treatment == "C")
SP_WR <- Sib_pro_2018_2021 %>% filter(OTC == "W" & treatment == "R")
SP_WE <- Sib_pro_2018_2021 %>% filter(OTC == "W" & treatment == "E")
SP_WN <- Sib_pro_2018_2021 %>% filter(OTC == "W" & treatment == "N")

SP_C_seed_bank <- seed_bank1 %>% 
  filter(species == "Sib_pro",
         warming == "C") %>% 
  ungroup()

SP_OTC_seed_bank <- seed_bank1 %>% 
  filter(species == "Sib_pro",
         warming == "OTC") %>% 
  ungroup() 

##### Ambient temperature control #####

#### P matrix ####

# choosing the best survival model
x11()
par(mfrow=c(1,1))
# survModelComp(dataf= VA_CC, makePlot=TRUE, legendPos="topleft", mainTitle="Survival", ncuts = 30)

summary(glmer(surv ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CC))
AIC(glmer(surv ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CC))
summary(glmer(surv ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_CC))
AIC(glmer(surv ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_CC))
summary(glmer(surv ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_CC)) #We chose this model based on AIC
AIC(glmer(surv ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_CC))
summary(glmer(surv ~ size + (1|block_trans), family = 'binomial', data = SP_CC))
AIC(glmer(surv ~ size + (1|block_trans), family = 'binomial', data = SP_CC))
summary(glmer(surv ~ 1 + (1|block_trans), family = 'binomial', data = SP_CC))
AIC(glmer(surv ~ 1 + (1|block_trans), family = 'binomial', data = SP_CC))

mod_surv_SP_CC <- glmer(surv ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_CC)
plot_surv_SP_CC <- plot_predictions_surv(model = mod_surv_SP_CC, data = SP_CC, minSize_SP, maxSize_SP)

plot_surv_SP_CC

so_SP_CC <- makeSurvObj(SP_CC, "surv ~ size + size2")
so_SP_CC <- coerceSurvObj(so_SP_CC, as.numeric(fixef(mod_surv_SP_CC))) #Adding coefficients from mixed effect model and not from the linear model as is default in makeSurvObj

# choosing the bext growth model
#growthModelComp(dataf=SP_CC, makePlot=TRUE, legendPos="bottomright", mainTitle="Growth")

summary(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = SP_CC))
AIC(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = SP_CC))
summary(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = SP_CC)) 
AIC(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = SP_CC))
summary(lmer(sizeNext ~ size+I(size^2) + (1|block_trans), data = SP_CC))
AIC(lmer(sizeNext ~ size+I(size^2) + (1|block_trans), data = SP_CC)) #We chose this model based on AIC
summary(lmer(sizeNext ~ size + (1|block_trans), data = SP_CC))
AIC(lmer(sizeNext ~ size + (1|block_trans), data = SP_CC))
summary(lmer(sizeNext ~ 1 + (1|block_trans), data = SP_CC))
AIC(lmer(sizeNext ~ 1 + (1|block_trans), data = SP_CC))


mod_growth_SP_CC <- lmer(sizeNext ~ size+I(size^2) + (1|block_trans), data = SP_CC)

plot_growth_SP_CC <- plot_predictions_growth(model = mod_growth_SP_CC, data = SP_CC, minSize_SP, maxSize_SP)

plot_surv_SP_CC | plot_growth_SP_CC


go_SP_CC <- makeGrowthObj(SP_CC, "sizeNext ~ size + size2")
go_SP_CC <- coerceGrowthObj(go_SP_CC, fixef(mod_growth_SP_CC),
                                    sigma.hat(mod_growth_SP_CC)$sigma$data)


# Make discrete transition object
dto_SP_CC <- makeDiscreteTrans(SP_CC, discreteTrans = matrix(
  c(SP_C_seed_bank$seeds_staySB,
    (1-SP_C_seed_bank$seeds_staySB)*seedling_est_SP_Veg,
    (1-SP_C_seed_bank$seeds_staySB)*(1-seedling_est_SP_Veg), 
    0,
    sum(SP_CC$number[SP_CC$stage=="continuous"&SP_CC$stageNext=="continuous"], na.rm=T),
    sum(SP_CC$number[SP_CC$stage=="continuous"&SP_CC$stageNext=="dead"], na.rm=T)),
  ncol = 2,
  nrow = 3, 
  dimnames = list(c("seedbank", "continuous", "dead"), c("seedbank", "continuous"))),
  meanToCont = matrix(Seedling_info_SP$mean_Veg, ncol = 1, nrow = 1, dimnames = list(c("mean"), c("seedbank"))),
  sdToCont = matrix(Seedling_info_SP$sd, ncol = 1, nrow = 1, dimnames = list(c(""),c("seedbank"))))


# With these survival and growth objects in hand, we build a survival/growth (P) matrix.
Pmatrix_SP_CC <- makeIPMPmatrix(survObj=so_SP_CC, growObj=go_SP_CC, minSize=minSize_SP, maxSize=maxSize_SP, correction = "constant", nBigMatrix = 100)

diagnosticsPmatrix(Pmatrix_SP_CC, survObj=so_SP_CC, growObj=go_SP_CC, dff = SP_CC)

Pmatrix_SP_CC <- makeIPMPmatrix(survObj=so_SP_CC, growObj=go_SP_CC, minSize=minSize_SP, maxSize=maxSize_SP, discreteTrans = dto_SP_CC, correction = "constant", nBigMatrix = 100)
x11()
contourPlot2(t(Pmatrix_SP_CC), Pmatrix_SP_CC@meshpoints, maxSize_SP, 0.03, 0, title = "Pmatrix: survival and growth") 


#### F matrix ####
# Choosing the best model for estimating if an individual flowers
summary(glmer(flo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CC))
AIC(glmer(flo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CC))
summary(glmer(flo.if ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_CC)) 
AIC(glmer(flo.if ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_CC))
summary(glmer(flo.if ~ size + precip + (1|block_trans), family = 'binomial', data = SP_CC))
AIC(glmer(flo.if ~ size + precip + (1|block_trans), family = 'binomial', data = SP_CC))
summary(glmer(flo.if ~ size + (1|block_trans), family = 'binomial', data = SP_CC))
AIC(glmer(flo.if ~ size + (1|block_trans), family = 'binomial', data = SP_CC)) #Choosing this model
summary(glmer(flo.if ~ 1 + (1|block_trans), family = 'binomial', data = SP_CC))
AIC(glmer(flo.if ~ 1 + (1|block_trans), family = 'binomial', data = SP_CC))

floweringChosenModel_SP_CC <- flo.if ~ size

mod_flo_if_SP_CC <- glmer(flo.if ~ size + (1|block_trans), family = 'binomial', data = SP_CC) 

par(mfrow=c(1,1))

plot_SP_CC_floif <- plot_predictions_floif(model = mod_flo_if_SP_CC, data = SP_CC, minSize_SP, maxSize_SP)

plot_SP_CC_floif 



# Choosing the best model for estimating the number of flowers, if an individual flowers

summary(glmer(flo.no ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'poisson', data = SP_CC))
AIC(glmer(flo.no ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'poisson', data = SP_CC))
summary(glmer(flo.no ~ size+I(size^2) + precip + (1|block_trans), family = 'poisson', data = SP_CC)) 
AIC(glmer(flo.no ~ size+I(size^2) + precip + (1|block_trans), family = 'poisson', data = SP_CC))
summary(glmer(flo.no ~ size +I(size^2)  + (1|block_trans), family = 'poisson', data = SP_CC)) 
AIC(glmer(flo.no ~ size +I(size^2) + (1|block_trans), family = 'poisson', data = SP_CC))
summary(glmer(flo.no ~ size + (1|block_trans), family = 'poisson', data = SP_CC)) 
AIC(glmer(flo.no ~ size + (1|block_trans), family = 'poisson', data = SP_CC)) 
summary(glmer(flo.no ~ 1 + (1|block_trans), family = 'poisson', data = SP_CC))#Choosing this model because the other ones are highly affected by one outlier
AIC(glmer(flo.no ~ 1 + (1|block_trans), family = 'poisson', data = SP_CC))


flowerNumberChosenModel_SP_CC <- flo.no ~ 1   #Chosen based on biology by looking at the data

mod_flo_no_SP_CC <- glmer(flo.no ~ 1 + (1|block_trans), family = 'poisson', data = SP_CC)

plot_flo_no_SP_CC <-plot_predictions_flono(model = mod_flo_no_SP_CC, data = SP_CC, minSize_SP, maxSize_SP, ylim = 15) 

plot_flo_no_SP_CC

# Make fecundity object
fo_SP_CC <-makeFecObj(SP_CC, 
                      Formula= c(floweringChosenModel_SP_CC, flowerNumberChosenModel_SP_CC),
                      Family = c("binomial", "poisson"),
                      fecConstants = data.frame(seedsPerCap = Seeds_per_capsule_SP,
                                                seedlingEstablishmentRate = seedling_est_SP_Veg), 
                      meanOffspringSize = Seedling_info_SP$mean_Veg,
                      sdOffspringSize = Seedling_info_SP$sd,
                      offspringSplitter = data.frame(seedbank=SP_C_seed_bank$seeds_alive_total_prop* (1-seedling_est_SP_Veg), continuous=(1-(SP_C_seed_bank$seeds_alive_total_prop* (1-seedling_est_SP_Veg)))),
                      vitalRatesPerOffspringType = data.frame(seedbank=c(1,1,1,0), continuous=c(1,1,1,1),
                                                              row.names=c("flo.if","flo.no","seedsPerCap","seedlingEstablishmentRate")))

fo_SP_CC@fitFec[[1]]$coefficients <- as.numeric(fixef(mod_flo_if_SP_CC))
fo_SP_CC@fitFec[[2]]$coefficients <- as.numeric(fixef(mod_flo_no_SP_CC))

Fmatrix_SP_CC <- makeIPMFmatrix(fecObj=fo_SP_CC, minSize=minSize_SP, maxSize=maxSize_SP, correction = "continuous", nBigMatrix = 100)

#Plotting the matrix
contourPlot2(t(Fmatrix_SP_CC), Fmatrix_SP_CC@meshpoints, maxSize, 0.003, 0, title = "Fmatrix: flower and seedlings")

# image.plot(Fmatrix_VA_CC@meshpoints,
#            Fmatrix_VA_CC@meshpoints,
#            t(Fmatrix_VA_CC),
#            main = "Fmatrix: flower and seedlings",
#            xlab = "Size at t",
#            ylab = "Size at t+1")
# 
# image(t(Fmatrix_VA_CC))

#### C matrix ####

SP_CC_clones <- SP_CC %>% 
  filter(offspringNext == "clonal") %>% 
  mutate(number_orphans = case_when(is.na(size) ~ 1,
                                    !is.na(size) ~0)) %>% 
  mutate(total_num_orphan = sum(number_orphans),
         total_num_clones = n()) %>% 
  fill(total_num_orphan, .direction = "downup") %>% 
  mutate(prop_orphan = total_num_orphan/total_num_clones)

#Is the production of clones size dependent
# Chose site_trans as random effect because block_trans complained about singularity
summary(glmer(clo.if ~ size+I(size^2) + precip+I(precip^2) + (1|site_trans), family = 'binomial', data = SP_CC))
AIC(glmer(clo.if ~ size+I(size^2) + precip+I(precip^2) + (1|site_trans), family = 'binomial', data = SP_CC))
summary(glmer(clo.if ~ size+I(size^2) + precip + (1|site_trans), family = 'binomial', data = SP_CC))
AIC(glmer(clo.if ~ size+I(size^2) + precip + (1|site_trans), family = 'binomial', data = SP_CC))
summary(glmer(clo.if ~ size+I(size^2) + (1|site_trans), family = 'binomial', data = SP_CC)) #Choosing thins model based of AIC
AIC(glmer(clo.if ~ size+I(size^2) + (1|site_trans), family = 'binomial', data = SP_CC))
summary(glmer(clo.if ~ size + (1|site_trans), family = 'binomial', data = SP_CC))
AIC(glmer(clo.if ~ size + (1|site_trans), family = 'binomial', data = SP_CC))
summary(glmer(clo.if ~ 1 + (1|site_trans), family = 'binomial', data = SP_CC))
AIC(glmer(clo.if ~ 1 + (1|site_trans), family = 'binomial', data = SP_CC))


mod_clo_SP_CC <- glmer(clo.if ~ size+I(size^2) + (1|site_trans), family = 'binomial', data = SP_CC)
CloneChosenModel_SP_CC <- clo.if ~ size + size2 


plot_clo_if_SP_CC <- plot_predictions_cloif(model = mod_clo_SP_CC, data = SP_CC, minSize_SP, maxSize_SP)
plot_clo_if_SP_CC

#If you produce clones, does how many clones you make change with size of the mother 
# Running linear mixed effects models because all the random effects gave singularity warning. I tried block_trans, site_trans, transition + blockID, transition + siteID, transition, blockID and siteID.
summary(glm(clo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = SP_CC))
AIC(glm(clo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = SP_CC))
summary(glm(clo.no ~ size+I(size^2) + precip, family = 'poisson', data = SP_CC))
AIC(glm(clo.no ~ size+I(size^2) + precip, family = 'poisson', data = SP_CC))
summary(glm(clo.no ~ size+I(size^2), family = 'poisson', data = SP_CC))
AIC(glm(clo.no ~ size+I(size^2), family = 'poisson', data = SP_CC))
summary(glm(clo.no ~ size, family = 'poisson', data = SP_CC))
AIC(glm(clo.no ~ size, family = 'poisson', data = SP_CC))
summary(glm(clo.no ~ 1, family = 'poisson', data = SP_CC)) #Chosing this model based of AIC
AIC(glm(clo.no ~ 1, family = 'poisson', data = SP_CC))

mod_clo_no_SP_CC <- glm(clo.no ~ 1, family = 'poisson', data = SP_CC)
CloneNumberChosenModel_SP_CC <- clo.no ~ 1


plot_clo_no_SP_CC <- plot_predictions_clono(model = mod_clo_no_SP_CC, data = SP_CC, minSize_SP, maxSize_SP, ylim = 6)
plot_clo_no_SP_CC

# Clonal size depending on mother size
# x11()
# par(mfrow=c(1,1))
# growthModelComp(dataf=SP_CC_clones, makePlot=TRUE, legendPos="topright", mainTitle="Growth")
# CloneSizeSPriable_SP_CC <- "1"  #Chosen based on AIC
# 
# go_clone_SP_CC <- makeGrowthObj(SP_CC_clones, sizeNext ~ 1)


summary(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = SP_CC_clones))
AIC(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = SP_CC_clones))
summary(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = SP_CC_clones)) 
AIC(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = SP_CC_clones))
summary(lmer(sizeNext ~ size+I(size^2) + (1|block_trans), data = SP_CC_clones))
AIC(lmer(sizeNext ~ size+I(size^2) + (1|block_trans), data = SP_CC_clones))
summary(lmer(sizeNext ~ size + (1|block_trans), data = SP_CC_clones)) #Using this model based of AIC
AIC(lmer(sizeNext ~ size + (1|block_trans), data = SP_CC_clones))
summary(lmer(sizeNext ~ 1 + (1|block_trans), data = SP_CC_clones)) 
AIC(lmer(sizeNext ~ 1 + (1|block_trans), data = SP_CC_clones))


mod_clone_growth_SP_CC <- lmer(sizeNext ~ size + (1|block_trans), data = SP_CC_clones)
CloneSizeVariable_SP_CC <- "size"

plot_clone_growth_SP_CC <- plot_predictions_growth(model = mod_clone_growth_SP_CC, data = SP_CC_clones, minSize_SP, maxSize_SP)
plot_clone_growth_SP_CC

#Make clonal object
co_SP_CC <- makeClonalObj(SP_CC, fecConstants=data.frame(correctionForOrphans= 1/(1-SP_CC_clones$prop_orphan[1])),
                          offspringSizeExplanatoryVariables = CloneSizeVariable_SP_CC, Formula = c(CloneChosenModel_SP_CC, CloneNumberChosenModel_SP_CC),
                          Family = c("binomial","poisson"), Transform=c("none","none"),offspringSplitter=data.frame(seedbank=0,continuous=1))

co_SP_CC@fitFec[[1]]$coefficients <- as.numeric(fixef(mod_clo_SP_CC))
co_SP_CC@fitFec[[2]]$coefficients <- as.numeric(coef(mod_clo_no_SP_CC)) #not really needed since this is a linear model
co_SP_CC@offspringRel$coefficients <- as.numeric(fixef(mod_clone_growth_SP_CC))
co_SP_CC@sdOffspringSize <- sigma.hat(mod_clone_growth_SP_CC)$sigma$data
co_SP_CC <- co_SP_CC


Cmatrix_SP_CC <- makeIPMCmatrix(clonalObj = co_SP_CC, minSize=minSize_SP, maxSize=maxSize_SP, nBigMatrix = 100, correction = "constant")

contourPlot2(t(Cmatrix_SP_CC), Cmatrix_SP_CC@meshpoints, maxSize_SP, 0.03, 0, title = "Cmatrix: clones") 


#### Build IPM ####
IPM_SP_CC <- Pmatrix_SP_CC + Fmatrix_SP_CC + Cmatrix_SP_CC
#contourPlot2(t( M = IPM_SP_CC_precip1, meshpts = Pmatrix_SP_CC_precip1@meshpoints, maxSize = maxSize, lower = 0.03, upper = 0))
IPM_plot(IPM_control = IPM_SP_CC, minSize = minSize_SP, maxSize = maxSize_SP, zrange = c(-0.03, 0.06)) + ggtitle("Sibbaldia procumbens CC")
#persp(IPM_SP_CC)
as.numeric(eigen(IPM_SP_CC)$value[1])


x11()
contourPlot2(t(IPM_SP_CC), Pmatrix_SP_CC@meshpoints, maxSize_SP, 0.06, 0, title = "Sibbaldia procumbensCC ")



