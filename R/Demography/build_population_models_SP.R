########################################################################
### Script for building population models for the INCLINE experiment ###
########################################################################

#### Source files ####
source("R/Demography/cleaning_demogprahy.R")
source("R/Demography/ready_demograhy_for_IPM.R")
source("R/Demography/functions_for_IPM_building.R")
rm(INCLINE_demography_Sib_pro)
rm(INCLINE_demography_Ver_alp)

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
maxSize_SP <- max(Sib_pro_2018_2021$sizeNext[Sib_pro_2018_2021$sizeNext != max(Sib_pro_2018_2021$sizeNext, na.rm = T)], na.rm=T) #taking out the largest number because this is so much higher than any other individuals
x_SP <- seq(from=round(minSize_SP),to=round(maxSize_SP),length=100)
x0_SP <- data.frame(size=x_SP,size2=x_SP*x_SP)


Sib_pro_2018_2021 <- Sib_pro_2018_2021 %>% 
  mutate(stage = as.factor(stage),
         stageNext = as.factor(stageNext)) %>% 
  mutate(number = 1)

Sib_pro_2018_2021 <- Sib_pro_2018_2021 %>% 
  left_join(INCLINE_metadata, by = c("plotID")) %>% 
  mutate(treat = paste0(OTC, treatment),
         site_trans = paste0(siteID, transition),
         block_trans = paste0(blockID, transition))


SP_CC <- Sib_pro_2018_2021 %>% filter(OTC == "C" & treatment == "C")
SP_CR <- Sib_pro_2018_2021 %>% filter(OTC == "C" & treatment == "R")
SP_CE <- Sib_pro_2018_2021 %>% filter(OTC == "C" & treatment == "E")
SP_CN <- Sib_pro_2018_2021 %>% filter(OTC == "C" & treatment == "N")
SP_WC <- Sib_pro_2018_2021 %>% filter(OTC == "W" & treatment == "C")
SP_WR <- Sib_pro_2018_2021 %>% filter(OTC == "W" & treatment == "R")
SP_WE <- Sib_pro_2018_2021 %>% filter(OTC == "W" & treatment == "E")
SP_WN <- Sib_pro_2018_2021 %>% filter(OTC == "W" & treatment == "N")

SP_C_seed_bank <- seed_bank %>% 
  filter(species == "Sib_pro",
         warming == "C") %>% 
  ungroup()

SP_OTC_seed_bank <- seed_bank %>% 
  filter(species == "Sib_pro",
         warming == "OTC") %>% 
  ungroup() 

##### Ambient temperature control #####

#### P matrix ####

# choosing the best survival model
summary(glmer(surv ~ size+I(size^2)+I(size^3) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CC))
AIC(glmer(surv ~ size+I(size^2)+I(size^3) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CC))
summary(glmer(surv ~ size+I(size^2)+I(size^3) + precip + (1|block_trans), family = 'binomial', data = SP_CC))
AIC(glmer(surv ~ size+I(size^2)+I(size^3) + precip+ (1|block_trans), family = 'binomial', data = SP_CC))
summary(glmer(surv ~ size+I(size^2)+I(size^3) + (1|block_trans), family = 'binomial', data = SP_CC))
AIC(glmer(surv ~ size+I(size^2)+I(size^3) + (1|block_trans), family = 'binomial', data = SP_CC))

summary(glmer(surv ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CC))
AIC(glmer(surv ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CC))
summary(glmer(surv ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_CC))
AIC(glmer(surv ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_CC))
summary(glmer(surv ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_CC)) #We chose this model based on AIC
AIC(glmer(surv ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_CC))
summary(glmer(surv ~ size + precip + (1|block_trans), family = 'binomial', data = SP_CC))
AIC(glmer(surv ~ size + precip + (1|block_trans), family = 'binomial', data = SP_CC))
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

plot_growth_SP_CC


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

#Removing one individual that had many flowers, but no leaves. It highly skews the model
SP_CC_FL <- SP_CC %>% 
  filter(!unique_IDS == "Lav_6_5_4")

# Choosing the best model for estimating if an individual flowers.
summary(glmer(flo.if ~ size+I(size^2)+I(size^3) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CC_FL))
AIC(glmer(flo.if ~ size+I(size^2)+I(size^3) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CC_FL))
summary(glmer(flo.if ~ size+I(size^2)+I(size^3) + precip + (1|block_trans), family = 'binomial', data = SP_CC_FL))
AIC(glmer(flo.if ~ size+I(size^2)+I(size^3) + precip + (1|block_trans), family = 'binomial', data = SP_CC_FL))
summary(glmer(flo.if ~ size+I(size^2)+I(size^3) + (1|block_trans), family = 'binomial', data = SP_CC_FL))
AIC(glmer(flo.if ~ size+I(size^2)+I(size^3) + (1|block_trans), family = 'binomial', data = SP_CC_FL))

summary(glmer(flo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CC_FL))
AIC(glmer(flo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CC_FL))
summary(glmer(flo.if ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_CC_FL)) 
AIC(glmer(flo.if ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_CC_FL))
summary(glmer(flo.if ~ size + precip + (1|block_trans), family = 'binomial', data = SP_CC_FL))
AIC(glmer(flo.if ~ size + precip + (1|block_trans), family = 'binomial', data = SP_CC_FL))
summary(glmer(flo.if ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_CC_FL)) 
AIC(glmer(flo.if ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_CC_FL))
summary(glmer(flo.if ~ size + (1|block_trans), family = 'binomial', data = SP_CC_FL))
AIC(glmer(flo.if ~ size + (1|block_trans), family = 'binomial', data = SP_CC_FL)) #Choosing this model
summary(glmer(flo.if ~ 1 + (1|block_trans), family = 'binomial', data = SP_CC_FL))
AIC(glmer(flo.if ~ 1 + (1|block_trans), family = 'binomial', data = SP_CC_FL))

floweringChosenModel_SP_CC <- flo.if ~ size

mod_flo_if_SP_CC <- glmer(flo.if ~ size + (1|block_trans), family = 'binomial', data = SP_CC_FL) 

par(mfrow=c(1,1))

plot_SP_CC_floif <- plot_predictions_floif(model = mod_flo_if_SP_CC, data = SP_CC_FL, minSize_SP, maxSize_SP)

plot_SP_CC_floif 



# Choosing the best model for estimating the number of flowers, if an individual flowers
# Using transition as the random effect because the other options came back with singularity warning. I tried block_trans, site_trans, transition + blockID, transition + siteID, blockId, and siteID.
summary(glmer(flo.no ~ size+I(size^2) + precip+I(precip^2) + (1|transition), family = 'poisson', data = SP_CC_FL))
AIC(glmer(flo.no ~ size+I(size^2) + precip+I(precip^2) + (1|transition), family = 'poisson', data = SP_CC_FL))
summary(glmer(flo.no ~ size+I(size^2) + precip + (1|transition), family = 'poisson', data = SP_CC_FL)) 
AIC(glmer(flo.no ~ size+I(size^2) + precip + (1|transition), family = 'poisson', data = SP_CC_FL))
summary(glmer(flo.no ~ size +I(size^2)  + (1|transition), family = 'poisson', data = SP_CC_FL)) 
AIC(glmer(flo.no ~ size +I(size^2) + (1|transition), family = 'poisson', data = SP_CC_FL))
summary(glmer(flo.no ~ size + (1|transition), family = 'poisson', data = SP_CC_FL)) 
AIC(glmer(flo.no ~ size + (1|transition), family = 'poisson', data = SP_CC_FL)) 
summary(glmer(flo.no ~ 1 + (1|transition), family = 'poisson', data = SP_CC_FL))#Choosing this model because the other ones are highly affected by one outlier
AIC(glmer(flo.no ~ 1 + (1|block_trans), family = 'poisson', data = SP_CC_FL))


flowerNumberChosenModel_SP_CC <- flo.no ~ 1   #Chosen based on biology by looking at the data

mod_flo_no_SP_CC <- glmer(flo.no ~ 1 + (1|transition), family = 'poisson', data = SP_CC_FL)

plot_flo_no_SP_CC <-plot_predictions_flono(model = mod_flo_no_SP_CC, data = SP_CC_FL, minSize_SP, maxSize_SP, ylim = 15) 

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
summary(glmer(clo.if ~ size+I(size^2)+I(size^3) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CC))
AIC(glmer(clo.if ~ size+I(size^2)+I(size^3) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CC))
summary(glmer(clo.if ~ size+I(size^2)+I(size^3) + precip+ (1|block_trans), family = 'binomial', data = SP_CC))
AIC(glmer(clo.if ~ size+I(size^2)+I(size^3) + precip + (1|block_trans), family = 'binomial', data = SP_CC))
summary(glmer(clo.if ~ size+I(size^2)+I(size^3) + (1|block_trans), family = 'binomial', data = SP_CC))
AIC(glmer(clo.if ~ size+I(size^2)+I(size^3) + (1|block_trans), family = 'binomial', data = SP_CC))

summary(glmer(clo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CC))
AIC(glmer(clo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CC))
summary(glmer(clo.if ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_CC))
AIC(glmer(clo.if ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_CC))
summary(glmer(clo.if ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_CC)) #Choosing this model because the cubic one predicts that the chance of producing clones is 100% at high sixe, which does not make biological sense.
AIC(glmer(clo.if ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_CC))
summary(glmer(clo.if ~ size + (1|block_trans), family = 'binomial', data = SP_CC))
AIC(glmer(clo.if ~ size + (1|block_trans), family = 'binomial', data = SP_CC))
summary(glmer(clo.if ~ 1 + (1|block_trans), family = 'binomial', data = SP_CC))
AIC(glmer(clo.if ~ 1 + (1|block_trans), family = 'binomial', data = SP_CC))

#Checking for convergiance in the model since it is giving a warning. Looks ok
glmer(clo.if ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_CC, verbose = TRUE) 

mod_clo_SP_CC <- glmer(clo.if ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_CC)
CloneChosenModel_SP_CC <- clo.if ~ size + size2 


plot_clo_if_SP_CC <- plot_predictions_cloif(model = mod_clo_SP_CC, data = SP_CC, minSize_SP, maxSize_SP)
plot_clo_if_SP_CC

#If you produce clones, does how many clones you make change with size of the mother 
# Running linear models because all the random effects gave singularity warning. I tried block_trans, site_trans, transition + blockID, transition + siteID, transition, blockID and siteID.
summary(glm(clo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = SP_CC))
AIC(glm(clo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = SP_CC))
summary(glm(clo.no ~ size+I(size^2) + precip, family = 'poisson', data = SP_CC))
AIC(glm(clo.no ~ size+I(size^2) + precip, family = 'poisson', data = SP_CC))
summary(glm(clo.no ~ size + precip, family = 'poisson', data = SP_CC))
AIC(glm(clo.no ~ size + precip, family = 'poisson', data = SP_CC))
summary(glm(clo.no ~ size, family = 'poisson', data = SP_CC))
AIC(glm(clo.no ~ size, family = 'poisson', data = SP_CC))
summary(glm(clo.no ~ 1, family = 'poisson', data = SP_CC)) #Chosing this model based of AIC
AIC(glm(clo.no ~ 1, family = 'poisson', data = SP_CC))

mod_clo_no_SP_CC <- glm(clo.no ~ 1, family = 'poisson', data = SP_CC)
CloneNumberChosenModel_SP_CC <- clo.no ~ 1


plot_clo_no_SP_CC <- plot_predictions_clono(model = mod_clo_no_SP_CC, data = SP_CC, minSize_SP, maxSize_SP, ylim = 6)
plot_clo_no_SP_CC

# Clonal size depending on mother size
summary(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = SP_CC_clones))
AIC(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = SP_CC_clones))
summary(lmer(sizeNext ~ size + precip+I(precip^2) + (1|block_trans), data = SP_CC_clones))
AIC(lmer(sizeNext ~ size + precip+I(precip^2) + (1|block_trans), data = SP_CC_clones))
summary(lmer(sizeNext ~ size + precip + (1|block_trans), data = SP_CC_clones)) 
AIC(lmer(sizeNext ~ size + precip + (1|block_trans), data = SP_CC_clones))
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

##### Ambient temperature removal #####

#### P matrix ####

# choosing the best survival model
summary(glmer(surv ~ size+I(size^2)+I(size^3) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CR))
AIC(glmer(surv ~ size+I(size^2)+I(size^3) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CR))
summary(glmer(surv ~ size+I(size^2)+I(size^3) + precip + (1|block_trans), family = 'binomial', data = SP_CR))
AIC(glmer(surv ~ size+I(size^2)+I(size^3) + precip + (1|block_trans), family = 'binomial', data = SP_CR))
summary(glmer(surv ~ size+I(size^2)+I(size^3) + (1|block_trans), family = 'binomial', data = SP_CR))
AIC(glmer(surv ~ size+I(size^2)+I(size^3) + (1|block_trans), family = 'binomial', data = SP_CR))

summary(glmer(surv ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CR))
AIC(glmer(surv ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CR))
summary(glmer(surv ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_CR))
AIC(glmer(surv ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_CR))
summary(glmer(surv ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_CR)) #We chose this model based on AIC
AIC(glmer(surv ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_CR))
summary(glmer(surv ~ size + precip + (1|block_trans), family = 'binomial', data = SP_CR))
AIC(glmer(surv ~ size + precip + (1|block_trans), family = 'binomial', data = SP_CR))
summary(glmer(surv ~ size + (1|block_trans), family = 'binomial', data = SP_CR))
AIC(glmer(surv ~ size + (1|block_trans), family = 'binomial', data = SP_CR))
summary(glmer(surv ~ 1 + (1|block_trans), family = 'binomial', data = SP_CR))
AIC(glmer(surv ~ 1 + (1|block_trans), family = 'binomial', data = SP_CR))

mod_surv_SP_CR <- glmer(surv ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_CR)
plot_surv_SP_CR <- plot_predictions_surv(model = mod_surv_SP_CR, data = SP_CR, minSize_SP, maxSize_SP)

plot_surv_SP_CR

so_SP_CR <- makeSurvObj(SP_CR, "surv ~ size + size2")
so_SP_CR <- coerceSurvObj(so_SP_CR, as.numeric(fixef(mod_surv_SP_CR))) #Adding coefficients from mixed effect model and not from the linear model as is default in makeSurvObj

# choosing the best growth model
summary(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = SP_CR))
AIC(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = SP_CR))
summary(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = SP_CR)) 
AIC(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = SP_CR))
summary(lmer(sizeNext ~ size+I(size^2) + (1|block_trans), data = SP_CR))
AIC(lmer(sizeNext ~ size+I(size^2) + (1|block_trans), data = SP_CR)) #We chose this model based on AIC
summary(lmer(sizeNext ~ size + (1|block_trans), data = SP_CR))
AIC(lmer(sizeNext ~ size + (1|block_trans), data = SP_CR))
summary(lmer(sizeNext ~ 1 + (1|block_trans), data = SP_CR))
AIC(lmer(sizeNext ~ 1 + (1|block_trans), data = SP_CR))


mod_growth_SP_CR <- lmer(sizeNext ~ size+I(size^2) + (1|block_trans), data = SP_CR)

plot_growth_SP_CR <- plot_predictions_growth(model = mod_growth_SP_CR, data = SP_CR, minSize_SP, maxSize_SP)

plot_growth_SP_CR


go_SP_CR <- makeGrowthObj(SP_CR, "sizeNext ~ size + size2")
go_SP_CR <- coerceGrowthObj(go_SP_CR, fixef(mod_growth_SP_CR),
                            sigma.hat(mod_growth_SP_CR)$sigma$data)


# Make discrete transition object
dto_SP_CR <- makeDiscreteTrans(SP_CR, discreteTrans = matrix(
  c(SP_C_seed_bank$seeds_staySB,
    (1-SP_C_seed_bank$seeds_staySB)*seedling_est_SP_NoVeg,
    (1-SP_C_seed_bank$seeds_staySB)*(1-seedling_est_SP_NoVeg), 
    0,
    sum(SP_CR$number[SP_CR$stage=="continuous"&SP_CR$stageNext=="continuous"], na.rm=T),
    sum(SP_CR$number[SP_CR$stage=="continuous"&SP_CR$stageNext=="dead"], na.rm=T)),
  ncol = 2,
  nrow = 3, 
  dimnames = list(c("seedbank", "continuous", "dead"), c("seedbank", "continuous"))),
  meanToCont = matrix(Seedling_info_SP$mean_NoVeg, ncol = 1, nrow = 1, dimnames = list(c("mean"), c("seedbank"))),
  sdToCont = matrix(Seedling_info_SP$sd, ncol = 1, nrow = 1, dimnames = list(c(""),c("seedbank"))))


# With these survival and growth objects in hand, we build a survival/growth (P) matrix.
Pmatrix_SP_CR <- makeIPMPmatrix(survObj=so_SP_CR, growObj=go_SP_CR, minSize=minSize_SP, maxSize=maxSize_SP, correction = "constant", nBigMatrix = 100)

diagnosticsPmatrix(Pmatrix_SP_CR, survObj=so_SP_CR, growObj=go_SP_CR, dff = SP_CR)

Pmatrix_SP_CR <- makeIPMPmatrix(survObj=so_SP_CR, growObj=go_SP_CR, minSize=minSize_SP, maxSize=maxSize_SP, discreteTrans = dto_SP_CR, correction = "constant", nBigMatrix = 100)
x11()
contourPlot2(t(Pmatrix_SP_CR), Pmatrix_SP_CR@meshpoints, maxSize_SP, 0.03, 0, title = "Pmatrix: survival and growth") 


#### F matrix ####
# Choosing the best model for estimating if an individual flowers
summary(glmer(flo.if ~ size+I(size^2)+I(size^3) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CR))
AIC(glmer(flo.if ~ size+I(size^2)+I(size^3) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CR))
summary(glmer(flo.if ~ size+I(size^2)+I(size^3) + precip + (1|block_trans), family = 'binomial', data = SP_CR))
AIC(glmer(flo.if ~ size+I(size^2)+I(size^3) + precip + (1|block_trans), family = 'binomial', data = SP_CR))
summary(glmer(flo.if ~ size+I(size^2)+I(size^3) + (1|block_trans), family = 'binomial', data = SP_CR))
AIC(glmer(flo.if ~ size+I(size^2)+I(size^3) + (1|block_trans), family = 'binomial', data = SP_CR))

summary(glmer(flo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CR))
AIC(glmer(flo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CR))
summary(glmer(flo.if ~ size + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CR))
AIC(glmer(flo.if ~ size + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CR))
summary(glmer(flo.if ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_CR)) 
AIC(glmer(flo.if ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_CR))
summary(glmer(flo.if ~ size + precip + (1|block_trans), family = 'binomial', data = SP_CR))
AIC(glmer(flo.if ~ size + precip + (1|block_trans), family = 'binomial', data = SP_CR))
summary(glmer(flo.if ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_CR)) 
AIC(glmer(flo.if ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_CR))
summary(glmer(flo.if ~ size + (1|block_trans), family = 'binomial', data = SP_CR))
AIC(glmer(flo.if ~ size + (1|block_trans), family = 'binomial', data = SP_CR)) #Choosing this model
summary(glmer(flo.if ~ 1 + (1|block_trans), family = 'binomial', data = SP_CR))
AIC(glmer(flo.if ~ 1 + (1|block_trans), family = 'binomial', data = SP_CR))

floweringChosenModel_SP_CR <- flo.if ~ size

mod_flo_if_SP_CR <- glmer(flo.if ~ size + (1|block_trans), family = 'binomial', data = SP_CR) 

plot_SP_CR_floif <- plot_predictions_floif(model = mod_flo_if_SP_CR, data = SP_CR, minSize_SP, maxSize_SP)

plot_SP_CR_floif 


# Choosing the best model for estimating the number of flowers, if an individual flowers
# Using transition as the random effect because the other options came back with singularity warning. I tried block_trans, site_trans, transition + blockID, transition + siteID, blockId, and siteID.
summary(glmer(flo.no ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'poisson', data = SP_CR))
AIC(glmer(flo.no ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'poisson', data = SP_CR))
summary(glmer(flo.no ~ size+I(size^2) + precip + (1|block_trans), family = 'poisson', data = SP_CR)) 
AIC(glmer(flo.no ~ size+I(size^2) + precip + (1|block_trans), family = 'poisson', data = SP_CR))
summary(glmer(flo.no ~ size +I(size^2)  + (1|block_trans), family = 'poisson', data = SP_CR)) 
AIC(glmer(flo.no ~ size +I(size^2) + (1|block_trans), family = 'poisson', data = SP_CR))
summary(glmer(flo.no ~ size + (1|block_trans), family = 'poisson', data = SP_CR)) 
AIC(glmer(flo.no ~ size + (1|block_trans), family = 'poisson', data = SP_CR)) 
summary(glmer(flo.no ~ 1 + (1|block_trans), family = 'poisson', data = SP_CR))#Choosing this model because the other ones are highly affected by one outlier
AIC(glmer(flo.no ~ 1 + (1|block_trans), family = 'poisson', data = SP_CR))


flowerNumberChosenModel_SP_CR <- flo.no ~ 1   #Chosen based on biology by looking at the data

mod_flo_no_SP_CR <- glmer(flo.no ~ 1 + (1|block_trans), family = 'poisson', data = SP_CR)

plot_flo_no_SP_CR <-plot_predictions_flono(model = mod_flo_no_SP_CR, data = SP_CR, minSize_SP, maxSize_SP, ylim = 15) 

plot_flo_no_SP_CR

# Make fecundity object
fo_SP_CR <-makeFecObj(SP_CR, 
                      Formula= c(floweringChosenModel_SP_CR, flowerNumberChosenModel_SP_CR),
                      Family = c("binomial", "poisson"),
                      fecConstants = data.frame(seedsPerCap = Seeds_per_capsule_SP,
                                                seedlingEstablishmentRate = seedling_est_SP_NoVeg), 
                      meanOffspringSize = Seedling_info_SP$mean_NoVeg,
                      sdOffspringSize = Seedling_info_SP$sd,
                      offspringSplitter = data.frame(seedbank=SP_C_seed_bank$seeds_alive_total_prop* (1-seedling_est_SP_NoVeg), continuous=(1-(SP_C_seed_bank$seeds_alive_total_prop* (1-seedling_est_SP_NoVeg)))),
                      vitalRatesPerOffspringType = data.frame(seedbank=c(1,1,1,0), continuous=c(1,1,1,1),
                                                              row.names=c("flo.if","flo.no","seedsPerCap","seedlingEstablishmentRate")))

fo_SP_CR@fitFec[[1]]$coefficients <- as.numeric(fixef(mod_flo_if_SP_CR))
fo_SP_CR@fitFec[[2]]$coefficients <- as.numeric(fixef(mod_flo_no_SP_CR))

Fmatrix_SP_CR <- makeIPMFmatrix(fecObj=fo_SP_CR, minSize=minSize_SP, maxSize=maxSize_SP, correction = "continuous", nBigMatrix = 100)

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

SP_CR_clones <- SP_CR %>% 
  filter(offspringNext == "clonal") %>% 
  mutate(number_orphans = case_when(is.na(size) ~ 1,
                                    !is.na(size) ~0)) %>% 
  mutate(total_num_orphan = sum(number_orphans),
         total_num_clones = n()) %>% 
  fill(total_num_orphan, .direction = "downup") %>% 
  mutate(prop_orphan = total_num_orphan/total_num_clones)

#Is the production of clones size dependent
summary(glmer(clo.if ~ size+I(size^2)+I(size^3) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CR))
AIC(glmer(clo.if ~ size+I(size^2)+I(size^3) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CR))
summary(glmer(clo.if ~ size+I(size^2)+I(size^3) + precip + (1|block_trans), family = 'binomial', data = SP_CR))
AIC(glmer(clo.if ~ size+I(size^2)+I(size^3) + precip + (1|block_trans), family = 'binomial', data = SP_CR))
summary(glmer(clo.if ~ size+I(size^2)+I(size^3) + (1|block_trans), family = 'binomial', data = SP_CR))
AIC(glmer(clo.if ~ size+I(size^2)+I(size^3) + (1|block_trans), family = 'binomial', data = SP_CR))

summary(glmer(clo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CR))
AIC(glmer(clo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CR))
summary(glmer(clo.if ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_CR))
AIC(glmer(clo.if ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_CR))
summary(glmer(clo.if ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_CR)) #Choosing this model because the cubic models don't make biological sense.
AIC(glmer(clo.if ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_CR))
summary(glmer(clo.if ~ size + (1|block_trans), family = 'binomial', data = SP_CR))
AIC(glmer(clo.if ~ size + (1|block_trans), family = 'binomial', data = SP_CR))
summary(glmer(clo.if ~ 1 + (1|block_trans), family = 'binomial', data = SP_CR))
AIC(glmer(clo.if ~ 1 + (1|block_trans), family = 'binomial', data = SP_CR))

mod_clo_SP_CR <- glmer(clo.if ~ size+I(size^2) + (1|site_trans), family = 'binomial', data = SP_CR)
CloneChosenModel_SP_CR <- clo.if ~ size + size2 

plot_clo_if_SP_CR <- plot_predictions_cloif(model = mod_clo_SP_CR, data = SP_CR, minSize_SP, maxSize_SP)
plot_clo_if_SP_CR

#If you produce clones, does how many clones you make change with size of the mother 
# Running linear models because all the random effects gave singularity warning. I tried block_trans, site_trans, transition + blockID, transition + siteID, transition, blockID and siteID.
summary(glm(clo.no ~ size+I(size^2) + precip+I(precip^2) +(1|block_trans), family = 'poisson', data = SP_CR))
AIC(glm(clo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = SP_CR))
summary(glm(clo.no ~ size+I(size^2) + precip, family = 'poisson', data = SP_CR))
AIC(glm(clo.no ~ size+I(size^2) + precip, family = 'poisson', data = SP_CR))
summary(glm(clo.no ~ size + precip, family = 'poisson', data = SP_CR))
AIC(glm(clo.no ~ size + precip, family = 'poisson', data = SP_CR))
summary(glm(clo.no ~ size, family = 'poisson', data = SP_CR))
AIC(glm(clo.no ~ size, family = 'poisson', data = SP_CR))
summary(glm(clo.no ~ 1, family = 'poisson', data = SP_CR)) #Chosing this model based of AIC
AIC(glm(clo.no ~ 1, family = 'poisson', data = SP_CR))

mod_clo_no_SP_CR <- glm(clo.no ~ 1, family = 'poisson', data = SP_CR)
CloneNumberChosenModel_SP_CR <- clo.no ~ 1


plot_clo_no_SP_CR <- plot_predictions_clono(model = mod_clo_no_SP_CR, data = SP_CR, minSize_SP, maxSize_SP, ylim = 6)
plot_clo_no_SP_CR

# Clonal size depending on mother size
summary(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = SP_CR_clones))
AIC(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = SP_CR_clones))
summary(lmer(sizeNext ~ size + precip+I(precip^2) + (1|block_trans), data = SP_CR_clones))
AIC(lmer(sizeNext ~ size + precip+I(precip^2) + (1|block_trans), data = SP_CR_clones))
summary(lmer(sizeNext ~ size + precip + (1|block_trans), data = SP_CR_clones)) 
AIC(lmer(sizeNext ~ size + precip + (1|block_trans), data = SP_CR_clones))
summary(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = SP_CR_clones)) 
AIC(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = SP_CR_clones))
summary(lmer(sizeNext ~ size+I(size^2) + (1|block_trans), data = SP_CR_clones))
AIC(lmer(sizeNext ~ size+I(size^2) + (1|block_trans), data = SP_CR_clones))
summary(lmer(sizeNext ~ size + (1|block_trans), data = SP_CR_clones)) #Using this model based of AIC
AIC(lmer(sizeNext ~ size + (1|block_trans), data = SP_CR_clones))
summary(lmer(sizeNext ~ 1 + (1|block_trans), data = SP_CR_clones)) 
AIC(lmer(sizeNext ~ 1 + (1|block_trans), data = SP_CR_clones))


mod_clone_growth_SP_CR <- lmer(sizeNext ~ size + (1|block_trans), data = SP_CR_clones)
CloneSizeVariable_SP_CR <- "size"

plot_clone_growth_SP_CR <- plot_predictions_growth(model = mod_clone_growth_SP_CR, data = SP_CR_clones, minSize_SP, maxSize_SP)
plot_clone_growth_SP_CR

#Make clonal object
co_SP_CR <- makeClonalObj(SP_CR, fecConstants=data.frame(correctionForOrphans= 1/(1-SP_CR_clones$prop_orphan[1])),
                          offspringSizeExplanatoryVariables = CloneSizeVariable_SP_CR, Formula = c(CloneChosenModel_SP_CR, CloneNumberChosenModel_SP_CR),
                          Family = c("binomial","poisson"), Transform=c("none","none"),offspringSplitter=data.frame(seedbank=0,continuous=1))

co_SP_CR@fitFec[[1]]$coefficients <- as.numeric(fixef(mod_clo_SP_CR))
co_SP_CR@fitFec[[2]]$coefficients <- as.numeric(coef(mod_clo_no_SP_CR)) #not really needed since this is a linear model
co_SP_CR@offspringRel$coefficients <- as.numeric(fixef(mod_clone_growth_SP_CR))
co_SP_CR@sdOffspringSize <- sigma.hat(mod_clone_growth_SP_CR)$sigma$data
co_SP_CR <- co_SP_CR


Cmatrix_SP_CR <- makeIPMCmatrix(clonalObj = co_SP_CR, minSize=minSize_SP, maxSize=maxSize_SP, nBigMatrix = 100, correction = "constant")

contourPlot2(t(Cmatrix_SP_CR), Cmatrix_SP_CR@meshpoints, maxSize_SP, 0.03, 0, title = "Cmatrix: clones") 


#### Build IPM ####
IPM_SP_CR <- Pmatrix_SP_CR + Fmatrix_SP_CR + Cmatrix_SP_CR
#contourPlot2(t( M = IPM_SP_CR_precip1, meshpts = Pmatrix_SP_CR_precip1@meshpoints, maxSize = maxSize, lower = 0.03, upper = 0))
IPM_plot(IPM_control = IPM_SP_CR, minSize = minSize_SP, maxSize = maxSize_SP, zrange = c(-0.03, 0.06)) + ggtitle("Sibbaldia procumbens CR")
#persp(IPM_SP_CR)
as.numeric(eigen(IPM_SP_CR)$value[1])


x11()
contourPlot2(t(IPM_SP_CR), Pmatrix_SP_CR@meshpoints, maxSize_SP, 0.06, 0, title = "Sibbaldia procumbensCR ")

##### Ambient temperature extant #####

#### P matrix ####

# choosing the best survival model
summary(glmer(surv ~ size+I(size^2)+I(size^3) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CE))#We chose this model based on AIC
AIC(glmer(surv ~ size+I(size^2)+I(size^3) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CE))
summary(glmer(surv ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CE))#We chose this model based on AIC
AIC(glmer(surv ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CE))
summary(glmer(surv ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_CE))
AIC(glmer(surv ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_CE))
summary(glmer(surv ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_CE)) 
AIC(glmer(surv ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_CE))
summary(glmer(surv ~ size + precip + (1|block_trans), family = 'binomial', data = SP_CE))
AIC(glmer(surv ~ size + precip + (1|block_trans), family = 'binomial', data = SP_CE))
summary(glmer(surv ~ size + (1|block_trans), family = 'binomial', data = SP_CE))
AIC(glmer(surv ~ size + (1|block_trans), family = 'binomial', data = SP_CE))
summary(glmer(surv ~ 1 + (1|block_trans), family = 'binomial', data = SP_CE))
AIC(glmer(surv ~ 1 + (1|block_trans), family = 'binomial', data = SP_CE))

#Checking for conergence. Looks ok.
glmer(surv ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CE, verbose = TRUE)

mod_surv_SP_CE <- glmer(surv ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CE)
plot_surv_SP_CE <- plot_predictions_surv_precip(model = mod_surv_SP_CE, data = SP_CE, minSize_SP, maxSize_SP)

plot_surv_SP_CE

so_SP_CE <- makeSurvObj(SP_CE, "surv ~ size + size2")
so_SP_CE_precip1 <- coerceSurvObj(so_SP_CE, c(as.numeric(fixef(mod_surv_SP_CE)[1]) + 1.2*as.numeric(fixef(mod_surv_SP_CE)[4]) + (1.2)^2*as.numeric(fixef(mod_surv_SP_CE)[5]), as.numeric(fixef(mod_surv_SP_CE)[2]), as.numeric(fixef(mod_surv_SP_CE)[3]))) 
so_SP_CE_precip2 <- coerceSurvObj(so_SP_CE, c(as.numeric(fixef(mod_surv_SP_CE)[1]) + 2.3*as.numeric(fixef(mod_surv_SP_CE)[4]) + (2.3)^2*as.numeric(fixef(mod_surv_SP_CE)[5]), as.numeric(fixef(mod_surv_SP_CE)[2]), as.numeric(fixef(mod_surv_SP_CE)[3]))) 
so_SP_CE_precip3 <- coerceSurvObj(so_SP_CE, c(as.numeric(fixef(mod_surv_SP_CE)[1]) + 3.4*as.numeric(fixef(mod_surv_SP_CE)[4]) + (3.4)^2*as.numeric(fixef(mod_surv_SP_CE)[5]), as.numeric(fixef(mod_surv_SP_CE)[2]), as.numeric(fixef(mod_surv_SP_CE)[3]))) 


# choosing the bext growth model
summary(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = SP_CE))
AIC(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = SP_CE))
summary(lmer(sizeNext ~ size + precip+I(precip^2) + (1|block_trans), data = SP_CE))
AIC(lmer(sizeNext ~ size + precip+I(precip^2) + (1|block_trans), data = SP_CE))
summary(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = SP_CE)) 
AIC(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = SP_CE))
summary(lmer(sizeNext ~ size+I(size^2) + (1|block_trans), data = SP_CE))
AIC(lmer(sizeNext ~ size+I(size^2) + (1|block_trans), data = SP_CE)) 
summary(lmer(sizeNext ~ size + precip + (1|block_trans), data = SP_CE))
AIC(lmer(sizeNext ~ size + precip + (1|block_trans), data = SP_CE))
summary(lmer(sizeNext ~ size + (1|block_trans), data = SP_CE)) #We chose this model based on AIC
AIC(lmer(sizeNext ~ size + (1|block_trans), data = SP_CE))
summary(lmer(sizeNext ~ 1 + (1|block_trans), data = SP_CE))
AIC(lmer(sizeNext ~ 1 + (1|block_trans), data = SP_CE))


mod_growth_SP_CE <- lmer(sizeNext ~ size + (1|block_trans), data = SP_CE)

plot_growth_SP_CE <- plot_predictions_growth(model = mod_growth_SP_CE, data = SP_CE, minSize_SP, maxSize_SP)

plot_growth_SP_CE


go_SP_CE <- makeGrowthObj(SP_CE, "sizeNext ~ size")
go_SP_CE <- coerceGrowthObj(go_SP_CE, fixef(mod_growth_SP_CE),
                            sigma.hat(mod_growth_SP_CE)$sigma$data)


# Make discrete transition object
dto_SP_CE <- makeDiscreteTrans(SP_CE, discreteTrans = matrix(
  c(SP_C_seed_bank$seeds_staySB,
    (1-SP_C_seed_bank$seeds_staySB)*seedling_est_SP_Veg,
    (1-SP_C_seed_bank$seeds_staySB)*(1-seedling_est_SP_Veg), 
    0,
    sum(SP_CE$number[SP_CE$stage=="continuous"&SP_CE$stageNext=="continuous"], na.rm=T),
    sum(SP_CE$number[SP_CE$stage=="continuous"&SP_CE$stageNext=="dead"], na.rm=T)),
  ncol = 2,
  nrow = 3, 
  dimnames = list(c("seedbank", "continuous", "dead"), c("seedbank", "continuous"))),
  meanToCont = matrix(Seedling_info_SP$mean_Veg, ncol = 1, nrow = 1, dimnames = list(c("mean"), c("seedbank"))),
  sdToCont = matrix(Seedling_info_SP$sd, ncol = 1, nrow = 1, dimnames = list(c(""),c("seedbank"))))


# With these survival and growth objects in hand, we build a survival/growth (P) matrix.
Pmatrix_SP_CE_precip1 <- makeIPMPmatrix(survObj=so_SP_CE_precip1, growObj=go_SP_CE, minSize=minSize_SP, maxSize=maxSize_SP, correction = "constant", nBigMatrix = 100)
Pmatrix_SP_CE_precip2 <- makeIPMPmatrix(survObj=so_SP_CE_precip2, growObj=go_SP_CE, minSize=minSize_SP, maxSize=maxSize_SP, correction = "constant", nBigMatrix = 100)
Pmatrix_SP_CE_precip3 <- makeIPMPmatrix(survObj=so_SP_CE_precip3, growObj=go_SP_CE, minSize=minSize_SP, maxSize=maxSize_SP, correction = "constant", nBigMatrix = 100)

diagnosticsPmatrix(Pmatrix_SP_CE_precip1, survObj=so_SP_CE_precip1, growObj=go_SP_CE, dff = SP_CE)
diagnosticsPmatrix(Pmatrix_SP_CE_precip2, survObj=so_SP_CE_precip2, growObj=go_SP_CE, dff = SP_CE)
diagnosticsPmatrix(Pmatrix_SP_CE_precip3, survObj=so_SP_CE_precip3, growObj=go_SP_CE, dff = SP_CE)

Pmatrix_SP_CE_precip1 <- makeIPMPmatrix(survObj=so_SP_CE_precip1, growObj=go_SP_CE, minSize=minSize_SP, maxSize=maxSize_SP, discreteTrans = dto_SP_CE, correction = "constant", nBigMatrix = 100)
Pmatrix_SP_CE_precip2 <- makeIPMPmatrix(survObj=so_SP_CE_precip2, growObj=go_SP_CE, minSize=minSize_SP, maxSize=maxSize_SP, discreteTrans = dto_SP_CE, correction = "constant", nBigMatrix = 100)
Pmatrix_SP_CE_precip3 <- makeIPMPmatrix(survObj=so_SP_CE_precip3, growObj=go_SP_CE, minSize=minSize_SP, maxSize=maxSize_SP, discreteTrans = dto_SP_CE, correction = "constant", nBigMatrix = 100)
x11()
contourPlot2(t(Pmatrix_SP_CE), Pmatrix_SP_CE@meshpoints, maxSize_SP, 0.03, 0, title = "Pmatrix: survival and growth") 


#### F matrix ####
# Choosing the best model for estimating if an individual flowers.
summary(glmer(flo.if ~ size+I(size^2)+I(size^3) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CE))
AIC(glmer(flo.if ~ size+I(size^2)+I(size^3) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CE))
summary(glmer(flo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CE))
AIC(glmer(flo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CE))
summary(glmer(flo.if ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_CE)) 
AIC(glmer(flo.if ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_CE))
summary(glmer(flo.if ~ size + precip + (1|block_trans), family = 'binomial', data = SP_CE)) #Choosing this model
AIC(glmer(flo.if ~ size + precip + (1|block_trans), family = 'binomial', data = SP_CE))
summary(glmer(flo.if ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_CE)) 
AIC(glmer(flo.if ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_CE))
summary(glmer(flo.if ~ size + (1|block_trans), family = 'binomial', data = SP_CE))
AIC(glmer(flo.if ~ size + (1|block_trans), family = 'binomial', data = SP_CE)) 
summary(glmer(flo.if ~ 1 + (1|block_trans), family = 'binomial', data = SP_CE))
AIC(glmer(flo.if ~ 1 + (1|block_trans), family = 'binomial', data = SP_CE))

floweringChosenModel_SP_CE <- flo.if ~ size
mod_flo_if_SP_CE <- glmer(flo.if ~ size + precip + (1|block_trans), family = 'binomial', data = SP_CE) 

plot_SP_CE_floif <- plot_predictions_floif_precip(model = mod_flo_if_SP_CE, data = SP_CE, minSize_SP, maxSize_SP)
plot_SP_CE_floif 



# Choosing the best model for estimating the number of flowers, if an individual flowers
# Using linear model as the random effects came back with singularity warning. I tried block_trans, site_trans, transition + blockID, transition + siteID, blockId, transition and siteID.
summary(glm(flo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = SP_CE))
AIC(glm(flo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = SP_CE))
summary(glm(flo.no ~ size + precip+I(precip^2), family = 'poisson', data = SP_CE))
AIC(glm(flo.no ~ size + precip+I(precip^2), family = 'poisson', data = SP_CE))
summary(glm(flo.no ~ precip+I(precip^2), family = 'poisson', data = SP_CE))
AIC(glm(flo.no ~ precip+I(precip^2), family = 'poisson', data = SP_CE))
summary(glm(flo.no ~ size + precip, family = 'poisson', data = SP_CE)) 
AIC(glm(flo.no ~ size + precip, family = 'poisson', data = SP_CE))
summary(glm(flo.no ~ size, family = 'poisson', data = SP_CE)) 
AIC(glm(flo.no ~ size, family = 'poisson', data = SP_CE)) 
summary(glm(flo.no ~ precip, family = 'poisson', data = SP_CE)) 
AIC(glm(flo.no ~ precip, family = 'poisson', data = SP_CE))
summary(glm(flo.no ~ 1, family = 'poisson', data = SP_CE))#Choosing this model because the other ones are highly affected by one outlier
AIC(glm(flo.no ~ 1, family = 'poisson', data = SP_CE))


flowerNumberChosenModel_SP_CE <- flo.no ~ 1   #Chosen based on biology by looking at the data

mod_flo_no_SP_CE <- glm(flo.no ~ 1, family = 'poisson', data = SP_CE)

plot_flo_no_SP_CE <-plot_predictions_flono(model = mod_flo_no_SP_CE, data = SP_CE, minSize_SP, maxSize_SP, ylim = 15) 

plot_flo_no_SP_CE

# Make fecundity object
fo_SP_CE <-makeFecObj(SP_CE, 
                      Formula= c(floweringChosenModel_SP_CE, flowerNumberChosenModel_SP_CE),
                      Family = c("binomial", "poisson"),
                      fecConstants = data.frame(seedsPerCap = Seeds_per_capsule_SP,
                                                seedlingEstablishmentRate = seedling_est_SP_Veg), 
                      meanOffspringSize = Seedling_info_SP$mean_Veg,
                      sdOffspringSize = Seedling_info_SP$sd,
                      offspringSplitter = data.frame(seedbank=SP_C_seed_bank$seeds_alive_total_prop* (1-seedling_est_SP_Veg), continuous=(1-(SP_C_seed_bank$seeds_alive_total_prop* (1-seedling_est_SP_Veg)))),
                      vitalRatesPerOffspringType = data.frame(seedbank=c(1,1,1,0), continuous=c(1,1,1,1),
                                                              row.names=c("flo.if","flo.no","seedsPerCap","seedlingEstablishmentRate")))

fo_SP_CE@fitFec[[1]]$coefficients <- c(as.numeric(fixef(mod_flo_if_SP_CE)[1]) + 1.2*as.numeric(fixef(mod_flo_if_SP_CE)[3]),
                                       as.numeric(fixef(mod_flo_if_SP_CE)[2]))
fo_SP_CE@fitFec[[2]]$coefficients <- as.numeric(coef(mod_flo_no_SP_CE)[1] + 1.2*as.numeric(coef(mod_flo_no_SP_CE)[2]) + (1.2)^2*as.numeric(coef(mod_flo_no_SP_CE)[3]))
fo_SP_CE_precip1 <- fo_SP_CE

fo_SP_CE@fitFec[[1]]$coefficients <- c(as.numeric(fixef(mod_flo_if_SP_CE)[1]) + 2.3*as.numeric(fixef(mod_flo_if_SP_CE)[3]),
                                       as.numeric(fixef(mod_flo_if_SP_CE)[2]))
fo_SP_CE@fitFec[[2]]$coefficients <- as.numeric(coef(mod_flo_no_SP_CE)[1] + 2.3*as.numeric(coef(mod_flo_no_SP_CE)[2]) + (2.3)^2*as.numeric(coef(mod_flo_no_SP_CE)[3]))
fo_SP_CE_precip2 <- fo_SP_CE

fo_SP_CE@fitFec[[1]]$coefficients <- c(as.numeric(fixef(mod_flo_if_SP_CE)[1]) + 3.4*as.numeric(fixef(mod_flo_if_SP_CE)[3]),
                                       as.numeric(fixef(mod_flo_if_SP_CE)[2]))
fo_SP_CE@fitFec[[2]]$coefficients <- as.numeric(coef(mod_flo_no_SP_CE)[1] + 3.4*as.numeric(coef(mod_flo_no_SP_CE)[2]) + (3.4)^2*as.numeric(coef(mod_flo_no_SP_CE)[3]))
fo_SP_CE_precip3 <- fo_SP_CE


Fmatrix_SP_CE_precip1 <- makeIPMFmatrix(fecObj=fo_SP_CE_precip1, minSize=minSize_SP, maxSize=maxSize_SP, correction = "continuous", nBigMatrix = 100)
Fmatrix_SP_CE_precip2 <- makeIPMFmatrix(fecObj=fo_SP_CE_precip2, minSize=minSize_SP, maxSize=maxSize_SP, correction = "continuous", nBigMatrix = 100)
Fmatrix_SP_CE_precip3 <- makeIPMFmatrix(fecObj=fo_SP_CE_precip3, minSize=minSize_SP, maxSize=maxSize_SP, correction = "continuous", nBigMatrix = 100)

#Plotting the matrix
contourPlot2(t(Fmatrix_SP_CE_precip1), Fmatrix_SP_CE_precip1@meshpoints, maxSize, 0.003, 0, title = "Fmatrix: flower and seedlings")
contourPlot2(t(Fmatrix_SP_CE_precip2), Fmatrix_SP_CE_precip2@meshpoints, maxSize, 0.003, 0, title = "Fmatrix: flower and seedlings")
contourPlot2(t(Fmatrix_SP_CE_precip3), Fmatrix_SP_CE_precip3@meshpoints, maxSize, 0.003, 0, title = "Fmatrix: flower and seedlings")

# image.plot(Fmatrix_VA_CC@meshpoints,
#            Fmatrix_VA_CC@meshpoints,
#            t(Fmatrix_VA_CC),
#            main = "Fmatrix: flower and seedlings",
#            xlab = "Size at t",
#            ylab = "Size at t+1")
# 
# image(t(Fmatrix_VA_CC))

#### C matrix ####

SP_CE_clones <- SP_CE %>% 
  filter(offspringNext == "clonal") %>% 
  mutate(number_orphans = case_when(is.na(size) ~ 1,
                                    !is.na(size) ~0)) %>% 
  mutate(total_num_orphan = sum(number_orphans),
         total_num_clones = n()) %>% 
  fill(total_num_orphan, .direction = "downup") %>% 
  mutate(prop_orphan = total_num_orphan/total_num_clones)

#Is the production of clones size dependent
summary(glmer(clo.if ~ size+I(size^2)+I(size^3) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CE))
AIC(glmer(clo.if ~ size+I(size^2)+I(size^3) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CE))
summary(glmer(clo.if ~ size+I(size^2)+I(size^3) + precip + (1|block_trans), family = 'binomial', data = SP_CE))
AIC(glmer(clo.if ~ size+I(size^2)+I(size^3) + precip+ (1|block_trans), family = 'binomial', data = SP_CE))
summary(glmer(clo.if ~ size+I(size^2)+I(size^3) + (1|block_trans), family = 'binomial', data = SP_CE))
AIC(glmer(clo.if ~ size+I(size^2)+I(size^3) + (1|block_trans), family = 'binomial', data = SP_CE))
summary(glmer(clo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CE))
AIC(glmer(clo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CE))
summary(glmer(clo.if ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_CE))
AIC(glmer(clo.if ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_CE))
summary(glmer(clo.if ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_CE)) #Choosing thins model based of AIC
AIC(glmer(clo.if ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_CE))
summary(glmer(clo.if ~ size + (1|block_trans), family = 'binomial', data = SP_CE))
AIC(glmer(clo.if ~ size + (1|block_trans), family = 'binomial', data = SP_CE))
summary(glmer(clo.if ~ 1 + (1|block_trans), family = 'binomial', data = SP_CE))
AIC(glmer(clo.if ~ 1 + (1|block_trans), family = 'binomial', data = SP_CE))


mod_clo_SP_CE <- glmer(clo.if ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_CE)
CloneChosenModel_SP_CE <- clo.if ~ size + size2 


plot_clo_if_SP_CE <- plot_predictions_cloif(model = mod_clo_SP_CE, data = SP_CE, minSize_SP, maxSize_SP)
plot_clo_if_SP_CE

#If you produce clones, does how many clones you make change with size of the mother 
# Running linear models because all the random effects gave singularity warning. I tried block_trans, site_trans, transition + blockID, transition + siteID, transition, blockID and siteID.
summary(glm(clo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = SP_CE))
AIC(glm(clo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = SP_CE))
summary(glm(clo.no ~ size+I(size^2) + precip, family = 'poisson', data = SP_CE))
AIC(glm(clo.no ~ size+I(size^2) + precip, family = 'poisson', data = SP_CE))
summary(glm(clo.no ~ size + precip, family = 'poisson', data = SP_CE))
AIC(glm(clo.no ~ size + precip, family = 'poisson', data = SP_CE))
summary(glm(clo.no ~ size, family = 'poisson', data = SP_CE))
AIC(glm(clo.no ~ size, family = 'poisson', data = SP_CE))
summary(glm(clo.no ~ 1, family = 'poisson', data = SP_CE)) #Chosing this model based of AIC
AIC(glm(clo.no ~ 1, family = 'poisson', data = SP_CE))

mod_clo_no_SP_CE <- glm(clo.no ~ 1, family = 'poisson', data = SP_CE)
CloneNumberChosenModel_SP_CE <- clo.no ~ 1


plot_clo_no_SP_CE <- plot_predictions_clono(model = mod_clo_no_SP_CE, data = SP_CE, minSize_SP, maxSize_SP, ylim = 6)
plot_clo_no_SP_CE

# Clonal size depending on mother size
summary(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = SP_CE_clones))
AIC(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = SP_CE_clones))
summary(lmer(sizeNext ~ size + precip+I(precip^2) + (1|block_trans), data = SP_CE_clones))
AIC(lmer(sizeNext ~ size + precip+I(precip^2) + (1|block_trans), data = SP_CE_clones))
summary(lmer(sizeNext ~ size + precip + (1|block_trans), data = SP_CE_clones)) 
AIC(lmer(sizeNext ~ size + precip + (1|block_trans), data = SP_CE_clones))
summary(lmer(sizeNext ~ precip+I(precip^2) + (1|block_trans), data = SP_CE_clones))
AIC(lmer(sizeNext ~ precip+I(precip^2) + (1|block_trans), data = SP_CE_clones))
summary(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = SP_CE_clones)) 
AIC(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = SP_CE_clones))
summary(lmer(sizeNext ~ size+I(size^2) + (1|block_trans), data = SP_CE_clones))
AIC(lmer(sizeNext ~ size+I(size^2) + (1|block_trans), data = SP_CE_clones))
summary(lmer(sizeNext ~ size + (1|block_trans), data = SP_CE_clones)) #Using this model based of AIC
AIC(lmer(sizeNext ~ size + (1|block_trans), data = SP_CE_clones))
summary(lmer(sizeNext ~ 1 + (1|block_trans), data = SP_CE_clones)) 
AIC(lmer(sizeNext ~ 1 + (1|block_trans), data = SP_CE_clones))


mod_clone_growth_SP_CE <- lmer(sizeNext ~ size + (1|block_trans), data = SP_CE_clones)
CloneSizeVariable_SP_CE <- "size"

plot_clone_growth_SP_CE <- plot_predictions_growth(model = mod_clone_growth_SP_CE, data = SP_CE_clones, minSize_SP, maxSize_SP)
plot_clone_growth_SP_CE

#Make clonal object
co_SP_CE <- makeClonalObj(SP_CE, fecConstants=data.frame(correctionForOrphans= 1/(1-SP_CE_clones$prop_orphan[1])),
                          offspringSizeExplanatoryVariables = CloneSizeVariable_SP_CE, Formula = c(CloneChosenModel_SP_CE, CloneNumberChosenModel_SP_CE),
                          Family = c("binomial","poisson"), Transform=c("none","none"),offspringSplitter=data.frame(seedbank=0,continuous=1))

co_SP_CE@fitFec[[1]]$coefficients <- as.numeric(fixef(mod_clo_SP_CE))
co_SP_CE@fitFec[[2]]$coefficients <- as.numeric(coef(mod_clo_no_SP_CE)) #not really needed since this is a linear model
co_SP_CE@offspringRel$coefficients <- as.numeric(fixef(mod_clone_growth_SP_CE))
co_SP_CE@sdOffspringSize <- sigma.hat(mod_clone_growth_SP_CE)$sigma$data
co_SP_CE <- co_SP_CE


Cmatrix_SP_CE <- makeIPMCmatrix(clonalObj = co_SP_CE, minSize=minSize_SP, maxSize=maxSize_SP, nBigMatrix = 100, correction = "constant")

contourPlot2(t(Cmatrix_SP_CE), Cmatrix_SP_CE@meshpoints, maxSize_SP, 0.03, 0, title = "Cmatrix: clones") 


#### Build IPM ####
IPM_SP_CE_precip1 <- Pmatrix_SP_CE_precip1 + Fmatrix_SP_CE_precip1 + Cmatrix_SP_CE
IPM_SP_CE_precip2 <- Pmatrix_SP_CE_precip2 + Fmatrix_SP_CE_precip2 + Cmatrix_SP_CE
IPM_SP_CE_precip3 <- Pmatrix_SP_CE_precip3 + Fmatrix_SP_CE_precip3 + Cmatrix_SP_CE
#contourPlot2(t( M = IPM_SP_CE_precip1, meshpts = Pmatrix_SP_CE_precip1@meshpoints, maxSize = maxSize, lower = 0.03, upper = 0))
IPM_plot(IPM_control = IPM_SP_CE_precip1, minSize = minSize_SP, maxSize = maxSize_SP, zrange = c(-0.03, 0.06)) + ggtitle("Sibbaldia procumbens CE 1.2 m/year")
IPM_plot(IPM_control = IPM_SP_CE_precip2, minSize = minSize_SP, maxSize = maxSize_SP, zrange = c(-0.03, 0.06)) + ggtitle("Sibbaldia procumbens CE 2.3 m/year")
IPM_plot(IPM_control = IPM_SP_CE_precip3, minSize = minSize_SP, maxSize = maxSize_SP, zrange = c(-0.03, 0.06)) + ggtitle("Sibbaldia procumbens CE 3.4 m/year")
#persp(IPM_SP_CE)
as.numeric(eigen(IPM_SP_CE_precip1)$value[1])
as.numeric(eigen(IPM_SP_CE_precip2)$value[1])
as.numeric(eigen(IPM_SP_CE_precip3)$value[1])

##### Ambient temperature novel #####

#### P matrix ####

# choosing the best survival model
summary(glmer(surv ~ size+I(size^2)+I(size^3) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CN))
AIC(glmer(surv ~ size+I(size^2)+I(size^3) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CN))
summary(glmer(surv ~ size+I(size^2)+I(size^3) + precip + (1|block_trans), family = 'binomial', data = SP_CN))
AIC(glmer(surv ~ size+I(size^2)+I(size^3) + precip + (1|block_trans), family = 'binomial', data = SP_CN))
summary(glmer(surv ~ size+I(size^2)+I(size^3) + (1|block_trans), family = 'binomial', data = SP_CN))
AIC(glmer(surv ~ size+I(size^2)+I(size^3) + (1|block_trans), family = 'binomial', data = SP_CN))
summary(glmer(surv ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CN))
AIC(glmer(surv ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CN))
summary(glmer(surv ~ size + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CN))#We chose this model based on AIC
AIC(glmer(surv ~ size+ precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CN))
summary(glmer(surv ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_CN))
AIC(glmer(surv ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_CN))
summary(glmer(surv ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_CN)) 
AIC(glmer(surv ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_CN))
summary(glmer(surv ~ size + precip + (1|block_trans), family = 'binomial', data = SP_CN))
AIC(glmer(surv ~ size + precip + (1|block_trans), family = 'binomial', data = SP_CN))
summary(glmer(surv ~ size + (1|block_trans), family = 'binomial', data = SP_CN))
AIC(glmer(surv ~ size + (1|block_trans), family = 'binomial', data = SP_CN))
summary(glmer(surv ~ 1 + (1|block_trans), family = 'binomial', data = SP_CN))
AIC(glmer(surv ~ 1 + (1|block_trans), family = 'binomial', data = SP_CN))

mod_surv_SP_CN <- glmer(surv ~ size + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CN)
plot_surv_SP_CN <- plot_predictions_surv_precip(model = mod_surv_SP_CN, data = SP_CN, minSize_SP, maxSize_SP)

plot_surv_SP_CN

so_SP_CN <- makeSurvObj(SP_CN, "surv ~ size")
so_SP_CN_precip1 <- coerceSurvObj(so_SP_CN, c(as.numeric(fixef(mod_surv_SP_CN)[1]) + 1.2*as.numeric(fixef(mod_surv_SP_CN)[3]) + (1.2)^2*as.numeric(fixef(mod_surv_SP_CN)[4]), 
                                              as.numeric(fixef(mod_surv_SP_CN)[2])))
so_SP_CN_precip2 <- coerceSurvObj(so_SP_CN, c(as.numeric(fixef(mod_surv_SP_CN)[1]) + 2.3*as.numeric(fixef(mod_surv_SP_CN)[3]) + (2.3)^2*as.numeric(fixef(mod_surv_SP_CN)[4]), 
                                              as.numeric(fixef(mod_surv_SP_CN)[2])))
so_SP_CN_precip3 <- coerceSurvObj(so_SP_CN, c(as.numeric(fixef(mod_surv_SP_CN)[1]) + 3.4*as.numeric(fixef(mod_surv_SP_CN)[3]) + (3.4)^2*as.numeric(fixef(mod_surv_SP_CN)[4]), 
                                              as.numeric(fixef(mod_surv_SP_CN)[2])))

# choosing the best growth model
summary(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = SP_CN))
AIC(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = SP_CN))
summary(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = SP_CN)) 
AIC(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = SP_CN))
summary(lmer(sizeNext ~ size+I(size^2) + (1|block_trans), data = SP_CN))#We chose this model based on AIC
AIC(lmer(sizeNext ~ size+I(size^2) + (1|block_trans), data = SP_CN)) 
summary(lmer(sizeNext ~ size + (1|block_trans), data = SP_CN)) 
AIC(lmer(sizeNext ~ size + (1|block_trans), data = SP_CN))
summary(lmer(sizeNext ~ 1 + (1|block_trans), data = SP_CN))
AIC(lmer(sizeNext ~ 1 + (1|block_trans), data = SP_CN))


mod_growth_SP_CN <- lmer(sizeNext ~ size+I(size^2) + (1|block_trans), data = SP_CN)

plot_growth_SP_CN <- plot_predictions_growth(model = mod_growth_SP_CN, data = SP_CN, minSize_SP, maxSize_SP)

plot_growth_SP_CN


go_SP_CN <- makeGrowthObj(SP_CN, "sizeNext ~ size + size2")
go_SP_CN <- coerceGrowthObj(go_SP_CN, fixef(mod_growth_SP_CN),
                            sigma.hat(mod_growth_SP_CN)$sigma$data)


# Make discrete transition object
dto_SP_CN <- makeDiscreteTrans(SP_CN, discreteTrans = matrix(
  c(SP_C_seed_bank$seeds_staySB,
    (1-SP_C_seed_bank$seeds_staySB)*seedling_est_SP_Veg,
    (1-SP_C_seed_bank$seeds_staySB)*(1-seedling_est_SP_Veg), 
    0,
    sum(SP_CN$number[SP_CN$stage=="continuous"&SP_CN$stageNext=="continuous"], na.rm=T),
    sum(SP_CN$number[SP_CN$stage=="continuous"&SP_CN$stageNext=="dead"], na.rm=T)),
  ncol = 2,
  nrow = 3, 
  dimnames = list(c("seedbank", "continuous", "dead"), c("seedbank", "continuous"))),
  meanToCont = matrix(Seedling_info_SP$mean_Veg, ncol = 1, nrow = 1, dimnames = list(c("mean"), c("seedbank"))),
  sdToCont = matrix(Seedling_info_SP$sd, ncol = 1, nrow = 1, dimnames = list(c(""),c("seedbank"))))


# With these survival and growth objects in hand, we build a survival/growth (P) matrix.
Pmatrix_SP_CN_precip1 <- makeIPMPmatrix(survObj=so_SP_CN_precip1, growObj=go_SP_CN, minSize=minSize_SP, maxSize=maxSize_SP, correction = "constant", nBigMatrix = 100)
Pmatrix_SP_CN_precip2 <- makeIPMPmatrix(survObj=so_SP_CN_precip2, growObj=go_SP_CN, minSize=minSize_SP, maxSize=maxSize_SP, correction = "constant", nBigMatrix = 100)
Pmatrix_SP_CN_precip3 <- makeIPMPmatrix(survObj=so_SP_CN_precip3, growObj=go_SP_CN, minSize=minSize_SP, maxSize=maxSize_SP, correction = "constant", nBigMatrix = 100)

diagnosticsPmatrix(Pmatrix_SP_CN_precip1, survObj=so_SP_CN_precip1, growObj=go_SP_CN, dff = SP_CN)
diagnosticsPmatrix(Pmatrix_SP_CN_precip2, survObj=so_SP_CN_precip2, growObj=go_SP_CN, dff = SP_CN)
diagnosticsPmatrix(Pmatrix_SP_CN_precip3, survObj=so_SP_CN_precip3, growObj=go_SP_CN, dff = SP_CN)

Pmatrix_SP_CN_precip1 <- makeIPMPmatrix(survObj=so_SP_CN_precip1, growObj=go_SP_CN, minSize=minSize_SP, maxSize=maxSize_SP, discreteTrans = dto_SP_CN, correction = "constant", nBigMatrix = 100)
Pmatrix_SP_CN_precip2 <- makeIPMPmatrix(survObj=so_SP_CN_precip2, growObj=go_SP_CN, minSize=minSize_SP, maxSize=maxSize_SP, discreteTrans = dto_SP_CN, correction = "constant", nBigMatrix = 100)
Pmatrix_SP_CN_precip3 <- makeIPMPmatrix(survObj=so_SP_CN_precip3, growObj=go_SP_CN, minSize=minSize_SP, maxSize=maxSize_SP, discreteTrans = dto_SP_CN, correction = "constant", nBigMatrix = 100)
x11()
contourPlot2(t(Pmatrix_SP_CN), Pmatrix_SP_CN@meshpoints, maxSize_SP, 0.03, 0, title = "Pmatrix: survival and growth") 


#### F matrix ####
# Choosing the best model for estimating if an individual flowers.
summary(glmer(flo.if ~ size+I(size^2)+I(size^3) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CN))
AIC(glmer(flo.if ~ size+I(size^2)+I(size^3) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CN))
summary(glmer(flo.if ~ size+I(size^2)+I(size^3) + precip + (1|block_trans), family = 'binomial', data = SP_CN))
AIC(glmer(flo.if ~ size+I(size^2)+I(size^3) + precip + (1|block_trans), family = 'binomial', data = SP_CN))
summary(glmer(flo.if ~ size+I(size^2)+I(size^3) + (1|block_trans), family = 'binomial', data = SP_CN))
AIC(glmer(flo.if ~ size+I(size^2)+I(size^3) + (1|block_trans), family = 'binomial', data = SP_CN))

summary(glmer(flo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CN))
AIC(glmer(flo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_CN))
summary(glmer(flo.if ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_CN)) #Choosing this model
AIC(glmer(flo.if ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_CN))
summary(glmer(flo.if ~ size + precip + (1|block_trans), family = 'binomial', data = SP_CN)) 
AIC(glmer(flo.if ~ size + precip + (1|block_trans), family = 'binomial', data = SP_CN))
summary(glmer(flo.if ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_CN)) 
AIC(glmer(flo.if ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_CN))
summary(glmer(flo.if ~ size + (1|block_trans), family = 'binomial', data = SP_CN))
AIC(glmer(flo.if ~ size + (1|block_trans), family = 'binomial', data = SP_CN)) 
summary(glmer(flo.if ~ 1 + (1|block_trans), family = 'binomial', data = SP_CN))
AIC(glmer(flo.if ~ 1 + (1|block_trans), family = 'binomial', data = SP_CN))

floweringChosenModel_SP_CN <- flo.if ~ size + size2
mod_flo_if_SP_CN <- glmer(flo.if ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_CN) 

plot_SP_CN_floif <- plot_predictions_floif_precip(model = mod_flo_if_SP_CN, data = SP_CN, minSize_SP, maxSize_SP)
plot_SP_CN_floif 



# Choosing the best model for estimating the number of flowers, if an individual flowers
# Using linear model as the random effects came back with singularity warning. I tried block_trans, site_trans, transition + blockID, transition + siteID, blockId, transition and siteID.
summary(glmer(flo.no ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'poisson', data = SP_CN))
AIC(glmer(flo.no ~ size+I(size^2) + precip+I(precip^2)  + (1|block_trans), family = 'poisson', data = SP_CN))
summary(glmer(flo.no ~ size+I(size^2) + precip + (1|block_trans), family = 'poisson', data = SP_CN))
AIC(glmer(flo.no ~ size+I(size^2) + precip  + (1|block_trans), family = 'poisson', data = SP_CN))
summary(glmer(flo.no ~ size+I(size^2) + (1|block_trans), family = 'poisson', data = SP_CN))
AIC(glmer(flo.no ~ size+I(size^2)  + (1|block_trans), family = 'poisson', data = SP_CN))
summary(glmer(flo.no ~ size + (1|block_trans), family = 'poisson', data = SP_CN))
AIC(glmer(flo.no ~ size  + (1|block_trans), family = 'poisson', data = SP_CN))
summary(glmer(flo.no ~ 1 + (1|block_trans), family = 'poisson', data = SP_CN)) #Chosing this based on AIC
AIC(glmer(flo.no ~ 1  + (1|block_trans), family = 'poisson', data = SP_CN))

flowerNumberChosenModel_SP_CN <- flo.no ~ 1   

mod_flo_no_SP_CN <- glmer(flo.no ~ 1 + (1|block_trans), family = 'poisson', data = SP_CN)

plot_flo_no_SP_CN <-plot_predictions_flono(model = mod_flo_no_SP_CN, data = SP_CN, minSize_SP, maxSize_SP, ylim = 15) 

plot_flo_no_SP_CN

# Make fecundity object
fo_SP_CN <-makeFecObj(SP_CN, 
                      Formula= c(floweringChosenModel_SP_CN, flowerNumberChosenModel_SP_CN),
                      Family = c("binomial", "poisson"),
                      fecConstants = data.frame(seedsPerCap = Seeds_per_capsule_SP,
                                                seedlingEstablishmentRate = seedling_est_SP_Veg), 
                      meanOffspringSize = Seedling_info_SP$mean_Veg,
                      sdOffspringSize = Seedling_info_SP$sd,
                      offspringSplitter = data.frame(seedbank=SP_C_seed_bank$seeds_alive_total_prop* (1-seedling_est_SP_Veg), continuous=(1-(SP_C_seed_bank$seeds_alive_total_prop* (1-seedling_est_SP_Veg)))),
                      vitalRatesPerOffspringType = data.frame(seedbank=c(1,1,1,0), continuous=c(1,1,1,1),
                                                              row.names=c("flo.if","flo.no","seedsPerCap","seedlingEstablishmentRate")))

fo_SP_CN@fitFec[[1]]$coefficients <- c(as.numeric(fixef(mod_flo_if_SP_CN)[1]) + 1.2*as.numeric(fixef(mod_flo_if_SP_CN)[4]),
                                       as.numeric(fixef(mod_flo_if_SP_CN)[2]),
                                       as.numeric(fixef(mod_flo_if_SP_CN)[3]))
fo_SP_CN@fitFec[[2]]$coefficients <- as.numeric(fixef(mod_flo_no_SP_CN))
fo_SP_CN_precip1 <- fo_SP_CN

fo_SP_CN@fitFec[[1]]$coefficients <- c(as.numeric(fixef(mod_flo_if_SP_CN)[1]) + 2.3*as.numeric(fixef(mod_flo_if_SP_CN)[4]),
                                       as.numeric(fixef(mod_flo_if_SP_CN)[2]),
                                       as.numeric(fixef(mod_flo_if_SP_CN)[3]))
fo_SP_CN_precip2 <- fo_SP_CN

fo_SP_CN@fitFec[[1]]$coefficients <- c(as.numeric(fixef(mod_flo_if_SP_CN)[1]) + 3.4*as.numeric(fixef(mod_flo_if_SP_CN)[4]),
                                       as.numeric(fixef(mod_flo_if_SP_CN)[2]),
                                       as.numeric(fixef(mod_flo_if_SP_CN)[3]))
fo_SP_CN_precip3 <- fo_SP_CN

Fmatrix_SP_CN_precip1 <- makeIPMFmatrix(fecObj=fo_SP_CN_precip1, minSize=minSize_SP, maxSize=maxSize_SP, correction = "continuous", nBigMatrix = 100)
Fmatrix_SP_CN_precip2 <- makeIPMFmatrix(fecObj=fo_SP_CN_precip2, minSize=minSize_SP, maxSize=maxSize_SP, correction = "continuous", nBigMatrix = 100)
Fmatrix_SP_CN_precip3 <- makeIPMFmatrix(fecObj=fo_SP_CN_precip3, minSize=minSize_SP, maxSize=maxSize_SP, correction = "continuous", nBigMatrix = 100)

#Plotting the matrix
contourPlot2(t(Fmatrix_SP_CN_precip1), Fmatrix_SP_CN_precip1@meshpoints, maxSize, 0.003, 0, title = "Fmatrix: flower and seedlings")
contourPlot2(t(Fmatrix_SP_CN_precip2), Fmatrix_SP_CN_precip2@meshpoints, maxSize, 0.003, 0, title = "Fmatrix: flower and seedlings")
contourPlot2(t(Fmatrix_SP_CN_precip3), Fmatrix_SP_CN_precip3@meshpoints, maxSize, 0.003, 0, title = "Fmatrix: flower and seedlings")

# image.plot(Fmatrix_VA_CC@meshpoints,
#            Fmatrix_VA_CC@meshpoints,
#            t(Fmatrix_VA_CC),
#            main = "Fmatrix: flower and seedlings",
#            xlab = "Size at t",
#            ylab = "Size at t+1")
# 
# image(t(Fmatrix_VA_CC))

#### C matrix ####

SP_CN_clones <- SP_CN %>% 
  filter(offspringNext == "clonal") %>% 
  mutate(number_orphans = case_when(is.na(size) ~ 1,
                                    !is.na(size) ~0)) %>% 
  mutate(total_num_orphan = sum(number_orphans),
         total_num_clones = n()) %>% 
  fill(total_num_orphan, .direction = "downup") %>% 
  mutate(prop_orphan = total_num_orphan/total_num_clones)

#Is the production of clones size dependent
# Running linear models because all the random effects gave singularity warning. I tried block_trans, site_trans, transition + blockID, transition + siteID, transition, blockID and siteID.
summary(glm(clo.if ~ size+I(size^2)+I(size^3) + precip+I(precip^2), family = 'binomial', data = SP_CN))
AIC(glm(clo.if ~ size+I(size^2)+I(size^3) + precip+I(precip^2), family = 'binomial', data = SP_CN))
summary(glm(clo.if ~ size+I(size^2)+I(size^3) + precip, family = 'binomial', data = SP_CN))
AIC(glm(clo.if ~ size+I(size^2)+I(size^3) + precip, family = 'binomial', data = SP_CN))
summary(glm(clo.if ~ size+I(size^2)+I(size^3) , family = 'binomial', data = SP_CN))
AIC(glm(clo.if ~ size+I(size^2)+I(size^3), family = 'binomial', data = SP_CN))
summary(glm(clo.if ~ size+I(size^2) + precip+I(precip^2), family = 'binomial', data = SP_CN))
AIC(glm(clo.if ~ size+I(size^2) + precip+I(precip^2), family = 'binomial', data = SP_CN))
summary(glm(clo.if ~ size+I(size^2) + precip, family = 'binomial', data = SP_CN))
AIC(glm(clo.if ~ size+I(size^2) + precip, family = 'binomial', data = SP_CN))
summary(glm(clo.if ~ size+I(size^2), family = 'binomial', data = SP_CN)) #Choosing thins model based of AIC
AIC(glm(clo.if ~ size+I(size^2), family = 'binomial', data = SP_CN))
summary(glm(clo.if ~ size, family = 'binomial', data = SP_CN))
AIC(glm(clo.if ~ size, family = 'binomial', data = SP_CN))
summary(glm(clo.if ~ 1, family = 'binomial', data = SP_CN))
AIC(glm(clo.if ~ 1, family = 'binomial', data = SP_CN))


mod_clo_SP_CN <- glm(clo.if ~ size+I(size^2), family = 'binomial', data = SP_CN)
CloneChosenModel_SP_CN <- clo.if ~ size + size2 


plot_clo_if_SP_CN <- plot_predictions_cloif(model = mod_clo_SP_CN, data = SP_CN, minSize_SP, maxSize_SP)
plot_clo_if_SP_CN

#If you produce clones, does how many clones you make change with size of the mother 
# Running linear models because all the random effects gave singularity warning. I tried block_trans, site_trans, transition + blockID, transition + siteID, transition, blockID and siteID.
summary(glm(clo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = SP_CN))
AIC(glm(clo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = SP_CN))
summary(glm(clo.no ~ size+I(size^2) + precip, family = 'poisson', data = SP_CN))
AIC(glm(clo.no ~ size+I(size^2) + precip, family = 'poisson', data = SP_CN))
summary(glm(clo.no ~ size + precip, family = 'poisson', data = SP_CN))
AIC(glm(clo.no ~ size + precip, family = 'poisson', data = SP_CN))
summary(glm(clo.no ~ size+I(size^2), family = 'poisson', data = SP_CN))
AIC(glm(clo.no ~ size+I(size^2), family = 'poisson', data = SP_CN))
summary(glm(clo.no ~ size, family = 'poisson', data = SP_CN))
AIC(glm(clo.no ~ size, family = 'poisson', data = SP_CN))
summary(glm(clo.no ~ 1, family = 'poisson', data = SP_CN)) #Chosing this model based of AIC
AIC(glm(clo.no ~ 1, family = 'poisson', data = SP_CN))

mod_clo_no_SP_CN <- glm(clo.no ~ 1, family = 'poisson', data = SP_CN)
CloneNumberChosenModel_SP_CN <- clo.no ~ 1


plot_clo_no_SP_CN <- plot_predictions_clono(model = mod_clo_no_SP_CN, data = SP_CN, minSize_SP, maxSize_SP, ylim = 6)
plot_clo_no_SP_CN

# Clonal size depending on mother size
summary(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = SP_CN_clones))
AIC(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = SP_CN_clones))
summary(lmer(sizeNext ~ size + precip+I(precip^2) + (1|block_trans), data = SP_CN_clones))
AIC(lmer(sizeNext ~ size + precip+I(precip^2) + (1|block_trans), data = SP_CN_clones))
summary(lmer(sizeNext ~ size + precip + (1|block_trans), data = SP_CN_clones)) 
AIC(lmer(sizeNext ~ size + precip + (1|block_trans), data = SP_CN_clones))
summary(lmer(sizeNext ~ precip+I(precip^2) + (1|block_trans), data = SP_CN_clones))
AIC(lmer(sizeNext ~ precip+I(precip^2) + (1|block_trans), data = SP_CN_clones))
summary(lmer(sizeNext ~ precip + (1|block_trans), data = SP_CN_clones))
AIC(lmer(sizeNext ~ precip + (1|block_trans), data = SP_CN_clones))
summary(lmer(sizeNext ~ size + (1|block_trans), data = SP_CN_clones)) #Best AIC
AIC(lmer(sizeNext ~ size + (1|block_trans), data = SP_CN_clones))
summary(lmer(sizeNext ~ 1 + (1|block_trans), data = SP_CN_clones)) #Using this model because it doesn't make biological sense that the clone size decreases with parent size.
AIC(lmer(sizeNext ~ 1 + (1|block_trans), data = SP_CN_clones))


mod_clone_growth_SP_CN <- lmer(sizeNext ~ 1 + (1|block_trans), data = SP_CN_clones)
CloneSizeVariable_SP_CN <- "1"

plot_clone_growth_SP_CN <- plot_predictions_growth(model = mod_clone_growth_SP_CN, data = SP_CN_clones, minSize_SP, maxSize_SP)
plot_clone_growth_SP_CN

#Make clonal object
co_SP_CN <- makeClonalObj(SP_CN, fecConstants=data.frame(correctionForOrphans= 1/(1-SP_CN_clones$prop_orphan[1])),
                          offspringSizeExplanatoryVariables = CloneSizeVariable_SP_CN, Formula = c(CloneChosenModel_SP_CN, CloneNumberChosenModel_SP_CN),
                          Family = c("binomial","poisson"), Transform=c("none","none"),offspringSplitter=data.frame(seedbank=0,continuous=1))

co_SP_CN@fitFec[[1]]$coefficients <- as.numeric(coef(mod_clo_SP_CN)) #not really needed since this is a linear model
co_SP_CN@fitFec[[2]]$coefficients <- as.numeric(coef(mod_clo_no_SP_CN)) #not really needed since this is a linear model
co_SP_CN@offspringRel$coefficients <- as.numeric(fixef(mod_clone_growth_SP_CN))
co_SP_CN@sdOffspringSize <- sigma.hat(mod_clone_growth_SP_CN)$sigma$data
co_SP_CN <- co_SP_CN


Cmatrix_SP_CN <- makeIPMCmatrix(clonalObj = co_SP_CN, minSize=minSize_SP, maxSize=maxSize_SP, nBigMatrix = 100, correction = "constant")

contourPlot2(t(Cmatrix_SP_CN), Cmatrix_SP_CN@meshpoints, maxSize_SP, 0.03, 0, title = "Cmatrix: clones") 


#### Build IPM ####
IPM_SP_CN_precip1 <- Pmatrix_SP_CN_precip1 + Fmatrix_SP_CN_precip1 + Cmatrix_SP_CN
IPM_SP_CN_precip2 <- Pmatrix_SP_CN_precip2 + Fmatrix_SP_CN_precip2 + Cmatrix_SP_CN
IPM_SP_CN_precip3 <- Pmatrix_SP_CN_precip3 + Fmatrix_SP_CN_precip3 + Cmatrix_SP_CN
#contourPlot2(t( M = IPM_SP_CN_precip1, meshpts = Pmatrix_SP_CN_precip1@meshpoints, maxSize = maxSize, lower = 0.03, upper = 0))
IPM_plot(IPM_control = IPM_SP_CN_precip1, minSize = minSize_SP, maxSize = maxSize_SP, zrange = c(-0.03, 0.06)) + ggtitle("Sibbaldia procumbens CN 1.2 m/year")
IPM_plot(IPM_control = IPM_SP_CN_precip2, minSize = minSize_SP, maxSize = maxSize_SP, zrange = c(-0.03, 0.06)) + ggtitle("Sibbaldia procumbens CN 2.3 m/year")
IPM_plot(IPM_control = IPM_SP_CN_precip3, minSize = minSize_SP, maxSize = maxSize_SP, zrange = c(-0.03, 0.06)) + ggtitle("Sibbaldia procumbens CN 3.4 m/year")
#persp(IPM_SP_CN)
as.numeric(eigen(IPM_SP_CN_precip1)$value[1])
as.numeric(eigen(IPM_SP_CN_precip2)$value[1])
as.numeric(eigen(IPM_SP_CN_precip3)$value[1])



##### Warming control #####

#### P matrix ####

# choosing the best survival model
summary(glmer(surv ~ size+I(size^2)+I(size^3) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WC))
AIC(glmer(surv ~ size+I(size^2)+I(size^3) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WC))
summary(glmer(surv ~ size+I(size^2)+I(size^3) + precip+ (1|block_trans), family = 'binomial', data = SP_WC))
AIC(glmer(surv ~ size+I(size^2)+I(size^3) + precip + (1|block_trans), family = 'binomial', data = SP_WC))
summary(glmer(surv ~ size+I(size^2)+I(size^3) + (1|block_trans), family = 'binomial', data = SP_WC))
AIC(glmer(surv ~ size+I(size^2)+I(size^3) + (1|block_trans), family = 'binomial', data = SP_WC))
summary(glmer(surv ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WC))
AIC(glmer(surv ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WC))
summary(glmer(surv ~ size + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WC))
AIC(glmer(surv ~ size + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WC))
summary(glmer(surv ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_WC))
AIC(glmer(surv ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_WC))
summary(glmer(surv ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_WC)) #We chose this model based on AIC
AIC(glmer(surv ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_WC))

summary(glmer(surv ~ size + precip + (1|block_trans), family = 'binomial', data = SP_WC))
AIC(glmer(surv ~ size + precip + (1|block_trans), family = 'binomial', data = SP_WC))
summary(glmer(surv ~ size + (1|block_trans), family = 'binomial', data = SP_WC))
AIC(glmer(surv ~ size + (1|block_trans), family = 'binomial', data = SP_WC))
summary(glmer(surv ~ 1 + (1|block_trans), family = 'binomial', data = SP_WC))
AIC(glmer(surv ~ 1 + (1|block_trans), family = 'binomial', data = SP_WC))

#Checking the convergence, because of a warning. Looks ok.
glmer(surv ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_WC, verbose = TRUE) 

mod_surv_SP_WC <- glmer(surv ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_WC)
plot_surv_SP_WC <- plot_predictions_surv(model = mod_surv_SP_WC, data = SP_WC, minSize_SP, maxSize_SP)

plot_surv_SP_WC

so_SP_WC <- makeSurvObj(SP_WC, "surv ~ size + size2")
so_SP_WC <- coerceSurvObj(so_SP_WC, as.numeric(fixef(mod_surv_SP_WC))) #Adding coefficients from mixed effect model and not from the linear model as is default in makeSurvObj

# choosing the bext growth model
summary(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = SP_WC))
AIC(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = SP_WC))
summary(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = SP_WC)) 
AIC(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = SP_WC))
summary(lmer(sizeNext ~ size+I(size^2) + (1|block_trans), data = SP_WC))
AIC(lmer(sizeNext ~ size+I(size^2) + (1|block_trans), data = SP_WC)) 
summary(lmer(sizeNext ~ size + (1|block_trans), data = SP_WC))#We chose this model based on AIC
AIC(lmer(sizeNext ~ size + (1|block_trans), data = SP_WC))
summary(lmer(sizeNext ~ 1 + (1|block_trans), data = SP_WC))
AIC(lmer(sizeNext ~ 1 + (1|block_trans), data = SP_WC))


mod_growth_SP_WC <- lmer(sizeNext ~ size + (1|block_trans), data = SP_WC)

plot_growth_SP_WC <- plot_predictions_growth(model = mod_growth_SP_WC, data = SP_WC, minSize_SP, maxSize_SP)

plot_growth_SP_WC


go_SP_WC <- makeGrowthObj(SP_WC, "sizeNext ~ size")
go_SP_WC <- coerceGrowthObj(go_SP_WC, fixef(mod_growth_SP_WC),
                            sigma.hat(mod_growth_SP_WC)$sigma$data)


# Make discrete transition object
dto_SP_WC <- makeDiscreteTrans(SP_WC, discreteTrans = matrix(
  c(SP_OTC_seed_bank$seeds_staySB,
    (1-SP_OTC_seed_bank$seeds_staySB)*seedling_est_SP_Veg,
    (1-SP_OTC_seed_bank$seeds_staySB)*(1-seedling_est_SP_Veg), 
    0,
    sum(SP_WC$number[SP_WC$stage=="continuous"&SP_WC$stageNext=="continuous"], na.rm=T),
    sum(SP_WC$number[SP_WC$stage=="continuous"&SP_WC$stageNext=="dead"], na.rm=T)),
  ncol = 2,
  nrow = 3, 
  dimnames = list(c("seedbank", "continuous", "dead"), c("seedbank", "continuous"))),
  meanToCont = matrix(Seedling_info_SP$mean_Veg, ncol = 1, nrow = 1, dimnames = list(c("mean"), c("seedbank"))),
  sdToCont = matrix(Seedling_info_SP$sd, ncol = 1, nrow = 1, dimnames = list(c(""),c("seedbank"))))


# With these survival and growth objects in hand, we build a survival/growth (P) matrix.
Pmatrix_SP_WC <- makeIPMPmatrix(survObj=so_SP_WC, growObj=go_SP_WC, minSize=minSize_SP, maxSize=maxSize_SP, correction = "constant", nBigMatrix = 100)

diagnosticsPmatrix(Pmatrix_SP_WC, survObj=so_SP_WC, growObj=go_SP_WC, dff = SP_WC)

Pmatrix_SP_WC <- makeIPMPmatrix(survObj=so_SP_WC, growObj=go_SP_WC, minSize=minSize_SP, maxSize=maxSize_SP, discreteTrans = dto_SP_WC, correction = "constant", nBigMatrix = 100)
x11()
contourPlot2(t(Pmatrix_SP_WC), Pmatrix_SP_WC@meshpoints, maxSize_SP, 0.03, 0, title = "Pmatrix: survival and growth") 


#### F matrix ####
# Choosing the best model for estimating if an individual flowers
summary(glmer(flo.if ~ size+I(size^2)+I(size^3) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WC))
AIC(glmer(flo.if ~ size+I(size^2)+I(size^3) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WC))
summary(glmer(flo.if ~ size+I(size^2)+I(size^3) + precip + (1|block_trans), family = 'binomial', data = SP_WC))
AIC(glmer(flo.if ~ size+I(size^2)+I(size^3) + precip + (1|block_trans), family = 'binomial', data = SP_WC))
summary(glmer(flo.if ~ size+I(size^2)+I(size^3) + (1|block_trans), family = 'binomial', data = SP_WC))
AIC(glmer(flo.if ~ size+I(size^2)+I(size^3) + (1|block_trans), family = 'binomial', data = SP_WC))

summary(glmer(flo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WC))
AIC(glmer(flo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WC))
summary(glmer(flo.if ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_WC)) 
AIC(glmer(flo.if ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_WC))
summary(glmer(flo.if ~ size + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WC))
AIC(glmer(flo.if ~ size + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WC))
summary(glmer(flo.if ~ size + precip + (1|block_trans), family = 'binomial', data = SP_WC)) #Choosing this model based of AIC
AIC(glmer(flo.if ~ size + precip + (1|block_trans), family = 'binomial', data = SP_WC))
summary(glmer(flo.if ~ size + (1|block_trans), family = 'binomial', data = SP_WC))
AIC(glmer(flo.if ~ size + (1|block_trans), family = 'binomial', data = SP_WC)) 
summary(glmer(flo.if ~ 1 + (1|block_trans), family = 'binomial', data = SP_WC))
AIC(glmer(flo.if ~ 1 + (1|block_trans), family = 'binomial', data = SP_WC))

floweringChosenModel_SP_WC <- flo.if ~ size
mod_flo_if_SP_WC <- glmer(flo.if ~ size + precip + (1|block_trans), family = 'binomial', data = SP_WC) 
plot_SP_WC_floif <- plot_predictions_floif_precip(model = mod_flo_if_SP_WC, data = SP_WC, minSize_SP, maxSize_SP)
plot_SP_WC_floif 


# Choosing the best model for estimating the number of flowers, if an individual flowers
# Running linear models because all the random effects gave singularity warning. I tried block_trans, site_trans, transition + blockID, transition + siteID, transition, blockID and siteID.
summary(glm(flo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = SP_WC))
AIC(glm(flo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = SP_WC))
summary(glm(flo.no ~ size+I(size^2) + precip, family = 'poisson', data = SP_WC)) 
AIC(glm(flo.no ~ size+I(size^2) + precip, family = 'poisson', data = SP_WC))
summary(glm(flo.no ~ size +I(size^2), family = 'poisson', data = SP_WC)) 
AIC(glm(flo.no ~ size +I(size^2), family = 'poisson', data = SP_WC))
summary(glm(flo.no ~ size, family = 'poisson', data = SP_WC)) 
AIC(glm(flo.no ~ size, family = 'poisson', data = SP_WC)) 
summary(glm(flo.no ~ 1, family = 'poisson', data = SP_WC))#Choosing this model because the other ones are highly affected by outliers
AIC(glm(flo.no ~ 1, family = 'poisson', data = SP_WC))


flowerNumberChosenModel_SP_WC <- flo.no ~ 1   #Chosen based on biology by looking at the data

mod_flo_no_SP_WC <- glm(flo.no ~ 1, family = 'poisson', data = SP_WC)

plot_flo_no_SP_WC <-plot_predictions_flono(model = mod_flo_no_SP_WC, data = SP_WC, minSize_SP, maxSize_SP, ylim = 15) 

plot_flo_no_SP_WC

# Make fecundity object
fo_SP_WC <-makeFecObj(SP_WC, 
                      Formula= c(floweringChosenModel_SP_WC, flowerNumberChosenModel_SP_WC),
                      Family = c("binomial", "poisson"),
                      fecConstants = data.frame(seedsPerCap = Seeds_per_capsule_SP,
                                                seedlingEstablishmentRate = seedling_est_SP_Veg), 
                      meanOffspringSize = Seedling_info_SP$mean_Veg,
                      sdOffspringSize = Seedling_info_SP$sd,
                      offspringSplitter = data.frame(seedbank=SP_OTC_seed_bank$seeds_alive_total_prop* (1-seedling_est_SP_Veg), continuous=(1-(SP_OTC_seed_bank$seeds_alive_total_prop* (1-seedling_est_SP_Veg)))),
                      vitalRatesPerOffspringType = data.frame(seedbank=c(1,1,1,0), continuous=c(1,1,1,1),
                                                              row.names=c("flo.if","flo.no","seedsPerCap","seedlingEstablishmentRate")))

fo_SP_WC@fitFec[[2]]$coefficients <- as.numeric(coef(mod_flo_no_SP_WC))
fo_SP_WC@fitFec[[1]]$coefficients <- c(as.numeric(fixef(mod_flo_if_SP_WC)[1]) + 1.2*as.numeric(fixef(mod_flo_if_SP_WC)[3]),
                                       as.numeric(fixef(mod_flo_if_SP_WC)[2]))

fo_SP_WC_precip1 <- fo_SP_WC

fo_SP_WC@fitFec[[1]]$coefficients <- c(as.numeric(fixef(mod_flo_if_SP_WC)[1]) + 2.3*as.numeric(fixef(mod_flo_if_SP_WC)[3]),
                                       as.numeric(fixef(mod_flo_if_SP_WC)[2]))

fo_SP_WC_precip2 <- fo_SP_WC

fo_SP_WC@fitFec[[1]]$coefficients <- c(as.numeric(fixef(mod_flo_if_SP_WC)[1]) + 3.4*as.numeric(fixef(mod_flo_if_SP_WC)[3]),
                                       as.numeric(fixef(mod_flo_if_SP_WC)[2]))

fo_SP_WC_precip3 <- fo_SP_WC

Fmatrix_SP_WC_precip1 <- makeIPMFmatrix(fecObj=fo_SP_WC_precip1, minSize=minSize_SP, maxSize=maxSize_SP, correction = "continuous", nBigMatrix = 100)
Fmatrix_SP_WC_precip2 <- makeIPMFmatrix(fecObj=fo_SP_WC_precip2, minSize=minSize_SP, maxSize=maxSize_SP, correction = "continuous", nBigMatrix = 100)
Fmatrix_SP_WC_precip3 <- makeIPMFmatrix(fecObj=fo_SP_WC_precip3, minSize=minSize_SP, maxSize=maxSize_SP, correction = "continuous", nBigMatrix = 100)

#Plotting the matrix
contourPlot2(t(Fmatrix_SP_WC_precip1), Fmatrix_SP_WC@meshpoints, maxSize, 0.003, 0, title = "Fmatrix: flower and seedlings")

# image.plot(Fmatrix_VA_WC@meshpoints,
#            Fmatrix_VA_WC@meshpoints,
#            t(Fmatrix_VA_WC),
#            main = "Fmatrix: flower and seedlings",
#            xlab = "Size at t",
#            ylab = "Size at t+1")
# 
# image(t(Fmatrix_VA_WC))

#### C matrix ####

SP_WC_clones <- SP_WC %>% 
  filter(offspringNext == "clonal") %>% 
  mutate(number_orphans = case_when(is.na(size) ~ 1,
                                    !is.na(size) ~0)) %>% 
  mutate(total_num_orphan = sum(number_orphans),
         total_num_clones = n()) %>% 
  fill(total_num_orphan, .direction = "downup") %>% 
  mutate(prop_orphan = total_num_orphan/total_num_clones)

#Is the production of clones size dependent
#Running linear models because all the random effects gave singularity warning. I tried block_trans, site_trans, transition + blockID, transition + siteID, transition, blockID and siteID.
summary(glm(clo.if ~ size+I(size^2)+I(size^3) + precip+I(precip^2), family = 'binomial', data = SP_WC)) 
AIC(glm(clo.if ~ size+I(size^2)+I(size^3) + precip+I(precip^2) , family = 'binomial', data = SP_WC))
summary(glm(clo.if ~ size+I(size^2)+I(size^3) + precip, family = 'binomial', data = SP_WC)) 
AIC(glm(clo.if ~ size+I(size^2)+I(size^3) + precip, family = 'binomial', data = SP_WC))
summary(glm(clo.if ~ size+I(size^2)+I(size^3), family = 'binomial', data = SP_WC)) 
AIC(glm(clo.if ~ size+I(size^2)+I(size^3), family = 'binomial', data = SP_WC))
summary(glm(clo.if ~ size+I(size^2) + precip+I(precip^2), family = 'binomial', data = SP_WC)) 
AIC(glm(clo.if ~ size+I(size^2) + precip+I(precip^2) , family = 'binomial', data = SP_WC))
summary(glm(clo.if ~ size+I(size^2) + precip , family = 'binomial', data = SP_WC))
AIC(glm(clo.if ~ size+I(size^2) + precip , family = 'binomial', data = SP_WC))
summary(glm(clo.if ~ size + precip , family = 'binomial', data = SP_WC))
AIC(glm(clo.if ~ size + precip , family = 'binomial', data = SP_WC))
summary(glm(clo.if ~ size+I(size^2) , family = 'binomial', data = SP_WC)) #Choosing this model based of AIC
AIC(glm(clo.if ~ size+I(size^2), family = 'binomial', data = SP_WC))
summary(glm(clo.if ~ size, family = 'binomial', data = SP_WC))
AIC(glm(clo.if ~ size, family = 'binomial', data = SP_WC))
summary(glm(clo.if ~ 1, family = 'binomial', data = SP_WC))
AIC(glm(clo.if ~ 1 , family = 'binomial', data = SP_WC))


mod_clo_SP_WC <- glm(clo.if ~ size+I(size^2), family = 'binomial', data = SP_WC)
CloneChosenModel_SP_WC <- clo.if ~ size + size2 


plot_clo_if_SP_WC <- plot_predictions_cloif(model = mod_clo_SP_WC, data = SP_WC, minSize_SP, maxSize_SP)
plot_clo_if_SP_WC

#If you produce clones, does how many clones you make change with size of the mother 
# Running linear models because all the random effects gave singularity warning. I tried block_trans, site_trans, transition + blockID, transition + siteID, transition, blockID and siteID.
summary(glm(clo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = SP_WC))
AIC(glm(clo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = SP_WC))
summary(glm(clo.no ~ size+I(size^2) + precip, family = 'poisson', data = SP_WC))
AIC(glm(clo.no ~ size+I(size^2) + precip, family = 'poisson', data = SP_WC))
summary(glm(clo.no ~ size+I(size^2) + precip, family = 'poisson', data = SP_WC))
AIC(glm(clo.no ~ size+I(size^2) + precip, family = 'poisson', data = SP_WC))
summary(glm(clo.no ~ size+I(size^2), family = 'poisson', data = SP_WC))
AIC(glm(clo.no ~ size+I(size^2), family = 'poisson', data = SP_WC))
summary(glm(clo.no ~ size, family = 'poisson', data = SP_WC))
AIC(glm(clo.no ~ size, family = 'poisson', data = SP_WC))
summary(glm(clo.no ~ 1, family = 'poisson', data = SP_WC)) #Choosing this model based of AIC
AIC(glm(clo.no ~ 1, family = 'poisson', data = SP_WC))

mod_clo_no_SP_WC <- glm(clo.no ~ 1, family = 'poisson', data = SP_WC)
CloneNumberChosenModel_SP_WC <- clo.no ~ 1


plot_clo_no_SP_WC <- plot_predictions_clono(model = mod_clo_no_SP_WC, data = SP_WC, minSize_SP, maxSize_SP, ylim = 6)
plot_clo_no_SP_WC

# Clonal size depending on mother size
# Using blockID as random effect. I tried block_trans, site_trans, transition + blockID, transition + siteID, transition as well.
summary(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|blockID), data = SP_WC_clones))
AIC(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|blockID), data = SP_WC_clones))
summary(lmer(sizeNext ~ size+I(size^2) + precip + (1|blockID), data = SP_WC_clones)) 
AIC(lmer(sizeNext ~ size+I(size^2) + precip + (1|blockID), data = SP_WC_clones))
summary(lmer(sizeNext ~ size + precip + (1|blockID), data = SP_WC_clones)) 
AIC(lmer(sizeNext ~ size + precip + (1|blockID), data = SP_WC_clones))
summary(lmer(sizeNext ~ size + (1|blockID), data = SP_WC_clones)) 
AIC(lmer(sizeNext ~ size + (1|blockID), data = SP_WC_clones)) #Using this model based of AIC
summary(lmer(sizeNext ~ 1 + (1|blockID), data = SP_WC_clones)) 
AIC(lmer(sizeNext ~ 1 + (1|blockID), data = SP_WC_clones))


mod_clone_growth_SP_WC <- lmer(sizeNext ~ size + (1|blockID), data = SP_WC_clones)
CloneSizeVariable_SP_WC <- "size"

plot_clone_growth_SP_WC <- plot_predictions_growth(model = mod_clone_growth_SP_WC, data = SP_WC_clones, minSize_SP, maxSize_SP)
plot_clone_growth_SP_WC

#Make clonal object
co_SP_WC <- makeClonalObj(SP_WC, fecConstants=data.frame(correctionForOrphans= 1/(1-SP_WC_clones$prop_orphan[1])),
                          offspringSizeExplanatoryVariables = CloneSizeVariable_SP_WC, Formula = c(CloneChosenModel_SP_WC, CloneNumberChosenModel_SP_WC),
                          Family = c("binomial","poisson"), Transform=c("none","none"),offspringSplitter=data.frame(seedbank=0,continuous=1))

co_SP_WC@fitFec[[1]]$coefficients <- as.numeric(coef(mod_clo_SP_WC))
co_SP_WC@fitFec[[2]]$coefficients <- as.numeric(coef(mod_clo_no_SP_WC)) #not really needed since this is a linear model
co_SP_WC@offspringRel$coefficients <- as.numeric(fixef(mod_clone_growth_SP_WC))
co_SP_WC@sdOffspringSize <- sigma.hat(mod_clone_growth_SP_WC)$sigma$data



Cmatrix_SP_WC <- makeIPMCmatrix(clonalObj = co_SP_WC, minSize=minSize_SP, maxSize=maxSize_SP, nBigMatrix = 100, correction = "constant")

contourPlot2(t(Cmatrix_SP_WC), Cmatrix_SP_WC@meshpoints, maxSize_SP, 0.03, 0, title = "Cmatrix: clones") 


#### Build IPM ####
IPM_SP_WC_precip1 <- Pmatrix_SP_WC + Fmatrix_SP_WC_precip1 + Cmatrix_SP_WC
#contourPlot2(t( M = IPM_SP_CC_precip1, meshpts = Pmatrix_SP_CC_precip1@meshpoints, maxSize = maxSize, lower = 0.03, upper = 0))
IPM_plot(IPM_control = IPM_SP_WC_precip1, minSize = minSize_SP, maxSize = maxSize_SP, zrange = c(-0.03, 0.06)) + ggtitle("Sibbaldia procumbens WC precip 1.2 m/year")
#persp(IPM_SP_CC)
as.numeric(eigen(IPM_SP_WC_precip1)$value[1])

IPM_SP_WC_precip2 <- Pmatrix_SP_WC + Fmatrix_SP_WC_precip2 + Cmatrix_SP_WC
#contourPlot2(t( M = IPM_SP_CC_precip1, meshpts = Pmatrix_SP_CC_precip1@meshpoints, maxSize = maxSize, lower = 0.03, upper = 0))
IPM_plot(IPM_control = IPM_SP_WC_precip2, minSize = minSize_SP, maxSize = maxSize_SP, zrange = c(-0.03, 0.06)) + ggtitle("Sibbaldia procumbens WC precip 2.3 m/year")
#persp(IPM_SP_CC)
as.numeric(eigen(IPM_SP_WC_precip2)$value[1])

IPM_SP_WC_precip3 <- Pmatrix_SP_WC + Fmatrix_SP_WC_precip3 + Cmatrix_SP_WC
#contourPlot2(t( M = IPM_SP_CC_precip1, meshpts = Pmatrix_SP_CC_precip1@meshpoints, maxSize = maxSize, lower = 0.03, upper = 0))
IPM_plot(IPM_control = IPM_SP_WC_precip3, minSize = minSize_SP, maxSize = maxSize_SP, zrange = c(-0.03, 0.06)) + ggtitle("Sibbaldia procumbens WC precip 3.4 m/year")
#persp(IPM_SP_CC)
as.numeric(eigen(IPM_SP_WC_precip3)$value[1])


x11()
contourPlot2(t(IPM_SP_CC), Pmatrix_SP_CC@meshpoints, maxSize_SP, 0.06, 0, title = "Sibbaldia procumbensCC ")

##### Warming removal #####

#### P matrix ####

# choosing the best survival model
summary(glmer(surv ~ size+I(size^2)+I(size^3) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WR))
AIC(glmer(surv ~ size+I(size^2)+I(size^3) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WR))
summary(glmer(surv ~ size+I(size^2)+I(size^3) + precip + (1|block_trans), family = 'binomial', data = SP_WR))
AIC(glmer(surv ~ size+I(size^2)+I(size^3) + precip + (1|block_trans), family = 'binomial', data = SP_WR))
summary(glmer(surv ~ size+I(size^2)+I(size^3) + (1|block_trans), family = 'binomial', data = SP_WR))
AIC(glmer(surv ~ size+I(size^2)+I(size^3) + (1|block_trans), family = 'binomial', data = SP_WR))

summary(glmer(surv ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WR))
AIC(glmer(surv ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WR))
summary(glmer(surv ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_WR))
AIC(glmer(surv ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_WR))
summary(glmer(surv ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_WR)) #Using this model based of AIC
AIC(glmer(surv ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_WR))
summary(glmer(surv ~ size + (1|block_trans), family = 'binomial', data = SP_WR))
AIC(glmer(surv ~ size + (1|block_trans), family = 'binomial', data = SP_WR))
summary(glmer(surv ~ 1 + (1|block_trans), family = 'binomial', data = SP_WR))
AIC(glmer(surv ~ 1 + (1|block_trans), family = 'binomial', data = SP_WR))

mod_surv_SP_WR <- glmer(surv ~ size +I(size^2) + (1|block_trans), family = 'binomial', data = SP_WR)
plot_surv_SP_WR <- plot_predictions_surv(model = mod_surv_SP_WR, data = SP_WR, minSize_SP, maxSize_SP)

plot_surv_SP_WR

so_SP_WR <- makeSurvObj(SP_WR, "surv ~ size + size2")
so_SP_WR <- coerceSurvObj(so_SP_WR, as.numeric(fixef(mod_surv_SP_WR))) #Adding coefficients from mixed effect model and not from the linear model as is default in makeSurvObj

# choosing the best growth model
summary(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = SP_WR))
AIC(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = SP_WR))
summary(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = SP_WR)) 
AIC(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = SP_WR))
summary(lmer(sizeNext ~ size+I(size^2) + (1|block_trans), data = SP_WR))#We chose this model based on AIC
AIC(lmer(sizeNext ~ size+I(size^2) + (1|block_trans), data = SP_WR)) 
summary(lmer(sizeNext ~ size + (1|block_trans), data = SP_WR))
AIC(lmer(sizeNext ~ size + (1|block_trans), data = SP_WR))
summary(lmer(sizeNext ~ 1 + (1|block_trans), data = SP_WR))
AIC(lmer(sizeNext ~ 1 + (1|block_trans), data = SP_WR))


mod_growth_SP_WR <- lmer(sizeNext ~ size + I(size^2) + (1|block_trans), data = SP_WR)

plot_growth_SP_WR <- plot_predictions_growth(model = mod_growth_SP_WR, data = SP_WR, minSize_SP, maxSize_SP)

plot_growth_SP_WR


go_SP_WR <- makeGrowthObj(SP_WR, "sizeNext ~ size + size2")
go_SP_WR <- coerceGrowthObj(go_SP_WR, fixef(mod_growth_SP_WR),
                            sigma.hat(mod_growth_SP_WR)$sigma$data)


# Make discrete transition object
dto_SP_WR <- makeDiscreteTrans(SP_WR, discreteTrans = matrix(
  c(SP_OTC_seed_bank$seeds_staySB,
    (1-SP_OTC_seed_bank$seeds_staySB)*seedling_est_SP_NoVeg,
    (1-SP_OTC_seed_bank$seeds_staySB)*(1-seedling_est_SP_NoVeg), 
    0,
    sum(SP_WR$number[SP_WR$stage=="continuous"&SP_WR$stageNext=="continuous"], na.rm=T),
    sum(SP_WR$number[SP_WR$stage=="continuous"&SP_WR$stageNext=="dead"], na.rm=T)),
  ncol = 2,
  nrow = 3, 
  dimnames = list(c("seedbank", "continuous", "dead"), c("seedbank", "continuous"))),
  meanToCont = matrix(Seedling_info_SP$mean_NoVeg, ncol = 1, nrow = 1, dimnames = list(c("mean"), c("seedbank"))),
  sdToCont = matrix(Seedling_info_SP$sd, ncol = 1, nrow = 1, dimnames = list(c(""),c("seedbank"))))


# With these survival and growth objects in hand, we build a survival/growth (P) matrix.
Pmatrix_SP_WR <- makeIPMPmatrix(survObj=so_SP_WR, growObj=go_SP_WR, minSize=minSize_SP, maxSize=maxSize_SP, correction = "constant", nBigMatrix = 100)

diagnosticsPmatrix(Pmatrix_SP_WR, survObj=so_SP_WR, growObj=go_SP_WR, dff = SP_WR)

Pmatrix_SP_WR <- makeIPMPmatrix(survObj=so_SP_WR, growObj=go_SP_WR, minSize=minSize_SP, maxSize=maxSize_SP, discreteTrans = dto_SP_WR, correction = "constant", nBigMatrix = 100)
x11()
contourPlot2(t(Pmatrix_SP_WR), Pmatrix_SP_WR@meshpoints, maxSize_SP, 0.03, 0, title = "Pmatrix: survival and growth") 


#### F matrix ####
# Choosing the best model for estimating if an individual flowers
summary(glmer(flo.if ~ size+I(size^2)+I(size^3) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WR))
AIC(glmer(flo.if ~ size+I(size^2)+I(size^3) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WR))
summary(glmer(flo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WR))
AIC(glmer(flo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WR))
summary(glmer(flo.if ~ size + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WR))
AIC(glmer(flo.if ~ size + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WR))
summary(glmer(flo.if ~ size + precip + (1|block_trans), family = 'binomial', data = SP_WR)) #Choosing this model based of AIC
AIC(glmer(flo.if ~ size + precip + (1|block_trans), family = 'binomial', data = SP_WR))
summary(glmer(flo.if ~ size + (1|block_trans), family = 'binomial', data = SP_WR))
AIC(glmer(flo.if ~ size + (1|block_trans), family = 'binomial', data = SP_WR))
summary(glmer(flo.if ~ 1 + (1|block_trans), family = 'binomial', data = SP_WR))
AIC(glmer(flo.if ~ 1 + (1|block_trans), family = 'binomial', data = SP_WR))

floweringChosenModel_SP_WR <- flo.if ~ size
mod_flo_if_SP_WR <- glmer(flo.if ~ size + precip + (1|block_trans), family = 'binomial', data = SP_WR) 
plot_SP_WR_floif <- plot_predictions_floif_precip(model = mod_flo_if_SP_WR, data = SP_WR, minSize_SP, maxSize_SP)
plot_SP_WR_floif 


# Choosing the best model for estimating the number of flowers, if an individual flowers
summary(glmer(flo.no ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'poisson', data = SP_WR))
AIC(glmer(flo.no ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'poisson', data = SP_WR))
summary(glmer(flo.no ~ size+I(size^2) + precip + (1|block_trans), family = 'poisson', data = SP_WR))
AIC(glmer(flo.no ~ size+I(size^2) + precip + (1|block_trans), family = 'poisson', data = SP_WR))
summary(glmer(flo.no ~ size+I(size^2) + (1|block_trans), family = 'poisson', data = SP_WR))
AIC(glmer(flo.no ~ size+I(size^2) + (1|block_trans), family = 'poisson', data = SP_WR))
summary(glmer(flo.no ~ size + precip + (1|block_trans), family = 'poisson', data = SP_WR))
AIC(glmer(flo.no ~ size + precip + (1|block_trans), family = 'poisson', data = SP_WR))
summary(glmer(flo.no ~ size + (1|block_trans), family = 'poisson', data = SP_WR)) # best AIC
AIC(glmer(flo.no ~ size + (1|block_trans), family = 'poisson', data = SP_WR))
summary(glmer(flo.no ~ 1 + (1|block_trans), family = 'poisson', data = SP_WR)) #Chosing this model to have similar models for all sibbaldia treatments
AIC(glmer(flo.no ~ 1 + (1|block_trans), family = 'poisson', data = SP_WR))


flowerNumberChosenModel_SP_WR <- flo.no ~ 1 
mod_flo_no_SP_WR <- glmer(flo.no ~ 1 + (1|block_trans), family = 'poisson', data = SP_WR)

plot_flo_no_SP_WR <-plot_predictions_flono(model = mod_flo_no_SP_WR, data = SP_WR, minSize_SP, maxSize_SP, ylim = 15) 
plot_flo_no_SP_WR

# Make fecundity object
fo_SP_WR <-makeFecObj(SP_WR, 
                      Formula= c(floweringChosenModel_SP_WR, flowerNumberChosenModel_SP_WR),
                      Family = c("binomial", "poisson"),
                      fecConstants = data.frame(seedsPerCap = Seeds_per_capsule_SP,
                                                seedlingEstablishmentRate = seedling_est_SP_NoVeg), 
                      meanOffspringSize = Seedling_info_SP$mean_NoVeg,
                      sdOffspringSize = Seedling_info_SP$sd,
                      offspringSplitter = data.frame(seedbank=SP_OTC_seed_bank$seeds_alive_total_prop* (1-seedling_est_SP_NoVeg), continuous=(1-(SP_OTC_seed_bank$seeds_alive_total_prop* (1-seedling_est_SP_NoVeg)))),
                      vitalRatesPerOffspringType = data.frame(seedbank=c(1,1,1,0), continuous=c(1,1,1,1),
                                                              row.names=c("flo.if","flo.no","seedsPerCap","seedlingEstablishmentRate")))

fo_SP_WR@fitFec[[2]]$coefficients <- as.numeric(fixef(mod_flo_no_SP_WR))
fo_SP_WR@fitFec[[1]]$coefficients <- c(as.numeric(fixef(mod_flo_if_SP_WR)[1]) + 1.2*as.numeric(fixef(mod_flo_if_SP_WR)[3]),
                                       as.numeric(fixef(mod_flo_if_SP_WR)[2]))
fo_SP_WR_precip1 <- fo_SP_WR

fo_SP_WR@fitFec[[1]]$coefficients <- c(as.numeric(fixef(mod_flo_if_SP_WR)[1]) + 2.3*as.numeric(fixef(mod_flo_if_SP_WR)[3]),
                                       as.numeric(fixef(mod_flo_if_SP_WR)[2]))

fo_SP_WR_precip2 <- fo_SP_WR

fo_SP_WR@fitFec[[1]]$coefficients <- c(as.numeric(fixef(mod_flo_if_SP_WR)[1]) + 3.4*as.numeric(fixef(mod_flo_if_SP_WR)[3]),
                                       as.numeric(fixef(mod_flo_if_SP_WR)[2]))

fo_SP_WR_precip3 <- fo_SP_WR

Fmatrix_SP_WR_precip1 <- makeIPMFmatrix(fecObj=fo_SP_WR_precip1, minSize=minSize_SP, maxSize=maxSize_SP, correction = "continuous", nBigMatrix = 100)
Fmatrix_SP_WR_precip2 <- makeIPMFmatrix(fecObj=fo_SP_WR_precip2, minSize=minSize_SP, maxSize=maxSize_SP, correction = "continuous", nBigMatrix = 100)
Fmatrix_SP_WR_precip3 <- makeIPMFmatrix(fecObj=fo_SP_WR_precip3, minSize=minSize_SP, maxSize=maxSize_SP, correction = "continuous", nBigMatrix = 100)

#Plotting the matrix
contourPlot2(t(Fmatrix_SP_WR_precip1), Fmatrix_SP_WR@meshpoints, maxSize, 0.003, 0, title = "Fmatrix: flower and seedlings")

# image.plot(Fmatrix_VA_WR@meshpoints,
#            Fmatrix_VA_WR@meshpoints,
#            t(Fmatrix_VA_WR),
#            main = "Fmatrix: flower and seedlings",
#            xlab = "Size at t",
#            ylab = "Size at t+1")
# 
# image(t(Fmatrix_VA_WR))

#### C matrix ####

SP_WR_clones <- SP_WR %>% 
  filter(offspringNext == "clonal") %>% 
  mutate(number_orphans = case_when(is.na(size) ~ 1,
                                    !is.na(size) ~0)) %>% 
  mutate(total_num_orphan = sum(number_orphans),
         total_num_clones = n()) %>% 
  fill(total_num_orphan, .direction = "downup") %>% 
  mutate(prop_orphan = total_num_orphan/total_num_clones)

#Is the production of clones size dependent
summary(glmer(clo.if ~ size+I(size^2)+I(size^3) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WR)) 
AIC(glmer(clo.if ~ size+I(size^2)+I(size^3) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WR))
summary(glmer(clo.if ~ size+I(size^2)+I(size^3) + precip + (1|block_trans), family = 'binomial', data = SP_WR)) 
AIC(glmer(clo.if ~ size+I(size^2)+I(size^3) + precip + (1|block_trans), family = 'binomial', data = SP_WR))
summary(glmer(clo.if ~ size+I(size^2)+I(size^3) + (1|block_trans), family = 'binomial', data = SP_WR)) 
AIC(glmer(clo.if ~ size+I(size^2)+I(size^3) + (1|block_trans), family = 'binomial', data = SP_WR))

summary(glmer(clo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WR)) 
AIC(glmer(clo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WR))
summary(glmer(clo.if ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_WR)) 
AIC(glmer(clo.if ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_WR))
summary(glmer(clo.if ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_WR)) #Chose this model because the cubic model estimates a 100 % chance of producing clones for large individuals - which is not likely
AIC(glmer(clo.if ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_WR))
summary(glmer(clo.if ~ size + (1|block_trans), family = 'binomial', data = SP_WR)) 
AIC(glmer(clo.if ~ size + (1|block_trans), family = 'binomial', data = SP_WR))
summary(glmer(clo.if ~ 1 + (1|block_trans), family = 'binomial', data = SP_WR)) 
AIC(glmer(clo.if ~ 1 + (1|block_trans), family = 'binomial', data = SP_WR))

mod_clo_SP_WR <- glmer(clo.if ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_WR)
CloneChosenModel_SP_WR <- clo.if ~ size + size2 


plot_clo_if_SP_WR <- plot_predictions_cloif(model = mod_clo_SP_WR, data = SP_WR, minSize_SP, maxSize_SP)
plot_clo_if_SP_WR

#If you produce clones, does how many clones you make change with size of the mother 
# Running linear models because all the random effects gave singularity warning. I tried block_trans, site_trans, transition + blockID, transition + siteID, transition, blockID and siteID.
summary(glm(clo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = SP_WR))
AIC(glm(clo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = SP_WR))
summary(glm(clo.no ~ size + precip+I(precip^2), family = 'poisson', data = SP_WR))
AIC(glm(clo.no ~ size + precip+I(precip^2), family = 'poisson', data = SP_WR))
summary(glm(clo.no ~ size + precip, family = 'poisson', data = SP_WR))
AIC(glm(clo.no ~ size + precip, family = 'poisson', data = SP_WR))
summary(glm(clo.no ~ precip, family = 'poisson', data = SP_WR))
AIC(glm(clo.no ~ precip, family = 'poisson', data = SP_WR))
summary(glm(clo.no ~ size, family = 'poisson', data = SP_WR))
AIC(glm(clo.no ~ size, family = 'poisson', data = SP_WR))
summary(glm(clo.no ~ 1, family = 'poisson', data = SP_WR)) #Chosing this model based of AIC
AIC(glm(clo.no ~ 1, family = 'poisson', data = SP_WR))

mod_clo_no_SP_WR <- glm(clo.no ~ 1, family = 'poisson', data = SP_WR)
CloneNumberChosenModel_SP_WR <- clo.no ~ 1

plot_clo_no_SP_WR <- plot_predictions_clono(model = mod_clo_no_SP_WR, data = SP_WR, minSize_SP, maxSize_SP, ylim = 6)
plot_clo_no_SP_WR

# Clonal size depending on mother size
# Using blockID as random effect. I tried block_trans, site_trans, transition + blockID, transition + siteID, transition as well.
summary(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|site_trans), data = SP_WR_clones))
AIC(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|site_trans), data = SP_WR_clones))
summary(lmer(sizeNext ~ size+I(size^2) + precip + (1|site_trans), data = SP_WR_clones))
AIC(lmer(sizeNext ~ size+I(size^2) + precip + (1|site_trans), data = SP_WR_clones))
summary(lmer(sizeNext ~ size+I(size^2) + (1|site_trans), data = SP_WR_clones))
AIC(lmer(sizeNext ~ size+I(size^2) + (1|site_trans), data = SP_WR_clones))
summary(lmer(sizeNext ~ size+ precip + (1|site_trans), data = SP_WR_clones))
AIC(lmer(sizeNext ~ size + precip + (1|site_trans), data = SP_WR_clones))
summary(lmer(sizeNext ~ size + (1|site_trans), data = SP_WR_clones)) #Using this model based of AIC
AIC(lmer(sizeNext ~ size + (1|site_trans), data = SP_WR_clones))
summary(lmer(sizeNext ~ 1 + (1|site_trans), data = SP_WR_clones))
AIC(lmer(sizeNext ~ 1 + (1|site_trans), data = SP_WR_clones))

mod_clone_growth_SP_WR <- lmer(sizeNext ~ size + (1|site_trans), data = SP_WR_clones)
CloneSizeVariable_SP_WR <- "size"

plot_clone_growth_SP_WR <- plot_predictions_growth(model = mod_clone_growth_SP_WR, data = SP_WR_clones, minSize_SP, maxSize_SP)
plot_clone_growth_SP_WR

#Make clonal object
co_SP_WR <- makeClonalObj(SP_WR, fecConstants=data.frame(correctionForOrphans= 1/(1-SP_WR_clones$prop_orphan[1])),
                          offspringSizeExplanatoryVariables = CloneSizeVariable_SP_WR, Formula = c(CloneChosenModel_SP_WR, CloneNumberChosenModel_SP_WR),
                          Family = c("binomial","poisson"), Transform=c("none","none"),offspringSplitter=data.frame(seedbank=0,continuous=1))

co_SP_WR@fitFec[[1]]$coefficients <- as.numeric(fixef(mod_clo_SP_WR))
co_SP_WR@fitFec[[2]]$coefficients <- as.numeric(coef(mod_clo_no_SP_WR)) #not really needed since this is a linear model
co_SP_WR@offspringRel$coefficients <- as.numeric(fixef(mod_clone_growth_SP_WR))
co_SP_WR@sdOffspringSize <- sigma.hat(mod_clone_growth_SP_WR)$sigma$data



Cmatrix_SP_WR <- makeIPMCmatrix(clonalObj = co_SP_WR, minSize=minSize_SP, maxSize=maxSize_SP, nBigMatrix = 100, correction = "constant")

contourPlot2(t(Cmatrix_SP_WR), Cmatrix_SP_WR@meshpoints, maxSize_SP, 0.03, 0, title = "Cmatrix: clones") 


#### Build IPM ####
IPM_SP_WR_precip1 <- Pmatrix_SP_WR + Fmatrix_SP_WR_precip1 + Cmatrix_SP_WR
#contourPlot2(t( M = IPM_SP_CC_precip1, meshpts = Pmatrix_SP_CC_precip1@meshpoints, maxSize = maxSize, lower = 0.03, upper = 0))
IPM_plot(IPM_control = IPM_SP_WR_precip1, minSize = minSize_SP, maxSize = maxSize_SP, zrange = c(-0.03, 0.06)) + ggtitle("Sibbaldia procumbens WR precip 1.2 m/year")
#persp(IPM_SP_CC)
as.numeric(eigen(IPM_SP_WR_precip1)$value[1])

IPM_SP_WR_precip2 <- Pmatrix_SP_WR + Fmatrix_SP_WR_precip2 + Cmatrix_SP_WR
#contourPlot2(t( M = IPM_SP_CC_precip1, meshpts = Pmatrix_SP_CC_precip1@meshpoints, maxSize = maxSize, lower = 0.03, upper = 0))
IPM_plot(IPM_control = IPM_SP_WR_precip2, minSize = minSize_SP, maxSize = maxSize_SP, zrange = c(-0.03, 0.06)) + ggtitle("Sibbaldia procumbens WR precip 2.3 m/year")
#persp(IPM_SP_CC)
as.numeric(eigen(IPM_SP_WR_precip2)$value[1])

IPM_SP_WR_precip3 <- Pmatrix_SP_WR + Fmatrix_SP_WR_precip3 + Cmatrix_SP_WR
#contourPlot2(t( M = IPM_SP_CC_precip1, meshpts = Pmatrix_SP_CC_precip1@meshpoints, maxSize = maxSize, lower = 0.03, upper = 0))
IPM_plot(IPM_control = IPM_SP_WR_precip3, minSize = minSize_SP, maxSize = maxSize_SP, zrange = c(-0.03, 0.06)) + ggtitle("Sibbaldia procumbens WR precip 3.4 m/year")
#persp(IPM_SP_CC)
as.numeric(eigen(IPM_SP_WR_precip3)$value[1])


x11()
contourPlot2(t(IPM_SP_CC), Pmatrix_SP_CC@meshpoints, maxSize_SP, 0.06, 0, title = "Sibbaldia procumbense WR")

##### Warming extant #####

#### P matrix ####

# choosing the best survival model
summary(glmer(surv ~ size+I(size^2)+I(size^3) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WE))
AIC(glmer(surv ~ size+I(size^2)+I(size^3) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WE))
summary(glmer(surv ~ size+I(size^2)+I(size^3) + precip + (1|block_trans), family = 'binomial', data = SP_WE))
AIC(glmer(surv ~ size+I(size^2)+I(size^3) + precip + (1|block_trans), family = 'binomial', data = SP_WE))
summary(glmer(surv ~ size+I(size^2)+I(size^3) + (1|block_trans), family = 'binomial', data = SP_WE))
AIC(glmer(surv ~ size+I(size^2)+I(size^3) + (1|block_trans), family = 'binomial', data = SP_WE))
summary(glmer(surv ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WE))
AIC(glmer(surv ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WE))
summary(glmer(surv ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_WE))
AIC(glmer(surv ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_WE))
summary(glmer(surv ~ size + precip + (1|block_trans), family = 'binomial', data = SP_WE))
AIC(glmer(surv ~ size + precip + (1|block_trans), family = 'binomial', data = SP_WE))
summary(glmer(surv ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_WE))
AIC(glmer(surv ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_WE))
summary(glmer(surv ~ size + (1|block_trans), family = 'binomial', data = SP_WE)) #Chose this model based of AIC
AIC(glmer(surv ~ size + (1|block_trans), family = 'binomial', data = SP_WE))
summary(glmer(surv ~ 1 + (1|block_trans), family = 'binomial', data = SP_WE))
AIC(glmer(surv ~ 1 + (1|block_trans), family = 'binomial', data = SP_WE))

mod_surv_SP_WE <- glmer(surv ~ size + (1|block_trans), family = 'binomial', data = SP_WE)
plot_surv_SP_WE <- plot_predictions_surv(model = mod_surv_SP_WE, data = SP_WE, minSize_SP, maxSize_SP)

plot_surv_SP_WE

so_SP_WE <- makeSurvObj(SP_WE, "surv ~ size")
so_SP_WE <- coerceSurvObj(so_SP_WE, as.numeric(fixef(mod_surv_SP_WE))) #Adding coefficients from mixed effect model and not from the linear model as is default in makeSurvObj

# choosing the best growth model
summary(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = SP_WE))
AIC(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = SP_WE))
summary(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = SP_WE))
AIC(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = SP_WE))
summary(lmer(sizeNext ~ size + precip+I(precip^2) + (1|block_trans), data = SP_WE))
AIC(lmer(sizeNext ~ size + precip+I(precip^2) + (1|block_trans), data = SP_WE))
summary(lmer(sizeNext ~ size + precip + (1|block_trans), data = SP_WE))
AIC(lmer(sizeNext ~ size + precip + (1|block_trans), data = SP_WE))
summary(lmer(sizeNext ~ size+I(size^2) + (1|block_trans), data = SP_WE))
AIC(lmer(sizeNext ~ size+I(size^2) + (1|block_trans), data = SP_WE)) 
summary(lmer(sizeNext ~ size + (1|block_trans), data = SP_WE))#We chose this model based on AIC
AIC(lmer(sizeNext ~ size + (1|block_trans), data = SP_WE))
summary(lmer(sizeNext ~ 1 + (1|block_trans), data = SP_WE))
AIC(lmer(sizeNext ~ 1 + (1|block_trans), data = SP_WE))

mod_growth_SP_WE <- lmer(sizeNext ~ size + (1|block_trans), data = SP_WE)

plot_growth_SP_WE <- plot_predictions_growth(model = mod_growth_SP_WE, data = SP_WE, minSize_SP, maxSize_SP)

plot_growth_SP_WE


go_SP_WE <- makeGrowthObj(SP_WE, "sizeNext ~ size")
go_SP_WE <- coerceGrowthObj(go_SP_WE, fixef(mod_growth_SP_WE),
                            sigma.hat(mod_growth_SP_WE)$sigma$data)


# Make discrete transition object
dto_SP_WE <- makeDiscreteTrans(SP_WE, discreteTrans = matrix(
  c(SP_OTC_seed_bank$seeds_staySB,
    (1-SP_OTC_seed_bank$seeds_staySB)*seedling_est_SP_Veg,
    (1-SP_OTC_seed_bank$seeds_staySB)*(1-seedling_est_SP_Veg), 
    0,
    sum(SP_WE$number[SP_WE$stage=="continuous"&SP_WE$stageNext=="continuous"], na.rm=T),
    sum(SP_WE$number[SP_WE$stage=="continuous"&SP_WE$stageNext=="dead"], na.rm=T)),
  ncol = 2,
  nrow = 3, 
  dimnames = list(c("seedbank", "continuous", "dead"), c("seedbank", "continuous"))),
  meanToCont = matrix(Seedling_info_SP$mean_Veg, ncol = 1, nrow = 1, dimnames = list(c("mean"), c("seedbank"))),
  sdToCont = matrix(Seedling_info_SP$sd, ncol = 1, nrow = 1, dimnames = list(c(""),c("seedbank"))))


# With these survival and growth objects in hand, we build a survival/growth (P) matrix.
Pmatrix_SP_WE <- makeIPMPmatrix(survObj=so_SP_WE, growObj=go_SP_WE, minSize=minSize_SP, maxSize=maxSize_SP, correction = "constant", nBigMatrix = 100)

diagnosticsPmatrix(Pmatrix_SP_WE, survObj=so_SP_WE, growObj=go_SP_WE, dff = SP_WE)

Pmatrix_SP_WE <- makeIPMPmatrix(survObj=so_SP_WE, growObj=go_SP_WE, minSize=minSize_SP, maxSize=maxSize_SP, discreteTrans = dto_SP_WE, correction = "constant", nBigMatrix = 100)
x11()
contourPlot2(t(Pmatrix_SP_WE), Pmatrix_SP_WE@meshpoints, maxSize_SP, 0.03, 0, title = "Pmatrix: survival and growth") 


#### F matrix ####
# Choosing the best model for estimating if an individual flowers
summary(glmer(flo.if ~ size+I(size^2)+I(size^3) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WE))
AIC(glmer(flo.if ~ size+I(size^2)+I(size^3) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WE))
summary(glmer(flo.if ~ size+I(size^2)+I(size^3) + precip + (1|block_trans), family = 'binomial', data = SP_WE))
AIC(glmer(flo.if ~ size+I(size^2)+I(size^3) + precip + (1|block_trans), family = 'binomial', data = SP_WE))
summary(glmer(flo.if ~ size+I(size^2)+I(size^3) + (1|block_trans), family = 'binomial', data = SP_WE))
AIC(glmer(flo.if ~ size+I(size^2)+I(size^3) + (1|block_trans), family = 'binomial', data = SP_WE))

summary(glmer(flo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WE))
AIC(glmer(flo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WE))
summary(glmer(flo.if ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_WE)) 
AIC(glmer(flo.if ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_WE))
summary(glmer(flo.if ~ size + precip + (1|block_trans), family = 'binomial', data = SP_WE)) #Choosing this model based of AIC
AIC(glmer(flo.if ~ size + precip + (1|block_trans), family = 'binomial', data = SP_WE))
summary(glmer(flo.if ~ size + (1|block_trans), family = 'binomial', data = SP_WE))
AIC(glmer(flo.if ~ size + (1|block_trans), family = 'binomial', data = SP_WE)) 
summary(glmer(flo.if ~ 1 + (1|block_trans), family = 'binomial', data = SP_WE))
AIC(glmer(flo.if ~ 1 + (1|block_trans), family = 'binomial', data = SP_WE))

floweringChosenModel_SP_WE <- flo.if ~ size
mod_flo_if_SP_WE <- glmer(flo.if ~ size + precip + (1|block_trans), family = 'binomial', data = SP_WE) 
plot_SP_WE_floif <- plot_predictions_floif_precip(model = mod_flo_if_SP_WE, data = SP_WE, minSize_SP, maxSize_SP)
plot_SP_WE_floif 


# Choosing the best model for estimating the number of flowers, if an individual flowers
summary(glmer(flo.no ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'poisson', data = SP_WE))
AIC(glmer(flo.no ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'poisson', data = SP_WE))
summary(glmer(flo.no ~ size + precip+I(precip^2) + (1|block_trans), family = 'poisson', data = SP_WE))
AIC(glmer(flo.no ~ size + precip+I(precip^2) + (1|block_trans), family = 'poisson', data = SP_WE))
summary(glmer(flo.no ~ size + precip + (1|block_trans), family = 'poisson', data = SP_WE))
AIC(glmer(flo.no ~ size + precip + (1|block_trans), family = 'poisson', data = SP_WE))
summary(glmer(flo.no ~ size + (1|block_trans), family = 'poisson', data = SP_WE)) #Best AIC
AIC(glmer(flo.no ~ size + (1|block_trans), family = 'poisson', data = SP_WE))
summary(glmer(flo.no ~ 1 + (1|block_trans), family = 'poisson', data = SP_WE)) #Chosing this model because we want to have the same number of flowers for all treatments of Sibbaldia procumbens
AIC(glmer(flo.no ~ 1 + (1|block_trans), family = 'poisson', data = SP_WE))

flowerNumberChosenModel_SP_WE <- flo.no ~ 1

mod_flo_no_SP_WE <- glmer(flo.no ~ 1 + (1|block_trans), family = 'poisson', data = SP_WE)

plot_flo_no_SP_WE <-plot_predictions_flono(model = mod_flo_no_SP_WE, data = SP_WE, minSize_SP, maxSize_SP, ylim = 15) 

plot_flo_no_SP_WE

# Make fecundity object
fo_SP_WE <-makeFecObj(SP_WE, 
                      Formula= c(floweringChosenModel_SP_WE, flowerNumberChosenModel_SP_WE),
                      Family = c("binomial", "poisson"),
                      fecConstants = data.frame(seedsPerCap = Seeds_per_capsule_SP,
                                                seedlingEstablishmentRate = seedling_est_SP_Veg), 
                      meanOffspringSize = Seedling_info_SP$mean_Veg,
                      sdOffspringSize = Seedling_info_SP$sd,
                      offspringSplitter = data.frame(seedbank=SP_OTC_seed_bank$seeds_alive_total_prop* (1-seedling_est_SP_Veg), continuous=(1-(SP_OTC_seed_bank$seeds_alive_total_prop* (1-seedling_est_SP_Veg)))),
                      vitalRatesPerOffspringType = data.frame(seedbank=c(1,1,1,0), continuous=c(1,1,1,1),
                                                              row.names=c("flo.if","flo.no","seedsPerCap","seedlingEstablishmentRate")))

fo_SP_WE@fitFec[[2]]$coefficients <- as.numeric(fixef(mod_flo_no_SP_WE))
fo_SP_WE@fitFec[[1]]$coefficients <- c(as.numeric(fixef(mod_flo_if_SP_WE)[1]) + 1.2*as.numeric(fixef(mod_flo_if_SP_WE)[3]),
                                       as.numeric(fixef(mod_flo_if_SP_WE)[2]))

fo_SP_WE_precip1 <- fo_SP_WE

fo_SP_WE@fitFec[[1]]$coefficients <- c(as.numeric(fixef(mod_flo_if_SP_WE)[1]) + 2.3*as.numeric(fixef(mod_flo_if_SP_WE)[3]),
                                       as.numeric(fixef(mod_flo_if_SP_WE)[2]))

fo_SP_WE_precip2 <- fo_SP_WE

fo_SP_WE@fitFec[[1]]$coefficients <- c(as.numeric(fixef(mod_flo_if_SP_WE)[1]) + 3.4*as.numeric(fixef(mod_flo_if_SP_WE)[3]),
                                       as.numeric(fixef(mod_flo_if_SP_WE)[2]))

fo_SP_WE_precip3 <- fo_SP_WE

Fmatrix_SP_WE_precip1 <- makeIPMFmatrix(fecObj=fo_SP_WE_precip1, minSize=minSize_SP, maxSize=maxSize_SP, correction = "continuous", nBigMatrix = 100)
Fmatrix_SP_WE_precip2 <- makeIPMFmatrix(fecObj=fo_SP_WE_precip2, minSize=minSize_SP, maxSize=maxSize_SP, correction = "continuous", nBigMatrix = 100)
Fmatrix_SP_WE_precip3 <- makeIPMFmatrix(fecObj=fo_SP_WE_precip3, minSize=minSize_SP, maxSize=maxSize_SP, correction = "continuous", nBigMatrix = 100)

#Plotting the matrix
contourPlot2(t(Fmatrix_SP_WE_precip1), Fmatrix_SP_WE@meshpoints, maxSize, 0.003, 0, title = "Fmatrix: flower and seedlings")

# image.plot(Fmatrix_VA_WE@meshpoints,
#            Fmatrix_VA_WE@meshpoints,
#            t(Fmatrix_VA_WE),
#            main = "Fmatrix: flower and seedlings",
#            xlab = "Size at t",
#            ylab = "Size at t+1")
# 
# image(t(Fmatrix_VA_WE))

#### C matrix ####

SP_WE_clones <- SP_WE %>% 
  filter(offspringNext == "clonal") %>% 
  mutate(number_orphans = case_when(is.na(size) ~ 1,
                                    !is.na(size) ~0)) %>% 
  mutate(total_num_orphan = sum(number_orphans),
         total_num_clones = n()) %>% 
  fill(total_num_orphan, .direction = "downup") %>% 
  mutate(prop_orphan = total_num_orphan/total_num_clones)

#Is the production of clones size dependent
#Running linear models because all the random effects gave singularity warning. I tried block_trans, site_trans, transition + blockID, transition + siteID, transition, blockID and siteID.
summary(glm(clo.if ~ size+I(size^2)+I(size^3) + precip+I(precip^2), family = 'binomial', data = SP_WE))
AIC(glm(clo.if ~ size+I(size^2)+I(size^3) + precip+I(precip^2) , family = 'binomial', data = SP_WE))
summary(glm(clo.if ~ size+I(size^2)+I(size^3) + precip, family = 'binomial', data = SP_WE))
AIC(glm(clo.if ~ size+I(size^2)+I(size^3) + precip, family = 'binomial', data = SP_WE))
summary(glm(clo.if ~ size+I(size^2)+I(size^3), family = 'binomial', data = SP_WE))
AIC(glm(clo.if ~ size+I(size^2)+I(size^3), family = 'binomial', data = SP_WE))

summary(glm(clo.if ~ size+I(size^2) + precip+I(precip^2), family = 'binomial', data = SP_WE)) 
AIC(glm(clo.if ~ size+I(size^2) + precip+I(precip^2) , family = 'binomial', data = SP_WE))
summary(glm(clo.if ~ size+I(size^2) + precip , family = 'binomial', data = SP_WE))#Choosing this model based of AIC
AIC(glm(clo.if ~ size+I(size^2) + precip , family = 'binomial', data = SP_WE))
summary(glm(clo.if ~ size + precip , family = 'binomial', data = SP_WE))
AIC(glm(clo.if ~ size + precip , family = 'binomial', data = SP_WE))
summary(glm(clo.if ~ size, family = 'binomial', data = SP_WE))
AIC(glm(clo.if ~ size, family = 'binomial', data = SP_WE))
summary(glm(clo.if ~ 1, family = 'binomial', data = SP_WE))
AIC(glm(clo.if ~ 1 , family = 'binomial', data = SP_WE))

mod_clo_SP_WE <- glm(clo.if ~ size+I(size^2) + precip , family = 'binomial', data = SP_WE)
CloneChosenModel_SP_WE <- clo.if ~ size + size2 

plot_clo_if_SP_WE <- plot_predictions_cloif_precip(model = mod_clo_SP_WE, data = SP_WE, minSize_SP, maxSize_SP)
plot_clo_if_SP_WE

#If you produce clones, does how many clones you make change with size of the mother 
# Running linear models because all the random effects gave singularity warning. I tried block_trans, site_trans, transition + blockID, transition + siteID, transition, blockID and siteID.
summary(glm(clo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = SP_WE))
AIC(glm(clo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = SP_WE))
summary(glm(clo.no ~ size + precip+I(precip^2), family = 'poisson', data = SP_WE))
AIC(glm(clo.no ~ size + precip+I(precip^2), family = 'poisson', data = SP_WE))
summary(glm(clo.no ~ size + precip, family = 'poisson', data = SP_WE))
AIC(glm(clo.no ~ size + precip, family = 'poisson', data = SP_WE))
summary(glm(clo.no ~ size, family = 'poisson', data = SP_WE))
AIC(glm(clo.no ~ size, family = 'poisson', data = SP_WE))
summary(glm(clo.no ~ 1, family = 'poisson', data = SP_WE)) #Chosing this model based of AIC
AIC(glm(clo.no ~ 1, family = 'poisson', data = SP_WE))

mod_clo_no_SP_WE <- glm(clo.no ~ 1, family = 'poisson', data = SP_WE)
CloneNumberChosenModel_SP_WE <- clo.no ~ 1

plot_clo_no_SP_WE <- plot_predictions_clono(model = mod_clo_no_SP_WE, data = SP_WE, minSize_SP, maxSize_SP, ylim = 6)
plot_clo_no_SP_WE

# Clonal size depending on mother size
summary(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = SP_WE_clones))
AIC(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = SP_WE_clones))
summary(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = SP_WE_clones)) 
AIC(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = SP_WE_clones))
summary(lmer(sizeNext ~ size+I(size^2) + (1|block_trans), data = SP_WE_clones)) 
AIC(lmer(sizeNext ~ size+I(size^2) + (1|block_trans), data = SP_WE_clones))
summary(lmer(sizeNext ~ size + (1|block_trans), data = SP_WE_clones)) 
AIC(lmer(sizeNext ~ size + (1|block_trans), data = SP_WE_clones)) 
summary(lmer(sizeNext ~ 1 + (1|block_trans), data = SP_WE_clones)) #Using this model because the other models does not make biological sense
AIC(lmer(sizeNext ~ 1 + (1|block_trans), data = SP_WE_clones))


mod_clone_growth_SP_WE <- lmer(sizeNext ~ 1 + (1|block_trans), data = SP_WE_clones)
CloneSizeVariable_SP_WE <- "1"

plot_clone_growth_SP_WE <- plot_predictions_growth(model = mod_clone_growth_SP_WE, data = SP_WE_clones, minSize_SP, maxSize_SP)
plot_clone_growth_SP_WE

#Make clonal object
co_SP_WE <- makeClonalObj(SP_WE, fecConstants=data.frame(correctionForOrphans= 1/(1-SP_WE_clones$prop_orphan[1])),
                          offspringSizeExplanatoryVariables = CloneSizeVariable_SP_WE, Formula = c(CloneChosenModel_SP_WE, CloneNumberChosenModel_SP_WE),
                          Family = c("binomial","poisson"), Transform=c("none","none"),offspringSplitter=data.frame(seedbank=0,continuous=1))

co_SP_WE@fitFec[[2]]$coefficients <- as.numeric(coef(mod_clo_no_SP_WE)) #not really needed since this is a linear model
co_SP_WE@offspringRel$coefficients <- as.numeric(fixef(mod_clone_growth_SP_WE))
co_SP_WE@sdOffspringSize <- sigma.hat(mod_clone_growth_SP_WE)$sigma$data
co_SP_WE@fitFec[[1]]$coefficients <- c(as.numeric(coef(mod_clo_SP_WE)[1]) + 1.2*as.numeric(coef(mod_clo_SP_WE)[4]),
                                       as.numeric(coef(mod_clo_SP_WE)[2]),
                                       as.numeric(coef(mod_clo_SP_WE)[3]))
co_SP_WE_precip1 <- co_SP_WE

co_SP_WE@fitFec[[1]]$coefficients <- c(as.numeric(coef(mod_clo_SP_WE)[1]) + 2.3*as.numeric(coef(mod_clo_SP_WE)[4]),
                                       as.numeric(coef(mod_clo_SP_WE)[2]),
                                       as.numeric(coef(mod_clo_SP_WE)[3]))
co_SP_WE_precip2 <- co_SP_WE

co_SP_WE@fitFec[[1]]$coefficients <- c(as.numeric(coef(mod_clo_SP_WE)[1]) + 3.4*as.numeric(coef(mod_clo_SP_WE)[4]),
                                       as.numeric(coef(mod_clo_SP_WE)[2]),
                                       as.numeric(coef(mod_clo_SP_WE)[3]))
co_SP_WE_precip3 <- co_SP_WE




Cmatrix_SP_WE_precip1 <- makeIPMCmatrix(clonalObj = co_SP_WE_precip1, minSize=minSize_SP, maxSize=maxSize_SP, nBigMatrix = 100, correction = "constant")
Cmatrix_SP_WE_precip2 <- makeIPMCmatrix(clonalObj = co_SP_WE_precip2, minSize=minSize_SP, maxSize=maxSize_SP, nBigMatrix = 100, correction = "constant")
Cmatrix_SP_WE_precip3 <- makeIPMCmatrix(clonalObj = co_SP_WE_precip3, minSize=minSize_SP, maxSize=maxSize_SP, nBigMatrix = 100, correction = "constant")

contourPlot2(t(Cmatrix_SP_WE_precip1), Cmatrix_SP_WE_precip1@meshpoints, maxSize_SP, 0.03, 0, title = "Cmatrix: clones") 


#### Build IPM ####
IPM_SP_WE_precip1 <- Pmatrix_SP_WE + Fmatrix_SP_WE_precip1 + Cmatrix_SP_WE_precip1
#contourPlot2(t( M = IPM_SP_CC_precip1, meshpts = Pmatrix_SP_CC_precip1@meshpoints, maxSize = maxSize, lower = 0.03, upper = 0))
IPM_plot(IPM_control = IPM_SP_WE_precip1, minSize = minSize_SP, maxSize = maxSize_SP, zrange = c(-0.03, 0.06)) + ggtitle("Sibbaldia procumbens WE precip 1.2 m/year")
#persp(IPM_SP_CC)
as.numeric(eigen(IPM_SP_WE_precip1)$value[1])

IPM_SP_WE_precip2 <- Pmatrix_SP_WE + Fmatrix_SP_WE_precip2 + Cmatrix_SP_WE_precip2
#contourPlot2(t( M = IPM_SP_CC_precip1, meshpts = Pmatrix_SP_CC_precip1@meshpoints, maxSize = maxSize, lower = 0.03, upper = 0))
IPM_plot(IPM_control = IPM_SP_WE_precip2, minSize = minSize_SP, maxSize = maxSize_SP, zrange = c(-0.03, 0.06)) + ggtitle("Sibbaldia procumbens WE precip 2.3 m/year")
#persp(IPM_SP_CC)
as.numeric(eigen(IPM_SP_WE_precip2)$value[1])

IPM_SP_WE_precip3 <- Pmatrix_SP_WE + Fmatrix_SP_WE_precip3 + Cmatrix_SP_WE_precip3
#contourPlot2(t( M = IPM_SP_CC_precip1, meshpts = Pmatrix_SP_CC_precip1@meshpoints, maxSize = maxSize, lower = 0.03, upper = 0))
IPM_plot(IPM_control = IPM_SP_WE_precip3, minSize = minSize_SP, maxSize = maxSize_SP, zrange = c(-0.03, 0.06)) + ggtitle("Sibbaldia procumbens WE precip 3.4 m/year")
#persp(IPM_SP_CC)
as.numeric(eigen(IPM_SP_WE_precip3)$value[1])


x11()
contourPlot2(t(IPM_SP_CC), Pmatrix_SP_CC@meshpoints, maxSize_SP, 0.06, 0, title = "Sibbaldia procumbensCC ")

##### Warming novel #####

#Filter out the outliers with large size
SP_WN <- SP_WN %>% 
  filter(!unique_IDS %in% c("Lav_6_3_21", "Lav_6_3_20"))

#### P matrix ####

# choosing the best survival model
summary(glmer(surv ~ size+I(size^2)+I(size^3)  + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WN))
AIC(glmer(surv ~ size+I(size^2)+I(size^3)  + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WN))
summary(glmer(surv ~ size+I(size^2)+I(size^3)  + precip + (1|block_trans), family = 'binomial', data = SP_WN))
AIC(glmer(surv ~ size+I(size^2)+I(size^3)  + precip + (1|block_trans), family = 'binomial', data = SP_WN))
summary(glmer(surv ~ size+I(size^2)+I(size^3) + (1|block_trans), family = 'binomial', data = SP_WN))
AIC(glmer(surv ~ size+I(size^2)+I(size^3) + (1|block_trans), family = 'binomial', data = SP_WN))

summary(glmer(surv ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WN))
AIC(glmer(surv ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WN))
summary(glmer(surv ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_WN))
AIC(glmer(surv ~ size+I(size^2) + precip + (1|block_trans), family = 'binomial', data = SP_WN))
summary(glmer(surv ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_WN))#Chose this model based of AIC
AIC(glmer(surv ~ size+I(size^2) + (1|block_trans), family = 'binomial', data = SP_WN))
summary(glmer(surv ~ size + (1|block_trans), family = 'binomial', data = SP_WN)) 
AIC(glmer(surv ~ size + (1|block_trans), family = 'binomial', data = SP_WN))
summary(glmer(surv ~ 1 + (1|block_trans), family = 'binomial', data = SP_WN))
AIC(glmer(surv ~ 1 + (1|block_trans), family = 'binomial', data = SP_WN))

mod_surv_SP_WN <- glmer(surv ~ size +I(size^2) + (1|block_trans), family = 'binomial', data = SP_WN)
plot_surv_SP_WN <- plot_predictions_surv(model = mod_surv_SP_WN, data = SP_WN, minSize_SP, maxSize_SP)

plot_surv_SP_WN

so_SP_WN <- makeSurvObj(SP_WN, "surv ~ size + size2")
so_SP_WN <- coerceSurvObj(so_SP_WN, as.numeric(fixef(mod_surv_SP_WN))) #Adding coefficients from mixed effect model and not from the linear model as is default in makeSurvObj

# choosing the best growth model
summary(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = SP_WN))
AIC(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = SP_WN))
summary(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = SP_WN))
AIC(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = SP_WN))
summary(lmer(sizeNext ~ size+I(size^2) + (1|block_trans), data = SP_WN))
AIC(lmer(sizeNext ~ size+I(size^2) + (1|block_trans), data = SP_WN)) 
summary(lmer(sizeNext ~ size + (1|block_trans), data = SP_WN)) #we chose this model based on AIC
AIC(lmer(sizeNext ~ size + (1|block_trans), data = SP_WN))
summary(lmer(sizeNext ~ 1 + (1|block_trans), data = SP_WN))
AIC(lmer(sizeNext ~ 1 + (1|block_trans), data = SP_WN))

mod_growth_SP_WN <- lmer(sizeNext ~ size + (1|block_trans), data = SP_WN)

plot_growth_SP_WN <- plot_predictions_growth(model = mod_growth_SP_WN, data = SP_WN, minSize_SP, maxSize_SP)

plot_growth_SP_WN


go_SP_WN <- makeGrowthObj(SP_WN, "sizeNext ~ size")
go_SP_WN <- coerceGrowthObj(go_SP_WN, fixef(mod_growth_SP_WN),
                            sigma.hat(mod_growth_SP_WN)$sigma$data)


# Make discrete transition object
dto_SP_WN <- makeDiscreteTrans(SP_WN, discreteTrans = matrix(
  c(SP_OTC_seed_bank$seeds_staySB,
    (1-SP_OTC_seed_bank$seeds_staySB)*seedling_est_SP_Veg,
    (1-SP_OTC_seed_bank$seeds_staySB)*(1-seedling_est_SP_Veg), 
    0,
    sum(SP_WN$number[SP_WN$stage=="continuous"&SP_WN$stageNext=="continuous"], na.rm=T),
    sum(SP_WN$number[SP_WN$stage=="continuous"&SP_WN$stageNext=="dead"], na.rm=T)),
  ncol = 2,
  nrow = 3, 
  dimnames = list(c("seedbank", "continuous", "dead"), c("seedbank", "continuous"))),
  meanToCont = matrix(Seedling_info_SP$mean_Veg, ncol = 1, nrow = 1, dimnames = list(c("mean"), c("seedbank"))),
  sdToCont = matrix(Seedling_info_SP$sd, ncol = 1, nrow = 1, dimnames = list(c(""),c("seedbank"))))


# With these survival and growth objects in hand, we build a survival/growth (P) matrix.
Pmatrix_SP_WN <- makeIPMPmatrix(survObj=so_SP_WN, growObj=go_SP_WN, minSize=minSize_SP, maxSize=maxSize_SP, correction = "constant", nBigMatrix = 100)

diagnosticsPmatrix(Pmatrix_SP_WN, survObj=so_SP_WN, growObj=go_SP_WN, dff = SP_WN)

Pmatrix_SP_WN <- makeIPMPmatrix(survObj=so_SP_WN, growObj=go_SP_WN, minSize=minSize_SP, maxSize=maxSize_SP, discreteTrans = dto_SP_WN, correction = "constant", nBigMatrix = 100)
x11()
contourPlot2(t(Pmatrix_SP_WN), Pmatrix_SP_WN@meshpoints, maxSize_SP, 0.03, 0, title = "Pmatrix: survival and growth") 


#### F matrix ####
# Choosing the best model for estimating if an individual flowers
summary(glmer(flo.if ~ size+I(size^2)+I(size^3) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WN))
AIC(glmer(flo.if ~ size+I(size^2)+I(size^3) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WN))
summary(glmer(flo.if ~ size+I(size^2)+I(size^3) + precip + (1|block_trans), family = 'binomial', data = SP_WN))
AIC(glmer(flo.if ~ size+I(size^2)+I(size^3) + precip+ (1|block_trans), family = 'binomial', data = SP_WN))
summary(glmer(flo.if ~ size+I(size^2)+I(size^3) + (1|block_trans), family = 'binomial', data = SP_WN))
AIC(glmer(flo.if ~ size+I(size^2)+I(size^3) + (1|block_trans), family = 'binomial', data = SP_WN))


summary(glmer(flo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WN))
AIC(glmer(flo.if ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WN))
summary(glmer(flo.if ~ size + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WN))
AIC(glmer(flo.if ~ size + precip+I(precip^2) + (1|block_trans), family = 'binomial', data = SP_WN))
summary(glmer(flo.if ~ size + precip + (1|block_trans), family = 'binomial', data = SP_WN))
AIC(glmer(flo.if ~ size + precip + (1|block_trans), family = 'binomial', data = SP_WN))
summary(glmer(flo.if ~ size + (1|block_trans), family = 'binomial', data = SP_WN)) #we chose this model based on AIC
AIC(glmer(flo.if ~ size + (1|block_trans), family = 'binomial', data = SP_WN)) 
summary(glmer(flo.if ~ 1 + (1|block_trans), family = 'binomial', data = SP_WN))
AIC(glmer(flo.if ~ 1 + (1|block_trans), family = 'binomial', data = SP_WN))

floweringChosenModel_SP_WN <- flo.if ~ size
mod_flo_if_SP_WN <- glmer(flo.if ~ size + (1|block_trans), family = 'binomial', data = SP_WN) 
plot_SP_WN_floif <- plot_predictions_floif_precip(model = mod_flo_if_SP_WN, data = SP_WN, minSize_SP, maxSize_SP)
plot_SP_WN_floif 


# Choosing the best model for estimating the number of flowers, if an individual flowers
summary(glmer(flo.no ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'poisson', data = SP_WN))
AIC(glmer(flo.no ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), family = 'poisson', data = SP_WN))
summary(glmer(flo.no ~ size + precip+I(precip^2) + (1|block_trans), family = 'poisson', data = SP_WN))
AIC(glmer(flo.no ~ size + precip+I(precip^2) + (1|block_trans), family = 'poisson', data = SP_WN))
summary(glmer(flo.no ~ size + precip + (1|block_trans), family = 'poisson', data = SP_WN))
AIC(glmer(flo.no ~ size + precip + (1|block_trans), family = 'poisson', data = SP_WN))
summary(glmer(flo.no ~ size + (1|block_trans), family = 'poisson', data = SP_WN)) 
AIC(glmer(flo.no ~ size + (1|block_trans), family = 'poisson', data = SP_WN))
summary(glmer(flo.no ~ 1 + (1|block_trans), family = 'poisson', data = SP_WN)) #Chosing this model based on AIC
AIC(glmer(flo.no ~ 1 + (1|block_trans), family = 'poisson', data = SP_WN))

flowerNumberChosenModel_SP_WN <- flo.no ~ 1 

mod_flo_no_SP_WN <- glmer(flo.no ~ 1 + (1|block_trans), family = 'poisson', data = SP_WN)

plot_flo_no_SP_WN <-plot_predictions_flono(model = mod_flo_no_SP_WN, data = SP_WN, minSize_SP, maxSize_SP, ylim = 15) 

plot_flo_no_SP_WN

# Make fecundity object
fo_SP_WN <-makeFecObj(SP_WN, 
                      Formula= c(floweringChosenModel_SP_WN, flowerNumberChosenModel_SP_WN),
                      Family = c("binomial", "poisson"),
                      fecConstants = data.frame(seedsPerCap = Seeds_per_capsule_SP,
                                                seedlingEstablishmentRate = seedling_est_SP_Veg), 
                      meanOffspringSize = Seedling_info_SP$mean_Veg,
                      sdOffspringSize = Seedling_info_SP$sd,
                      offspringSplitter = data.frame(seedbank=SP_OTC_seed_bank$seeds_alive_total_prop* (1-seedling_est_SP_Veg), continuous=(1-(SP_OTC_seed_bank$seeds_alive_total_prop* (1-seedling_est_SP_Veg)))),
                      vitalRatesPerOffspringType = data.frame(seedbank=c(1,1,1,0), continuous=c(1,1,1,1),
                                                              row.names=c("flo.if","flo.no","seedsPerCap","seedlingEstablishmentRate")))


fo_SP_WN@fitFec[[1]]$coefficients <- as.numeric(fixef(mod_flo_if_SP_WN))
fo_SP_WN@fitFec[[2]]$coefficients <- as.numeric(fixef(mod_flo_no_SP_WN))
fo_SP_WN <- fo_SP_WN

#Making the F matrix
Fmatrix_SP_WN <- makeIPMFmatrix(fecObj=fo_SP_WN, minSize=minSize_SP, maxSize=maxSize_SP, correction = "continuous", nBigMatrix = 100)

#Plotting the matrix
contourPlot2(t(Fmatrix_SP_WN_precip1), Fmatrix_SP_WN@meshpoints, maxSize, 0.003, 0, title = "Fmatrix: floWNr and seedlings")

# image.plot(Fmatrix_VA_WN@meshpoints,
#            Fmatrix_VA_WN@meshpoints,
#            t(Fmatrix_VA_WN),
#            main = "Fmatrix: floWNr and seedlings",
#            xlab = "Size at t",
#            ylab = "Size at t+1")
# 
# image(t(Fmatrix_VA_WN))

#### C matrix ####

SP_WN_clones <- SP_WN %>% 
  filter(offspringNext == "clonal") %>% 
  mutate(number_orphans = case_when(is.na(size) ~ 1,
                                    !is.na(size) ~0)) %>% 
  mutate(total_num_orphan = sum(number_orphans),
         total_num_clones = n()) %>% 
  fill(total_num_orphan, .direction = "downup") %>% 
  mutate(prop_orphan = total_num_orphan/total_num_clones)

#Is the production of clones size dependent
#Using site_trans as the random effect because block_trans  gave singularity warning. 
summary(glmer(clo.if ~ size+I(size^2)+I(size^3) + precip+I(precip^2) + (1|site_trans), family = 'binomial', data = SP_WN)) 
AIC(glmer(clo.if ~ size+I(size^2)+I(size^3) + precip+I(precip^2) + (1|site_trans), family = 'binomial', data = SP_WN))
summary(glmer(clo.if ~ size+I(size^2)+I(size^3) + precip + (1|site_trans), family = 'binomial', data = SP_WN)) 
AIC(glmer(clo.if ~ size+I(size^2)+I(size^3) + precip + (1|site_trans), family = 'binomial', data = SP_WN))
summary(glmer(clo.if ~ size+I(size^2)+I(size^3) + (1|site_trans), family = 'binomial', data = SP_WN)) 
AIC(glmer(clo.if ~ size+I(size^2)+I(size^3) + (1|site_trans), family = 'binomial', data = SP_WN))


summary(glmer(clo.if ~ size+I(size^2) + precip+I(precip^2) + (1|site_trans), family = 'binomial', data = SP_WN)) 
AIC(glmer(clo.if ~ size+I(size^2) + precip+I(precip^2) + (1|site_trans), family = 'binomial', data = SP_WN)) 
summary(glmer(clo.if ~ size+I(size^2) + precip + (1|site_trans), family = 'binomial', data = SP_WN)) 
AIC(glmer(clo.if ~ size+I(size^2) + precip + (1|site_trans), family = 'binomial', data = SP_WN)) 
summary(glmer(clo.if ~ size+I(size^2) + (1|site_trans), family = 'binomial', data = SP_WN)) #Choosing this model because the increase in propability of making clones at large individuals does not make biological sense
AIC(glmer(clo.if ~ size+I(size^2) + (1|site_trans), family = 'binomial', data = SP_WN)) 
summary(glmer(clo.if ~ size + (1|site_trans), family = 'binomial', data = SP_WN)) 
AIC(glmer(clo.if ~ size + (1|site_trans), family = 'binomial', data = SP_WN)) 
summary(glmer(clo.if ~ 1 + (1|site_trans), family = 'binomial', data = SP_WN)) 
AIC(glmer(clo.if ~ 1 + (1|site_trans), family = 'binomial', data = SP_WN)) 

mod_clo_SP_WN <- glmer(clo.if ~ size+I(size^2) + (1|site_trans), family = 'binomial', data = SP_WN)
CloneChosenModel_SP_WN <- clo.if ~ size + size2 

plot_clo_if_SP_WN <- plot_predictions_cloif(model = mod_clo_SP_WN, data = SP_WN, minSize_SP, maxSize_SP)
plot_clo_if_SP_WN

#If you produce clones, does how many clones you make change with size of the mother 
# Running linear models because all the random effects gave singularity warning. I tried block_trans, site_trans, transition + blockID, transition + siteID, transition, blockID and siteID.
summary(glm(clo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = SP_WN))
AIC(glm(clo.no ~ size+I(size^2) + precip+I(precip^2), family = 'poisson', data = SP_WN))
summary(glm(clo.no ~ size+I(size^2) + precip, family = 'poisson', data = SP_WN))
AIC(glm(clo.no ~ size+I(size^2) + precip, family = 'poisson', data = SP_WN))
summary(glm(clo.no ~ size+I(size^2), family = 'poisson', data = SP_WN))
AIC(glm(clo.no ~ size+I(size^2), family = 'poisson', data = SP_WN))
summary(glm(clo.no ~ size, family = 'poisson', data = SP_WN))
AIC(glm(clo.no ~ size, family = 'poisson', data = SP_WN))
summary(glm(clo.no ~ 1, family = 'poisson', data = SP_WN)) #Choosing this model based of AIC
AIC(glm(clo.no ~ 1, family = 'poisson', data = SP_WN))

mod_clo_no_SP_WN <- glm(clo.no ~ 1, family = 'poisson', data = SP_WN)
CloneNumberChosenModel_SP_WN <- clo.no ~ 1

plot_clo_no_SP_WN <- plot_predictions_clono(model = mod_clo_no_SP_WN, data = SP_WN, minSize_SP, maxSize_SP, ylim = 6)
plot_clo_no_SP_WN

# Clonal size depending on mother size
summary(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = SP_WN_clones))
AIC(lmer(sizeNext ~ size+I(size^2) + precip+I(precip^2) + (1|block_trans), data = SP_WN_clones))
summary(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = SP_WN_clones)) 
AIC(lmer(sizeNext ~ size+I(size^2) + precip + (1|block_trans), data = SP_WN_clones))
summary(lmer(sizeNext ~ size+ precip + (1|block_trans), data = SP_WN_clones)) 
AIC(lmer(sizeNext ~ size + precip + (1|block_trans), data = SP_WN_clones))
summary(lmer(sizeNext ~ size+I(size^2) + (1|block_trans), data = SP_WN_clones)) 
AIC(lmer(sizeNext ~ size+I(size^2) + (1|block_trans), data = SP_WN_clones))
summary(lmer(sizeNext ~ size + (1|block_trans), data = SP_WN_clones)) #Using this model based on AIC
AIC(lmer(sizeNext ~ size + (1|block_trans), data = SP_WN_clones)) 
summary(lmer(sizeNext ~ 1 + (1|block_trans), data = SP_WN_clones)) 
AIC(lmer(sizeNext ~ 1 + (1|block_trans), data = SP_WN_clones))


mod_clone_growth_SP_WN <- lmer(sizeNext ~ size + (1|block_trans), data = SP_WN_clones)
CloneSizeVariable_SP_WN <- "size"

plot_clone_growth_SP_WN <- plot_predictions_growth(model = mod_clone_growth_SP_WN, data = SP_WN_clones, minSize_SP, maxSize_SP)
plot_clone_growth_SP_WN

#Make clonal object
co_SP_WN <- makeClonalObj(SP_WN, fecConstants=data.frame(correctionForOrphans= 1/(1-SP_WN_clones$prop_orphan[1])),
                          offspringSizeExplanatoryVariables = CloneSizeVariable_SP_WN, Formula = c(CloneChosenModel_SP_WN, CloneNumberChosenModel_SP_WN),
                          Family = c("binomial","poisson"), Transform=c("none","none"),offspringSplitter=data.frame(seedbank=0,continuous=1))

co_SP_WN@fitFec[[1]]$coefficients <- as.numeric(fixef(mod_clo_SP_WN))
co_SP_WN@fitFec[[2]]$coefficients <- as.numeric(coef(mod_clo_no_SP_WN)) #not really needed since this is a linear model
co_SP_WN@offspringRel$coefficients <- as.numeric(fixef(mod_clone_growth_SP_WN))
co_SP_WN@sdOffspringSize <- sigma.hat(mod_clone_growth_SP_WN)$sigma$data
co_SP_WN <- co_SP_WN

#Build C matrix
Cmatrix_SP_WN <- makeIPMCmatrix(clonalObj = co_SP_WN, minSize=minSize_SP, maxSize=maxSize_SP, nBigMatrix = 100, correction = "constant")

contourPlot2(t(Cmatrix_SP_WN), Cmatrix_SP_WN_precip1@meshpoints, maxSize_SP, 0.03, 0, title = "Cmatrix: clones") 


#### Build IPM ####
IPM_SP_WN<- Pmatrix_SP_WN + Fmatrix_SP_WN + Cmatrix_SP_WN
#contourPlot2(t( M = IPM_SP_CC_precip1, meshpts = Pmatrix_SP_CC_precip1@meshpoints, maxSize = maxSize, loWNr = 0.03, upper = 0))
IPM_plot(IPM_control = IPM_SP_WN, minSize = minSize_SP, maxSize = maxSize_SP, zrange = c(-0.03, 0.06)) + ggtitle("Sibbaldia procumbens WN precip 1.2 m/year")
#persp(IPM_SP_CC)
as.numeric(eigen(IPM_SP_WN)$value[1])

