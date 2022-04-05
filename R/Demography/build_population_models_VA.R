########################################################################
### Script for building population models for the INCLINE experiment ###
########################################################################

#### Source files ####
source("R/Demography/cleaning_demogprahy.R")
source("R/Demography/ready_demograhy_for_IPM.R")

#### Libraries ####
library(tidyverse)
library(lme4)
library(lmerTest)
library(IPMpack)
library(fields)
library(conflicted)

#### Select preferences for conflicts ####

conflict_prefer("select", "dplyr")
conflict_prefer("lmer", "lmerTest")

#### Downloading data from OSF ####

#Make script for reading in ready made data from OSF

#### Load data ####

#Load data 

# We define a new size axis for the midpoint evaluation of the IPMs:
minSize<-min(Ver_alp_2018_2021$size, na.rm=T)-1
maxSize<-max(Ver_alp_2018_2021$sizeNext, na.rm=T)+2
x<-seq(from=round(minSize),to=round(maxSize),length=100)
x0<-data.frame(size=x,size2=x*x)


# # To understand the data, we plot survival, growth/shrinkage/stasis, number of seeds, and size of recruits:
# par(mfrow=c(2,2),mar=c(4,4,2,1))
# plot(Ver_alp_2018_2021$size,jitter(Ver_alp_2018_2021$surv),xlab="Size (t)", ylab="Survival to t+1")
# plot(Ver_alp_2018_2021$size, Ver_alp_2018_2021$sizeNext,xlab="Size (t)",ylab="Size (t+1)") 
# plot(Ver_alp_2018_2021$size,jitter(Ver_alp_2018_2021$flo.if),xlab="Size (t)", ylab="Flowering probability") 
# plot(Ver_alp_2018_2021$size, Ver_alp_2018_2021$fec,xlab="Size (t)",ylab="Number of seeds") 
# 
# Ver_alp_2018_2021 %>% filter(offspringNext == "sexual") %>% ggplot(aes( x = sizeNext)) + geom_histogram() + ylab("Seedling size")
# Ver_alp_2018_2021 %>% filter(offspringNext == "clone") %>% ggplot(aes( x = sizeNext)) + geom_histogram() + ylab("Clone size")

Ver_alp_2018_2021 <- Ver_alp_2018_2021 %>% 
   ungroup() %>%
   as.data.frame() %>%
   mutate(stage = case_when(!is.na(size) ~ "continuous",
                            is.na(size) ~ NA_character_),
          stageNext = case_when(!is.na(size) & !is.na(sizeNext) ~ "continuous",
                                is.na(size) & !is.na(sizeNext) ~ "continuous",
                                !is.na(size) & is.na(sizeNext) ~ "dead",
                                TRUE ~ NA_character_))

VA_CC <- Ver_alp_2018_2021 %>% filter(OTC == "C" & treatment == "C")
VA_CR <- Ver_alp_2018_2021 %>% filter(OTC == "C" & treatment == "R")
VA_CE <- Ver_alp_2018_2021 %>% filter(OTC == "C" & treatment == "E")
VA_CN <- Ver_alp_2018_2021 %>% filter(OTC == "C" & treatment == "N")
VA_WC <- Ver_alp_2018_2021 %>% filter(OTC == "W" & treatment == "C")
VA_WR <- Ver_alp_2018_2021 %>% filter(OTC == "W" & treatment == "R")
VA_WE <- Ver_alp_2018_2021 %>% filter(OTC == "W" & treatment == "E")
VA_WN <- Ver_alp_2018_2021 %>% filter(OTC == "W" & treatment == "N")

VA_C_seed_bank <- seed_bank1 %>% 
   filter(species == "Ver_alp",
          warming == "C") %>% 
   ungroup() %>% 
   mutate(seeds_alive_total_prop = mean(seeds_alive_total_prop),
          seeds_dead_total_prop = mean(seeds_dead_total_prop)) %>% 
   select(seeds_alive_total_prop) %>% 
   unique()

VA_OTC_seed_bank <- seed_bank1 %>% 
   filter(species == "Ver_alp",
          warming == "OTC") %>% 
   ungroup() %>% 
   mutate(seeds_alive_total_prop = mean(seeds_alive_total_prop),
          seeds_dead_total_prop = mean(seeds_dead_total_prop)) %>% 
   select(seeds_alive_total_prop) %>% 
   unique()
   
   

##### P matrix #####

#### Ambient temperature control ####

# The first step in constructing an IPM with IPMpack is a survival analysis. We use the function ‘survModelComp’ to explore whether survival is related to size, as illustrated in this figure:
x11()
par(mfrow=c(1,1))
survModelComp(dataf= VA_CC, makePlot=TRUE, legendPos="topleft", mainTitle="Survival", ncuts = 30)

# Based on this simple analysis we select the following survival model since it has the lowest AIC value:
so_CC <- makeSurvObj(VA_CC, surv ~ size)

# We next model growth, conditional on survival. Here, ’growth’ is the process relating size in year t+1 to size in year t. We use the following code to illustrate it in a figure:
growthModelComp(dataf=VA_CC, makePlot=TRUE, legendPos="bottomright", mainTitle="Growth")

# Based on this simple model comparison, we select the following growth model:
go_CC <- makeGrowthObj(VA_CC, sizeNext ~ size)

# Make discrete transition object
dto_VA_CC <- makeDiscreteTrans(VA_CC, discreteTrans = matrix(
                                   c(VA_C_seed_bank$seeds_alive_total_prop,
                                     (1-seedling_est_VA_C_Veg)*seedling_est_VA_C_Veg,
                                     (1-seedling_est_VA_C_Veg)*(1-seedling_est_VA_C_Veg), 
                                     0,
                                     0.5, #Placeholder number until I know what to put in here, see comment below for what Joachim put in there.
                                     0.5), #same
                                     #sum(VA_CC$number[VA_CC$stage=="continuous"&VA_CC$stageNext=="continuous"], na.rm=T),
                                     #sum(VA.all.TT2$number[VA.all.TT2$stage=="continuous"&VA.all.TT2$stageNext=="dead"], na.rm=T)),
                                     ncol = 2,
                                   nrow = 3, 
                                   dimnames = list(
                                      c("seedbank", "continuous", "dead"),
                                      c("seedbank", "continuous"))),
                                meanToCont = matrix(mean_NoVeg_VA, ncol = 1, nrow = 1, dimnames = list(c("mean"), c("seedbank"))),
                                sdToCont = matrix(Seedling_info_VA$sd, ncol = 1, nrow = 1,dimnames = list(c(""),c("seedbank"))))


# With these survival and growth objects in hand, we build a survival/growth (P) matrix.
Pmatrix_CC <- makeIPMPmatrix(survObj=so_CC, growObj=go_CC, minSize=minSize, maxSize=maxSize, correction = "constant")

# We plot this P-matrix using the ’image.plot’ function of the fields package:

image.plot(Pmatrix_CC@meshpoints,
           Pmatrix_CC@meshpoints,
           t(Pmatrix_CC),
           main = "Pmatrix: survival and growth",
           xlab = "Size at t",
           ylab = "Size at t+1")
abline(0,1,lty=2,lwd=3)

diagnosticsPmatrix(Pmatrix_CC, growObj=go_CC, survObj=so_CC, correction="constant", dff = VA_CC) 

#Bindwidth and range size looks ok

#### Ambient temperature removal ####

# The first step in constructing an IPM with IPMpack is a survival analysis. We use the function ‘survModelComp’ to explore whether survival is related to size, as illustrated in this figure:
x11()
par(mfrow=c(1,1))
survModelComp(dataf= VA_CR, makePlot=TRUE, legendPos="topleft", mainTitle="Survival", ncuts = 30)

# Based on this simple analysis we select the following survival model since it has the lowest AIC value:
so_CR <- makeSurvObj(VA_CR, surv ~ size)

# We next model growth, conditional on survival. Here, ’growth’ is the process relating size in year t+1 to size in year t. We use the following code to illustrate it in a figure:
growthModelComp(dataf=VA_CR, makePlot=TRUE, legendPos="bottomright", mainTitle="Growth")

# Based on this simple model comparison, we select the following growth model:
go_CR <- makeGrowthObj(VA_CR, sizeNext ~ size + size2)

# With these survival and growth objects in hand, we build a survival/growth (P) matrix.
Pmatrix_CR <- makeIPMPmatrix(survObj=so_CR, growObj=go_CR, minSize=minSize, maxSize=maxSize, correction = "constant")

# We plot this P-matrix using the ’image.plot’ function of the fields package:

image.plot(Pmatrix_CR@meshpoints,
           Pmatrix_CR@meshpoints,
           t(Pmatrix_CR),
           main = "Pmatrix: survival and growth",
           xlab = "Size at t",
           ylab = "Size at t+1")
abline(0,1,lty=2,lwd=3)

diagnosticsPmatrix(Pmatrix_CR, growObj=go_CR, survObj=so_CR, correction="constant", dff = VA_CR) 

#Bindwidth and range size looks ok


#### Ambient temperature extant ####

# The first step in constructing an IPM with IPMpack is a survival analysis. We use the function ‘survModelComp’ to explore whether survival is related to size, as illustrated in this figure:
x11()
par(mfrow=c(1,1))
survModelComp(dataf= VA_CE, makePlot=TRUE, legendPos="topleft", mainTitle="Survival", ncuts = 30)

# Based on this simple analysis we select the following survival model since it has the lowest AIC value:
so_CE <- makeSurvObj(VA_CE, surv ~ 1)

# We next model growth, conditional on survival. Here, ’growth’ is the process relating size in year t+1 to size in year t. We use the following code to illustrate it in a figure:
growthModelComp(dataf=VA_CE, makePlot=TRUE, legendPos="bottomright", mainTitle="Growth")

# Based on this simple model comparison, we select the following growth model:
go_CE <- makeGrowthObj(VA_CE, sizeNext ~ size + size2)

# With these survival and growth objects in hand, we build a survival/growth (P) matrix.
Pmatrix_CE <- makeIPMPmatrix(survObj=so_CE, growObj=go_CE, minSize=minSize, maxSize=maxSize)

# We plot this P-matrix using the ’image.plot’ function of the fields package:

image.plot(Pmatrix_CE@meshpoints,
           Pmatrix_CE@meshpoints,
           t(Pmatrix_CE),
           main = "Pmatrix: survival and growth",
           xlab = "Size at t",
           ylab = "Size at t+1")
abline(0,1,lty=2,lwd=3)

diagnosticsPmatrix(Pmatrix_CE, growObj=go_CE, survObj=so_CE, correction="constant") 

#Bindwidth looks ok, range size could maybe be fixed


#### Ambient temperature novel ####

# The first step in constructing an IPM with IPMpack is a survival analysis. We use the function ‘survModelComp’ to explore whether survival is related to size, as illustrated in this figure:
x11()
par(mfrow=c(1,1))
survModelComp(dataf= VA_CN, makePlot=TRUE, legendPos="topleft", mainTitle="Survival", ncuts = 30)

# Based on this simple analysis we select the following survival model since it has the lowest AIC value:
so_CN <- makeSurvObj(VA_CN, surv ~ 1)

# We next model growth, conditional on survival. Here, ’growth’ is the process relating size in year t+1 to size in year t. We use the following code to illustrate it in a figure:
growthModelComp(dataf=VA_CN, makePlot=TRUE, legendPos="bottomright", mainTitle="Growth")

# Based on this simple model comparison, we select the following growth model:
go_CN <- makeGrowthObj(VA_CN, sizeNext ~ size + size2)

# With these survival and growth objects in hand, we build a survival/growth (P) matrix.
Pmatrix_CN <- makeIPMPmatrix(survObj=so_CN, growObj=go_CN, minSize=minSize, maxSize=maxSize)

# We plot this P-matrix using the ’image.plot’ function of the fields package:

image.plot(Pmatrix_CN@meshpoints,
           Pmatrix_CN@meshpoints,
           t(Pmatrix_CN),
           main = "Pmatrix: survival and growth",
           xlab = "Size at t",
           ylab = "Size at t+1")
abline(0,1,lty=2,lwd=3)

diagnosticsPmatrix(Pmatrix_CN, growObj=go_CN, survObj=so_CN, correction="constant") 

#Bindwidth looks ok, range size could maybe be fixed


#### Warming control ####

# The first step in constructing an IPM with IPMpack is a survival analysis. We use the function ‘survModelComp’ to explore whether survival is related to size, as illustrated in this figure:
x11()
par(mfrow=c(1,1))
survModelComp(dataf= VA_WC, makePlot=TRUE, legendPos="topleft", mainTitle="Survival", ncuts = 30)

# Based on this simple analysis we select the following survival model since it has the lowest AIC value:
so_WC <- makeSurvObj(VA_WC, surv ~ 1)

# We next model growth, conditional on survival. Here, ’growth’ is the process relating size in year t+1 to size in year t. We use the following code to illustrate it in a figure:
growthModelComp(dataf=VA_WC, makePlot=TRUE, legendPos="bottomright", mainTitle="Growth")

# Based on this simple model comparison, we select the following growth model:
go_WC <- makeGrowthObj(VA_WC, sizeNext ~ size + size2)

# With these survival and growth objects in hand, we build a survival/growth (P) matrix.
Pmatrix_WC <- makeIPMPmatrix(survObj=so_WC, growObj=go_WC, minSize=minSize, maxSize=maxSize)

# We plot this P-matrix using the ’image.plot’ function of the fields package:

image.plot(Pmatrix_WC@meshpoints,
           Pmatrix_WC@meshpoints,
           t(Pmatrix_WC),
           main = "Pmatrix: survival and growth",
           xlab = "Size at t",
           ylab = "Size at t+1")
abline(0,1,lty=2,lwd=3)

diagnosticsPmatrix(Pmatrix_WC, growObj=go_WC, survObj=so_WC, correction="constant") 

#Bindwidth looks ok, range size could maybe be fixed


#### Warming and removal ####

# The first step in constructing an IPM with IPMpack is a survival analysis. We use the function ‘survModelComp’ to explore whether survival is related to size, as illustrated in this figure:
x11()
par(mfrow=c(1,1))
survModelComp(dataf= VA_WR, makePlot=TRUE, legendPos="topleft", mainTitle="Survival", ncuts = 30)

# Based on this simple analysis we select the following survival model since it has the lowest AIC value:
so_WR <- makeSurvObj(VA_WR, surv ~ size)

# We next model growth, conditional on survival. Here, ’growth’ is the process relating size in year t+1 to size in year t. We use the following code to illustrate it in a figure:
growthModelComp(dataf=VA_WR, makePlot=TRUE, legendPos="bottomright", mainTitle="Growth")

# Based on this simple model comparison, we select the following growth model:
go_WR <- makeGrowthObj(VA_WR, sizeNext ~ size)

# With these survival and growth objects in hand, we build a survival/growth (P) matrix.
Pmatrix_WR <- makeIPMPmatrix(survObj=so_WR, growObj=go_WR, minSize=minSize, maxSize=maxSize)

# We plot this P-matrix using the ’image.plot’ function of the fields package:

image.plot(Pmatrix_WR@meshpoints,
           Pmatrix_WR@meshpoints,
           t(Pmatrix_WR),
           main = "Pmatrix: survival and growth",
           xlab = "Size at t",
           ylab = "Size at t+1")
abline(0,1,lty=2,lwd=3)

diagnosticsPmatrix(Pmatrix_WR, growObj=go_WR, survObj=so_WR, correction="constant") 

#Bindwidth looks ok, range size could maybe be fixed


#### Warming and extant ####

# The first step in constructing an IPM with IPMpack is a survival analysis. We use the function ‘survModelComp’ to explore whether survival is related to size, as illustrated in this figure:
x11()
par(mfrow=c(1,1))
survModelComp(dataf= VA_WE, makePlot=TRUE, legendPos="topleft", mainTitle="Survival", ncuts = 30)

# Based on this simple analysis we select the following survival model since it has the lowest AIC value:
so_WE <- makeSurvObj(VA_WE, surv ~ 1)

# We next model growth, conditional on survival. Here, ’growth’ is the process relating size in year t+1 to size in year t. We use the following code to illustrate it in a figure:
growthModelComp(dataf=VA_WE, makePlot=TRUE, legendPos="bottomright", mainTitle="Growth")

# Based on this simple model comparison, we select the following growth model:
go_WE <- makeGrowthObj(VA_WE, sizeNext ~ size)

# With these survival and growth objects in hand, we build a survival/growth (P) matrix.
Pmatrix_WE <- makeIPMPmatrix(survObj=so_WE, growObj=go_WE, minSize=minSize, maxSize=maxSize)

# We plot this P-matrix using the ’image.plot’ function of the fields package:

image.plot(Pmatrix_WE@meshpoints,
           Pmatrix_WE@meshpoints,
           t(Pmatrix_WE),
           main = "Pmatrix: survival and growth",
           xlab = "Size at t",
           ylab = "Size at t+1")
abline(0,1,lty=2,lwd=3)

diagnosticsPmatrix(Pmatrix_WE, growObj=go_WE, survObj=so_CE, correction="constant") 

#Bindwidth looks ok, range size could maybe be fixed


#### Warming and novel ####

# The first step in constructing an IPM with IPMpack is a survival analysis. We use the function ‘survModelComp’ to explore whether survival is related to size, as illustrated in this figure:
x11()
par(mfrow=c(1,1))
survModelComp(dataf= VA_WN, makePlot=TRUE, legendPos="topleft", mainTitle="Survival", ncuts = 30)

# Based on this simple analysis we select the following survival model since it has the lowest AIC value:
so_WN <- makeSurvObj(VA_WN, surv ~ size) #although the surv ~ size + size2 has the lowest AIC the shape of it does not make biological sense.

# We next model growth, conditional on survival. Here, ’growth’ is the process relating size in year t+1 to size in year t. We use the following code to illustrate it in a figure:
growthModelComp(dataf=VA_WN, makePlot=TRUE, legendPos="bottomright", mainTitle="Growth")

# Based on this simple model comparison, we select the following growth model:
go_WN <- makeGrowthObj(VA_WN, sizeNext ~ size)

# With these survival and growth objects in hand, we build a survival/growth (P) matrix.
Pmatrix_WN <- makeIPMPmatrix(survObj=so_WN, growObj=go_WN, minSize=minSize, maxSize=maxSize)

# We plot this P-matrix using the ’image.plot’ function of the fields package:

image.plot(Pmatrix_WN@meshpoints,
           Pmatrix_WN@meshpoints,
           t(Pmatrix_WN),
           main = "Pmatrix: survival and growth",
           xlab = "Size at t",
           ylab = "Size at t+1")
abline(0,1,lty=2,lwd=3)

diagnosticsPmatrix(Pmatrix_WN, growObj=go_WN, survObj=so_CN, correction="constant") 

#Bindwidth looks ok, range size could maybe be fixed

##### F matrix #####
# The fecundity component of an IPM requires analysis of each step of the process of reproduction. Here we start with the first step: whether or not individuals flower in year t (a binomial response). We reemphasize that the population was censused during flowering, and thus we construct the population model based on a pre-reproductive census. Since IPMpack does not have a fecundity model comparison function yet, we must perform model comparison manually:

#### Ambient temperature control ####

AIC(glm(flo.if~1, family = 'binomial', data = VA_CC))
AIC(glm(flo.if~size, family = 'binomial', data = VA_CC))
AIC(glm(flo.if~size+I(size^2), family = 'binomial', data = VA_CC))
AIC(glm(flo.if~size+I(size^2)+I(size^3), family = 'binomial', data = VA_CC))
floweringChosenModel_VA_CC <- flo.if ~ size #Chosen based on AIC

mod1_VA_CC <- glm(flo.if~size, family = 'binomial', data = VA_CC)

par(mfrow=c(1,1))
with(VA_CC, 
      plot(size, jitter(flo.if)))
points(seq(-10, 45, 0.01),
       predict(mod1_VA_CC, newdata = data.frame(size = seq(-10, 45, 0.01)), type = "response"),
       type = "l", col = "red")


AIC(glm(flo.no~1, family = 'poisson', data = VA_CC))
AIC(glm(flo.no~size, family = 'poisson', data = VA_CC))
AIC(glm(flo.no~size+I(size^2), family = 'poisson', data = VA_CC))
AIC(glm(flo.no~size+I(size^2)+I(size^3), family = 'poisson', data = VA_CC))
flowerNumberChosenModel_VA_CC <- flo.no ~ size  #Chosen based on biology by looking at the data

mod2_VA_CC <- glm(flo.no~size, family = 'poisson', data = VA_CC)

with(VA_CC, 
     plot(size, jitter(flo.no)))
points(seq(-10, 45, 0.01),
       predict(mod2_VA_CC, newdata = data.frame(size = seq(-10, 45, 0.01)), type = "response"),
       type = "l", col = "red")



fo_VA_CC <-makeFecObj(VA_CC, 
                Formula= c(floweringChosenModel_VA_CC, flowerNumberChosenModel_VA_CC),
                Family = c("binomial", "poisson"),
                fecConstants = data.frame(seedsPerCap = Seeds_per_capsule_VA_null,
                                          seedlingEstablishmentRate = seedling_est_VA_C_Veg), 
                meanOffspringSize = Seedling_info_VA$mean_Veg,
                sdOffspringSize = Seedling_info_VA$sd)

Fmatrix_VA_CC <- makeIPMFmatrix(fecObj=fo_VA_CC, minSize=minSize, maxSize=maxSize, correction = "continuous", nBigMatrix = 100)

# We plot this P-matrix using the ’image.plot’ function of the fields package:

image.plot(Fmatrix_VA_CC@meshpoints,
           Fmatrix_VA_CC@meshpoints,
           t(Fmatrix_VA_CC),
           main = "Fmatrix: flower and seedlings",
           xlab = "Size at t",
           ylab = "Size at t+1")

#### Ambient temperature removal ####


AIC(glm(flo.if~1, family = 'binomial', data = VA_CR))
AIC(glm(flo.if~size, family = 'binomial', data = VA_CR))
AIC(glm(flo.if~size+I(size^2), family = 'binomial', data = VA_CR))
AIC(glm(flo.if~size+I(size^2)+I(size^3), family = 'binomial', data = VA_CR))
floweringChosenModel_VA_CR <- flo.if ~ size #Chosen based on AIC

mod1_VA_CR <- glm(flo.if~size, family = 'binomial', data = VA_CR)

par(mfrow=c(1,1))
with(VA_CR, 
     plot(size, jitter(flo.if)))
points(seq(-10, 45, 0.01),
       predict(mod1_VA_CR, newdata = data.frame(size = seq(-10, 45, 0.01)), type = "response"),
       type = "l", col = "red")


AIC(glm(flo.no~1, family = 'poisson', data = VA_CR))
AIC(glm(flo.no~size, family = 'poisson', data = VA_CR))
AIC(glm(flo.no~size+I(size^2), family = 'poisson', data = VA_CR))
AIC(glm(flo.no~size+I(size^2)+I(size^3), family = 'poisson', data = VA_CR))
flowerNumberChosenModel_VA_CR <- flo.no ~ size  #Chosen based on biology by looking at the data

mod2_VA_CR <- glm(flo.no~size, family = 'poisson', data = VA_CR)

with(VA_CR, 
     plot(size, jitter(flo.no)))
points(seq(-10, 45, 0.01),
       predict(mod2_VA_CR, newdata = data.frame(size = seq(-10, 45, 0.01)), type = "response"),
       type = "l", col = "red")



fo_VA_CR <-makeFecObj(VA_CR, 
                      Formula= c(floweringChosenModel_VA_CR, flowerNumberChosenModel_VA_CR),
                      Family = c("binomial", "poisson"),
                      fecConstants = data.frame(seedsPerCap = Seeds_per_capsule_VA_null,
                                                seedlingEstablishmentRate = seedling_est_VA_C_NoVeg), 
                      meanOffspringSize = Seedling_info_VA$mean_NoVeg,
                      sdOffspringSize = Seedling_info_VA$sd)

Fmatrix_VA_CR <- makeIPMFmatrix(fecObj=fo_VA_CR, minSize=minSize, maxSize=maxSize, correction = "continuous", nBigMatrix = 100)

# We plot this P-matrix using the ’image.plot’ function of the fields package:

image.plot(Fmatrix_VA_CR@meshpoints,
           Fmatrix_VA_CR@meshpoints,
           t(Fmatrix_VA_CR),
           main = "Fmatrix: flower and seedlings",
           xlab = "Size at t",
           ylab = "Size at t+1")

#### Ambient temperature extant transplant ####


AIC(glm(flo.if~1, family = 'binomial', data = VA_CE))
AIC(glm(flo.if~size, family = 'binomial', data = VA_CE))
AIC(glm(flo.if~size+I(size^2), family = 'binomial', data = VA_CE))
AIC(glm(flo.if~size+I(size^2)+I(size^3), family = 'binomial', data = VA_CE))
floweringChosenModel_VA_CE <- flo.if ~ size + size2 #Chosen based on AIC

mod1_VA_CE <- glm(flo.if ~ size + I(size^2), family = 'binomial', data = VA_CE)

par(mfrow=c(1,1))
with(VA_CE, 
     plot(size, jitter(flo.if)))
points(seq(-10, 45, 0.01),
       predict(mod1_VA_CE, newdata = data.frame(size = seq(-10, 45, 0.01)), type = "response"),
       type = "l", col = "red")


AIC(glm(flo.no~1, family = 'poisson', data = VA_CE))
AIC(glm(flo.no~size, family = 'poisson', data = VA_CE))
AIC(glm(flo.no~size+I(size^2), family = 'poisson', data = VA_CE))
AIC(glm(flo.no~size+I(size^2)+I(size^3), family = 'poisson', data = VA_CE))
flowerNumberChosenModel_VA_CE <- flo.no ~ size  #Chosen based on AIC

mod2_VA_CE <- glm(flo.no~size, family = 'poisson', data = VA_CE)

with(VA_CE, 
     plot(size, jitter(flo.no)))
points(seq(-10, 45, 0.01),
       predict(mod2_VA_CE, newdata = data.frame(size = seq(-10, 45, 0.01)), type = "response"),
       type = "l", col = "red")



fo_VA_CE <-makeFecObj(VA_CE, 
                      Formula= c(floweringChosenModel_VA_CE, flowerNumberChosenModel_VA_CE),
                      Family = c("binomial", "poisson"),
                      fecConstants = data.frame(seedsPerCap = Seeds_per_capsule_VA_null,
                                                seedlingEstablishmentRate = seedling_est_VA_C_Veg), 
                      meanOffspringSize = Seedling_info_VA$mean_Veg,
                      sdOffspringSize = Seedling_info_VA$sd)

Fmatrix_VA_CE <- makeIPMFmatrix(fecObj=fo_VA_CE, minSize=minSize, maxSize=maxSize, correction = "continuous", nBigMatrix = 100)

# We plot this P-matrix using the ’image.plot’ function of the fields package:

image.plot(Fmatrix_VA_CE@meshpoints,
           Fmatrix_VA_CE@meshpoints,
           t(Fmatrix_VA_CE),
           main = "Fmatrix: flower and seedlings",
           xlab = "Size at t",
           ylab = "Size at t+1")


#### Ambient temperature novel transplant ####


AIC(glm(flo.if~1, family = 'binomial', data = VA_CN))
AIC(glm(flo.if~size, family = 'binomial', data = VA_CN))
AIC(glm(flo.if~size+I(size^2), family = 'binomial', data = VA_CN))
AIC(glm(flo.if~size+I(size^2)+I(size^3), family = 'binomial', data = VA_CN))
floweringChosenModel_VA_CN <- flo.if ~ size #Chosen based on biology

mod1_VA_CN <- glm(flo.if ~ size, family = 'binomial', data = VA_CN)

par(mfrow=c(1,1))
with(VA_CN, 
     plot(size, jitter(flo.if)))
points(seq(-10, 45, 0.01),
       predict(mod1_VA_CN, newdata = data.frame(size = seq(-10, 45, 0.01)), type = "response"),
       type = "l", col = "red")


AIC(glm(flo.no~1, family = 'poisson', data = VA_CN))
AIC(glm(flo.no~size, family = 'poisson', data = VA_CN))
AIC(glm(flo.no~size+I(size^2), family = 'poisson', data = VA_CN))
AIC(glm(flo.no~size+I(size^2)+I(size^3), family = 'poisson', data = VA_CN))
flowerNumberChosenModel_VA_CN <- flo.no ~ size  #Chosen based on AIC

mod2_VA_CN <- glm(flo.no~size, family = 'poisson', data = VA_CN)

with(VA_CN, 
     plot(size, jitter(flo.no)))
points(seq(-10, 45, 0.01),
       predict(mod2_VA_CN, newdata = data.frame(size = seq(-10, 45, 0.01)), type = "response"),
       type = "l", col = "red")



fo_VA_CN <-makeFecObj(VA_CN, 
                      Formula= c(floweringChosenModel_VA_CN, flowerNumberChosenModel_VA_CN),
                      Family = c("binomial", "poisson"),
                      fecConstants = data.frame(seedsPerCap = Seeds_per_capsule_VA_null,
                                                seedlingEstablishmentRate = seedling_est_VA_C_Veg), 
                      meanOffspringSize = Seedling_info_VA$mean_Veg,
                      sdOffspringSize = Seedling_info_VA$sd)

Fmatrix_VA_CN <- makeIPMFmatrix(fecObj=fo_VA_CN, minSize=minSize, maxSize=maxSize, correction = "continuous", nBigMatrix = 100)

# We plot this P-matrix using the ’image.plot’ function of the fields package:

image.plot(Fmatrix_VA_CN@meshpoints,
           Fmatrix_VA_CN@meshpoints,
           t(Fmatrix_VA_CN),
           main = "Fmatrix: flower and seedlings",
           xlab = "Size at t",
           ylab = "Size at t+1")

#### Warming control ####

AIC(glm(flo.if~1, family = 'binomial', data = VA_WC))
AIC(glm(flo.if~size, family = 'binomial', data = VA_WC))
AIC(glm(flo.if~size+I(size^2), family = 'binomial', data = VA_WC))
AIC(glm(flo.if~size+I(size^2)+I(size^3), family = 'binomial', data = VA_WC))
floweringChosenModel_VA_WC <- flo.if ~ size  #Chosen based on AIC

mod1_VA_WC <- glm(flo.if~size, family = 'binomial', data = VA_WC)

par(mfrow=c(1,1))
with(VA_WC, 
     plot(size, jitter(flo.if)))
points(seq(-10, 45, 0.01),
       predict(mod1_VA_WC, newdata = data.frame(size = seq(-10, 45, 0.01)), type = "response"),
       type = "l", col = "red")


AIC(glm(flo.no~1, family = 'poisson', data = VA_WC))
AIC(glm(flo.no~size, family = 'poisson', data = VA_WC))
AIC(glm(flo.no~size+I(size^2), family = 'poisson', data = VA_WC))
AIC(glm(flo.no~size+I(size^2)+I(size^3), family = 'poisson', data = VA_WC))
flowerNumberChosenModel_VA_WC <- flo.no ~ size  #Chosen based on biology 

mod2_VA_WC <- glm(flo.no~size, family = 'poisson', data = VA_WC)

with(VA_WC, 
     plot(size, jitter(flo.no)))
points(seq(-10, 45, 0.01),
       predict(mod2_VA_WC, newdata = data.frame(size = seq(-10, 45, 0.01)), type = "response"),
       type = "l", col = "red")



fo_VA_WC <-makeFecObj(VA_WC, 
                      Formula= c(floweringChosenModel_VA_WC, flowerNumberChosenModel_VA_WC),
                      Family = c("binomial", "poisson"),
                      fecConstants = data.frame(seedsPerCap = Seeds_per_capsule_VA_null,
                                                seedlingEstablishmentRate = seedling_est_VA_OTC_Veg), 
                      meanOffspringSize = Seedling_info_VA$mean_Veg,
                      sdOffspringSize = Seedling_info_VA$sd)

Fmatrix_VA_WC <- makeIPMFmatrix(fecObj=fo_VA_WC, minSize=minSize, maxSize=maxSize, correction = "continuous", nBigMatrix = 100)

# We plot this P-matrix using the ’image.plot’ function of the fields package:

image.plot(Fmatrix_VA_WC@meshpoints,
           Fmatrix_VA_WC@meshpoints,
           t(Fmatrix_VA_WC),
           main = "Fmatrix: flower and seedlings",
           xlab = "Size at t",
           ylab = "Size at t+1")

#### Warming and removal ####


AIC(glm(flo.if~1, family = 'binomial', data = VA_WR))
AIC(glm(flo.if~size, family = 'binomial', data = VA_WR))
AIC(glm(flo.if~size+I(size^2), family = 'binomial', data = VA_WR))
AIC(glm(flo.if~size+I(size^2)+I(size^3), family = 'binomial', data = VA_WR))
floweringChosenModel_VA_WR <- flo.if ~ size #Chosen based on AIC

mod1_VA_WR <- glm(flo.if~size, family = 'binomial', data = VA_WR)

par(mfrow=c(1,1))
with(VA_WR, 
     plot(size, jitter(flo.if)))
points(seq(-10, 45, 0.01),
       predict(mod1_VA_WR, newdata = data.frame(size = seq(-10, 45, 0.01)), type = "response"),
       type = "l", col = "red")


AIC(glm(flo.no~1, family = 'poisson', data = VA_WR))
AIC(glm(flo.no~size, family = 'poisson', data = VA_WR))
AIC(glm(flo.no~size+I(size^2), family = 'poisson', data = VA_WR))
AIC(glm(flo.no~size+I(size^2)+I(size^3), family = 'poisson', data = VA_WR))
flowerNumberChosenModel_VA_WR <- flo.no ~ size  #Chosen based on AIC

mod2_VA_WR <- glm(flo.no~size, family = 'poisson', data = VA_WR)

with(VA_WR, 
     plot(size, jitter(flo.no)))
points(seq(-10, 45, 0.01),
       predict(mod2_VA_WR, newdata = data.frame(size = seq(-10, 45, 0.01)), type = "response"),
       type = "l", col = "red")

fo_VA_WR <-makeFecObj(VA_WR, 
                      Formula= c(floweringChosenModel_VA_WR, flowerNumberChosenModel_VA_WR),
                      Family = c("binomial", "poisson"),
                      fecConstants = data.frame(seedsPerCap = Seeds_per_capsule_VA_null,
                                                seedlingEstablishmentRate = seedling_est_VA_OTC_NoVeg), 
                      meanOffspringSize = Seedling_info_VA$mean_NoVeg,
                      sdOffspringSize = Seedling_info_VA$sd)

Fmatrix_VA_WR <- makeIPMFmatrix(fecObj=fo_VA_WR, minSize=minSize, maxSize=maxSize, correction = "continuous", nBigMatrix = 100)

# We plot this P-matrix using the ’image.plot’ function of the fields package:

image.plot(Fmatrix_VA_WR@meshpoints,
           Fmatrix_VA_WR@meshpoints,
           t(Fmatrix_VA_WR),
           main = "Fmatrix: flower and seedlings",
           xlab = "Size at t",
           ylab = "Size at t+1")

#### Warming and extant transplant ####


AIC(glm(flo.if~1, family = 'binomial', data = VA_WE))
AIC(glm(flo.if~size, family = 'binomial', data = VA_WE))
AIC(glm(flo.if~size+I(size^2), family = 'binomial', data = VA_WE))
AIC(glm(flo.if~size+I(size^2)+I(size^3), family = 'binomial', data = VA_WE))
floweringChosenModel_VA_WE <- flo.if ~ size + size2 #Chosen based on AIC

mod1_VA_WE <- glm(flo.if ~ size + I(size^2), family = 'binomial', data = VA_CE)

par(mfrow=c(1,1))
with(VA_WE, 
     plot(size, jitter(flo.if)))
points(seq(-10, 45, 0.01),
       predict(mod1_VA_WE, newdata = data.frame(size = seq(-10, 45, 0.01)), type = "response"),
       type = "l", col = "red")


AIC(glm(flo.no~1, family = 'poisson', data = VA_WE))
AIC(glm(flo.no~size, family = 'poisson', data = VA_WE))
AIC(glm(flo.no~size+I(size^2), family = 'poisson', data = VA_WE))
AIC(glm(flo.no~size+I(size^2)+I(size^3), family = 'poisson', data = VA_WE))
flowerNumberChosenModel_VA_WE <- flo.no ~ size  #Chosen based on AIC

mod2_VA_WE <- glm(flo.no~size, family = 'poisson', data = VA_WE)

with(VA_WE, 
     plot(size, jitter(flo.no)))
points(seq(-10, 45, 0.01),
       predict(mod2_VA_WE, newdata = data.frame(size = seq(-10, 45, 0.01)), type = "response"),
       type = "l", col = "red")



fo_VA_WE <-makeFecObj(VA_WE, 
                      Formula= c(floweringChosenModel_VA_WE, flowerNumberChosenModel_VA_WE),
                      Family = c("binomial", "poisson"),
                      fecConstants = data.frame(seedsPerCap = Seeds_per_capsule_VA_null,
                                                seedlingEstablishmentRate = seedling_est_VA_OTC_Veg), 
                      meanOffspringSize = Seedling_info_VA$mean_Veg,
                      sdOffspringSize = Seedling_info_VA$sd)

Fmatrix_VA_WE <- makeIPMFmatrix(fecObj=fo_VA_WE, minSize=minSize, maxSize=maxSize, correction = "continuous", nBigMatrix = 100)

# We plot this P-matrix using the ’image.plot’ function of the fields package:

image.plot(Fmatrix_VA_WE@meshpoints,
           Fmatrix_VA_WE@meshpoints,
           t(Fmatrix_VA_WE),
           main = "Fmatrix: flower and seedlings",
           xlab = "Size at t",
           ylab = "Size at t+1")


#### Warming and novel transplant ####


AIC(glm(flo.if~1, family = 'binomial', data = VA_WN))
AIC(glm(flo.if~size, family = 'binomial', data = VA_WN))
AIC(glm(flo.if~size+I(size^2), family = 'binomial', data = VA_WN))
AIC(glm(flo.if~size+I(size^2)+I(size^3), family = 'binomial', data = VA_WN))
floweringChosenModel_VA_WN <- flo.if ~ size #Chosen based on biology

mod1_VA_WN <- glm(flo.if ~ size, family = 'binomial', data = VA_WN)

par(mfrow=c(1,1))
with(VA_WN, 
     plot(size, jitter(flo.if)))
points(seq(-10, 45, 0.01),
       predict(mod1_VA_WN, newdata = data.frame(size = seq(-10, 45, 0.01)), type = "response"),
       type = "l", col = "red")


AIC(glm(flo.no~1, family = 'poisson', data = VA_WN))
AIC(glm(flo.no~size, family = 'poisson', data = VA_WN))
AIC(glm(flo.no~size+I(size^2), family = 'poisson', data = VA_WN))
AIC(glm(flo.no~size+I(size^2)+I(size^3), family = 'poisson', data = VA_WN))
flowerNumberChosenModel_VA_WN <- flo.no ~ size  #Chosen based on AIC

mod2_VA_WN <- glm(flo.no ~ size, family = 'poisson', data = VA_WN)

with(VA_WN, 
     plot(size, jitter(flo.no)))
points(seq(-10, 45, 0.01),
       predict(mod2_VA_WN, newdata = data.frame(size = seq(-10, 45, 0.01)), type = "response"),
       type = "l", col = "red")



fo_VA_WN <-makeFecObj(VA_WN, 
                      Formula= c(floweringChosenModel_VA_WN, flowerNumberChosenModel_VA_WN),
                      Family = c("binomial", "poisson"),
                      fecConstants = data.frame(seedsPerCap = Seeds_per_capsule_VA_null,
                                                seedlingEstablishmentRate = seedling_est_VA_OTC_Veg), 
                      meanOffspringSize = Seedling_info_VA$mean_Veg,
                      sdOffspringSize = Seedling_info_VA$sd)

Fmatrix_VA_WN <- makeIPMFmatrix(fecObj=fo_VA_WN, minSize=minSize, maxSize=maxSize, correction = "continuous", nBigMatrix = 100)

# We plot this P-matrix using the ’image.plot’ function of the fields package:

image.plot(Fmatrix_VA_WN@meshpoints,
           Fmatrix_VA_WN@meshpoints,
           t(Fmatrix_VA_WN),
           main = "Fmatrix: flower and seedlings",
           xlab = "Size at t",
           ylab = "Size at t+1")

##### Clonal objects #####

VA_CC_clones <- VA_CC %>% 
   filter(offspringNext == "clonal") %>% 
   mutate(number_orphans = case_when(is.na(size) ~ 1,
                                     !is.na(size) ~0)) %>% 
   mutate(total_num_orphan = sum(number_orphans),
          total_num_clones = n()) %>% 
   fill(total_num_orphan, .direction = "downup") %>% 
   mutate(prop_orphan = total_num_orphan/total_num_clones)

VA_CR_clones <- VA_CR %>% 
   filter(offspringNext == "clonal") %>% 
   mutate(number_orphans = case_when(is.na(size) ~ 1,
                                     !is.na(size) ~0)) %>% 
   mutate(total_num_orphan = sum(number_orphans),
          total_num_clones = n()) %>% 
   fill(total_num_orphan, .direction = "downup") %>% 
   mutate(prop_orphan = total_num_orphan/total_num_clones)

VA_CE_clones <- VA_CE %>% 
   filter(offspringNext == "clonal") %>% 
   mutate(number_orphans = case_when(is.na(size) ~ 1,
                                     !is.na(size) ~0)) %>% 
   mutate(total_num_orphan = sum(number_orphans),
          total_num_clones = n()) %>% 
   fill(total_num_orphan, .direction = "downup") %>% 
   mutate(prop_orphan = total_num_orphan/total_num_clones)

VA_CN_clones <- VA_CN %>% 
   filter(offspringNext == "clonal") %>% 
   mutate(number_orphans = case_when(is.na(size) ~ 1,
                                     !is.na(size) ~0)) %>% 
   mutate(total_num_orphan = sum(number_orphans),
          total_num_clones = n()) %>% 
   fill(total_num_orphan, .direction = "downup") %>% 
   mutate(prop_orphan = total_num_orphan/total_num_clones)

VA_WC_clones <- VA_WC %>% 
   filter(offspringNext == "clonal") %>% 
   mutate(number_orphans = case_when(is.na(size) ~ 1,
                                     !is.na(size) ~0)) %>% 
   mutate(total_num_orphan = sum(number_orphans),
          total_num_clones = n()) %>% 
   fill(total_num_orphan, .direction = "downup") %>% 
   mutate(prop_orphan = total_num_orphan/total_num_clones)

VA_WR_clones <- VA_WR %>% 
   filter(offspringNext == "clonal") %>% 
   mutate(number_orphans = case_when(is.na(size) ~ 1,
                                     !is.na(size) ~0)) %>% 
   mutate(total_num_orphan = sum(number_orphans),
          total_num_clones = n()) %>% 
   fill(total_num_orphan, .direction = "downup") %>% 
   mutate(prop_orphan = total_num_orphan/total_num_clones)

VA_WE_clones <- VA_WE %>% 
   filter(offspringNext == "clonal") %>% 
   mutate(number_orphans = case_when(is.na(size) ~ 1,
                                     !is.na(size) ~0)) %>% 
   mutate(total_num_orphan = sum(number_orphans),
          total_num_clones = n()) %>% 
   fill(total_num_orphan, .direction = "downup") %>% 
   mutate(prop_orphan = total_num_orphan/total_num_clones)

VA_WN_clones <- VA_WN %>% 
   filter(offspringNext == "clonal") %>% 
   mutate(number_orphans = case_when(is.na(size) ~ 1,
                                     !is.na(size) ~0)) %>% 
   mutate(total_num_orphan = sum(number_orphans),
          total_num_clones = n()) %>% 
   fill(total_num_orphan, .direction = "downup") %>% 
   mutate(prop_orphan = total_num_orphan/total_num_clones)

#### Ambient temperature control ####

#Is the production of clones size dependent
AIC(glm(clo.if~1, family = 'binomial', data = VA_CC))
AIC(glm(clo.if~size, family = 'binomial', data = VA_CC))
AIC(glm(clo.if~size+I(size^2), family = 'binomial', data = VA_CC))
CloneChosenModel_VA_CC <- clo.if ~ size + size2 #Chosen based on biology

mod1_VA_CC <- glm(clo.if ~ size + I(size^2), family = 'binomial', data = VA_CC)

par(mfrow=c(1,1))
with(VA_CC, 
     plot(size, jitter(clo.if)))
points(seq(-10, 45, 0.01),
       predict(mod1_VA_CC, newdata = data.frame(size = seq(-10, 45, 0.01)), type = "response"),
       type = "l", col = "red")

#If you produce clones, does how many clones you make change with size of the mother 
AIC(glm(clo.no~1, family = 'poisson', data = VA_CC))
AIC(glm(clo.no~size, family = 'poisson', data = VA_CC))
AIC(glm(clo.no~size+I(size^2), family = 'poisson', data = VA_CC))
CloneNumberChosenModel_VA_CC <- clo.no ~ size  #Chosen based on AIC

mod2_VA_CC <- glm(clo.no ~ 1, family = 'poisson', data = VA_CC)

with(VA_CC, 
     plot(size, jitter(clo.no)))
points(seq(-10, 45, 0.01),
       predict(mod2_VA_CC, newdata = data.frame(size = seq(-10, 45, 0.01)), type = "response"),
       type = "l", col = "red")

# Clonal size depending on mother size
x11()
par(mfrow=c(1,1))
growthModelComp(dataf=VA_CC_clones, makePlot=TRUE, legendPos="topright", mainTitle="Growth")
CloneSizeVariable_VA_CC <- "1"  #Chosen based on AIC

go_clone_VA_CC <- makeGrowthObj(VA_CC_clones, sizeNext ~ 1)

#Make clonal object
co_VA_CC <- makeClonalObj(VA_CC, fecConstants=data.frame(correctionForOrphans= 1/(1-VA_CC_clones$prop_orphan[1])),
                    offspringSizeExplanatoryVariables = CloneSizeVariable_VA_CC, Formula = c(CloneChosenModel_VA_CC, CloneNumberChosenModel_VA_CC),
                    Family = c("binomial","poisson"), Transform=c("none","none"))
#,offspringSplitter=data.frame(seedbank=0,continuous=1)




#### Ambient temperature removal ####

#Is the production of clones size dependent
AIC(glm(clo.if~1, family = 'binomial', data = VA_CR))
AIC(glm(clo.if~size, family = 'binomial', data = VA_CR))
AIC(glm(clo.if~size+I(size^2), family = 'binomial', data = VA_CR))
CloneChosenModel_VA_CR <- flo.if ~ size + size2 #Chosen based on biology

mod1_VA_CR <- glm(clo.if ~ size +I(size^2), family = 'binomial', data = VA_CR)

par(mfrow=c(1,1))
with(VA_CR, 
     plot(size, jitter(clo.if)))
points(seq(-10, 45, 0.01),
       predict(mod1_VA_CR, newdata = data.frame(size = seq(-10, 45, 0.01)), type = "response"),
       type = "l", col = "red")

#If you produce clones, does how many clones you make change with size of the mother 
AIC(glm(clo.no~1, family = 'poisson', data = VA_CR))
AIC(glm(clo.no~size, family = 'poisson', data = VA_CR))
AIC(glm(clo.no~size+I(size^2), family = 'poisson', data = VA_CR))
CloneNumberChosenModel_VA_CR <- clo.no ~ size  #Chosen based on AIC

mod2_VA_CR <- glm(clo.no ~ 1, family = 'poisson', data = VA_CR)

with(VA_CR, 
     plot(size, jitter(clo.no)))
points(seq(-10, 45, 0.01),
       predict(mod2_VA_CR, newdata = data.frame(size = seq(-10, 45, 0.01)), type = "response"),
       type = "l", col = "red")

# Clonal size depending on mother size
x11()
par(mfrow=c(1,1))
growthModelComp(dataf=VA_CR_clones, makePlot=TRUE, legendPos="topright", mainTitle="Growth")
CloneSizeVariable_VA_CR <- "1"  #Chosen based on AIC

go_clone_VA_CR <- makeGrowthObj(VA_CR_clones, sizeNext ~ 1)

#Make clonal object
co_VA_CR <- makeClonalObj(VA_CR, fecConstants=data.frame(correctionForOrphans= 1/(1-VA_CR_clones$prop_orphan[1])),
                          offspringSizeExplanatoryVariables = CloneSizeVariable_VA_CR, Formula = c(CloneChosenModel_VA_CR, CloneNumberChosenModel_VA_CR),
                          Family = c("binomial","poisson"), Transform=c("none","none"),offspringSplitter=data.frame(seedbank=0,continuous=1))




