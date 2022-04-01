########################################################################
### Script for building population models for the INCLINE experiment ###
########################################################################


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


# To understand the data, we plot survival, growth/shrinkage/stasis, number of seeds, and size of recruits:
par(mfrow=c(2,2),mar=c(4,4,2,1))
plot(Ver_alp_2018_2021$size,jitter(Ver_alp_2018_2021$surv),xlab="Size (t)", ylab="Survival to t+1")
plot(Ver_alp_2018_2021$size, Ver_alp_2018_2021$sizeNext,xlab="Size (t)",ylab="Size (t+1)") 
plot(Ver_alp_2018_2021$size,jitter(Ver_alp_2018_2021$flo.if),xlab="Size (t)", ylab="Flowering probability") 
plot(Ver_alp_2018_2021$size, Ver_alp_2018_2021$fec,xlab="Size (t)",ylab="Number of seeds") 

Ver_alp_2018_2021 %>% filter(offspringNext == "sexual") %>% ggplot(aes( x = sizeNext)) + geom_histogram() + ylab("Seedling size")
Ver_alp_2018_2021 %>% filter(offspringNext == "clone") %>% ggplot(aes( x = sizeNext)) + geom_histogram() + ylab("Clone size")

VA_CC <- Ver_alp_2018_2021 %>% filter(OTC == "C" & treatment == "C")
VA_CR <- Ver_alp_2018_2021 %>% filter(OTC == "C" & treatment == "R")
VA_CE <- Ver_alp_2018_2021 %>% filter(OTC == "C" & treatment == "E")
VA_CN <- Ver_alp_2018_2021 %>% filter(OTC == "C" & treatment == "N")
VA_WC <- Ver_alp_2018_2021 %>% filter(OTC == "W" & treatment == "C")
VA_WR <- Ver_alp_2018_2021 %>% filter(OTC == "W" & treatment == "R")
VA_WE <- Ver_alp_2018_2021 %>% filter(OTC == "W" & treatment == "E")
VA_WN <- Ver_alp_2018_2021 %>% filter(OTC == "W" & treatment == "N")

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


#### Warm control ####

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


#### Ambient temperature removal ####

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


#### Ambient temperature extant ####

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


#### Ambient temperature novel ####

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


# FECUNDITY KERNEL
# The fecundity component of an IPM requires analysis of each step of the process of reproduction. Here we start with the first step: whether or not individuals flower in year t (a binomial response). We reemphasize that the population was censused during flowering, and thus we construct the population model based on a pre-reproductive census. Since IPMpack does not have a fecundity model comparison function yet, we must perform model comparison manually:

### This does not work

 Ver_alp_2018_2021_flo <- Ver_alp_2018_2021 %>% 
   ungroup() %>% 
   as.data.frame()
# 



AIC(glm(flo.if~1, family = 'binomial', data = Ver_alp_2018_2021_flo))
AIC(glm(flo.if~size, family = 'binomial', data = Ver_alp_2018_2021_flo))
mod1 <- glm(flo.if~size+I(size^2), family = 'binomial', data = Ver_alp_2018_2021_flo)
AIC(mod1)
AIC(glm(flo.if~size+I(size^2)+I(size^3), family = 'binomial', data = Ver_alp_2018_2021_flo))
floweringChosenModel <- flo.if ~ size + size2 #Chosen based on AIC

par(mfrow=c(1,1))
with(Ver_alp_2018_2021_flo, 
      plot(size, jitter(flo.if)))
points(seq(-10, 45, 0.01),
       predict(mod1, newdata = data.frame(size = seq(-10, 45, 0.01)), type = "response"),
       type = "l", col = "red")



AIC(glm(flo.no~1, family = 'poisson', data = Ver_alp_2018_2021_flo))
AIC(glm(flo.no~size, family = 'poisson', data = Ver_alp_2018_2021_flo))
mod2 <- glm(flo.no~size+I(size^2), family = 'poisson', data = Ver_alp_2018_2021_flo)
AIC(mod2)
AIC(glm(flo.no~size+I(size^2)+I(size^3), family = 'poisson', data = Ver_alp_2018_2021_flo))
floweringChosenModel <- flo.if ~ size + size2 #Chosen based on AIC

with(Ver_alp_2018_2021_flo, 
     plot(size, jitter(flo.no)))
points(seq(-10, 45, 0.01),
       predict(mod2, newdata = data.frame(size = seq(-10, 45, 0.01)), type = "response"),
       type = "l", col = "red")



fo <-makeFecObj(Ver_alp_2018_2021_flo, 
                Formula= c(flo.if~size+size2, flo.no~size+size2),
                Family = c("binomial", "poisson"),
                fecConstants = data.frame(seedsPerCap = Seeds_per_capsule_VA_null,
                                          seedlingEstablishmentRate = seedling_est_VA_C_Veg), 
                meanOffspringSize = Seedling_info_VA$mean_Veg,
                sdOffspringSize = Seedling_info_VA$sd)

Fmatrix <- makeIPMFmatrix(fecObj=fo, minSize=minSize, maxSize=maxSize, correction = "continuous", nBigMatrix = 100)

# We plot this P-matrix using the ’image.plot’ function of the fields package:

image.plot(Fmatrix@meshpoints,
           Fmatrix@meshpoints,
           t(Fmatrix),
           main = "Fmatrix: flower and seedlings",
           xlab = "Size at t",
           ylab = "Size at t+1")
abline(0,1,lty=2,lwd=3)
