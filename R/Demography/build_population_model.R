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
x<-seq(from=0,to=10,length=1001)
x0<-data.frame(size=x,size2=x*x)
minSize<-min(Ver_alp_2018_2021$size,na.rm=T)-1
maxSize<-max(Ver_alp_2018_2021$size,na.rm=T)+2

# To understand the data, we plot survival, growth/shrinkage/stasis, number of seeds, and size of recruits:
par(mfrow=c(2,2),mar=c(4,4,2,1))
plot(Ver_alp_2018_2021$size,jitter(Ver_alp_2018_2021$surv),xlab="Size (t)", ylab="Survival to t+1")
plot(Ver_alp_2018_2021$size, Ver_alp_2018_2021$sizeNext,xlab="Size (t)",ylab="Size (t+1)") 
plot(Ver_alp_2018_2021$size,jitter(Ver_alp_2018_2021$flo.if),xlab="Size (t)", ylab="Flowering probability") 
plot(Ver_alp_2018_2021$size, Ver_alp_2018_2021$fec,xlab="Size (t)",ylab="Number of seeds") 

Ver_alp_2018_2021 %>% filter(offspringNext == "sexual") %>% ggplot(aes( x = sizeNext)) + geom_histogram() + ylab("Seedling size")
Ver_alp_2018_2021 %>% filter(offspringNext == "clone") %>% ggplot(aes( x = sizeNext)) + geom_histogram() + ylab("Clone size")


# The first step in constructing an IPM with IPMpack is a survival analysis. We use the function ‘survModelComp’ to explore whether survival is related to size, as illustrated in this figure:
par(mfrow=c(1,1))
survModelComp(dataf=Ver_alp_2018_2021, makePlot=TRUE, legendPos="topleft", mainTitle="Survival", ncuts = 30)

# Based on this simple analysis we select the following survival model since it has the lowest AIC value:
so <- makeSurvObj(Ver_alp_2018_2021, surv ~ 1)

# We next model growth, conditional on survival. Here, ’growth’ is the process relating size in year t+1 to size in year t. We use the following code to illustrate it in a figure:
growthModelComp(dataf=Ver_alp_2018_2021, makePlot=TRUE, legendPos="bottomright", mainTitle="Growth")

# Based on this simple model comparison, we select the following growth model:
go <- makeGrowthObj(Ver_alp_2018_2021, sizeNext~size)

# With these survival and growth objects in hand, we build a survival/growth (P) matrix.
Pmatrix<-makeIPMPmatrix(survObj=so,growObj=go,minSize=minSize,maxSize=maxSize)

# We plot this P-matrix using the ’image.plot’ function of the fields package:

image.plot(Pmatrix@meshpoints,
           Pmatrix@meshpoints,
           t(Pmatrix),
           main = "Pmatrix: survival and growth",
           xlab = "Size at t",
           ylab = "Size at t+1")
abline(0,1,lty=2,lwd=3)

diagnosticsPmatrix(Pmatrix, growObj=go, survObj=so, correction="constant")
#Looks good - binwidth and range is ok.


# FECUNDITY KERNEL
# The fecundity component of an IPM requires analysis of each step of the process of reproduction. Here we start with the first step: whether or not individuals flower in year t (a binomial response). We reemphasize that the population was censused during flowering, and thus we construct the population model based on a pre-reproductive census. Since IPMpack does not have a fecundity model comparison function yet, we must perform model comparison manually:
fo1<-makeFecObj(Ver_alp_2018_2021, Formula=flo.if~1, Family = "binomial") # Intercept only model 
fo2<-makeFecObj(Ver_alp_2018_2021, Formula=flo.if~size, Family = "binomial") 
fo3<-makeFecObj(Ver_alp_2018_2021, Formula=flo.if~size+size2, Family = "binomial")

# We plot these models for comparison
fs <- order(Ver_alp_2018_2021$size)
fs.fec <- (Ver_alp_2018_2021$flo.no)[fs]
fs.size <- (Ver_alp_2018_2021$size)[fs]
pfz <- tapply(fs.size, as.numeric(cut(fs.size, 21)), mean, na.rm = TRUE)
ps <- tapply(fs.fec, as.numeric(cut(fs.size, 21)), mean, na.rm = TRUE)
plot(as.numeric(pfz), as.numeric(ps), pch = 19, cex=2, col="blue",ylim=c(0,1),
     xlab="size", ylab="proportion flowering", main="")
y0<-predict(fo1@fitFec[[1]],newdata=x0,type="response")
lines(x,y0,col="red")
y0<-predict(fo2@fitFec[[1]],newdata=x0,type="response")
lines(x,y0,col="green")
y0<-predict(fo3@fitFec[[1]],newdata=x0,type="response")
lines(x,y0,col="blue")
legend("topleft", legend = sprintf("%s: %s = %.1f",c("1","size","size+size2"), c("AIC"),c(AIC(fo1@fitFec[[1]]),AIC(fo2@fitFec[[1]]),AIC(fo3@fitFec[[1]]))), col = c(2:4),lty = 1, xjust = 1, bg = "white")
