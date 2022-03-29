########################################################################
### Script for building population models for the INCLINE experiment ###
########################################################################


#### Libraries ####
library(tidyverse)
library(lme4)
library(lmerTest)
library(IPMpack)

#### Downloading data from OSF ####

#Make script for reading in ready made data from OSF

#### Load data ####

#Load data 

# To understand the data, we plot survival, growth/shrinkage/stasis, number of seeds, and size of recruits:
par(mfrow=c(2,2),mar=c(4,4,2,1))
plot(Ver_alp_2018_2021$size,jitter(Ver_alp_2018_2021$surv),xlab="Size (t)", ylab="Survival to t+1")
plot(Ver_alp_2018_2021$size, Ver_alp_2018_2021$sizeNext,xlab="Size (t)",ylab="Size (t+1)") 
plot(Ver_alp_2018_2021$size,jitter(Ver_alp_2018_2021$flo.if),xlab="Size (t)", ylab="Flowering probability") 
plot(Ver_alp_2018_2021$size, Ver_alp_2018_2021$fec,xlab="Size (t)",ylab="Number of seeds") 
Ver_alp_2018_2021 %>% filter(offspringNext == "sexual") %>% ggplot(aes( x = sizeNext)) + geom_histogram() + ylab("Seedling size")
Ver_alp_2018_2021 %>% filter(offspringNext == "clone") %>% ggplot(aes( x = sizeNext)) + geom_histogram() + ylab("Clone size")
