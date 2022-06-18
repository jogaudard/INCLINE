####################################################################
### Script for testing seed mass differences between populations ###
####################################################################

#### Libraries ####
library(tidyverse)
library(dataDownloader)
library(osfr)
library(lubridate)

#### Downloading data from OSF ####

#osf_auth(token = "get from my document every time you run the code") 

#### Load data ####

seed_mass <- read.delim("data/Germination/Seed_weight.csv", sep = ",", dec = ".")


#### Clean data ####

#Be aware that one of the batches of seeds contain a different number than the others. So we either need to calculate the weight of individual seeds by dividing by the number_of_seeds column, or filter out the one that has 33 seeds and not 50 seeds.

seed_mass <- seed_mass %>% 
  mutate(weight_per_seed = total_weight/number_of_seeds) %>% 
  mutate(siteID = as.factor(siteID),
         species = as.factor(species)) %>% 
  mutate(siteID = factor(siteID, levels = c( "Lavisdalen", "Ulvehaugen", "Gudmedalen", "Skjellingahaugen"))) %>% 
  mutate(species = factor(species, levels = c("Sib_pro", "Ver_alp")))
  

#### Average seed weight in mg ####

seed_mass %>% group_by(species) %>% mutate(mean = mean(weight_per_seed) * 1000) %>% select(species, mean) %>% unique()

#### Testing for difference between species and populations ####

test <- lm(weight_per_seed ~ species*siteID, data = seed_mass)
summary(test)

ggplot(aes(x = siteID, y = weight_per_seed, fill = species), data = seed_mass) +
  geom_boxplot()+
  geom_jitter(alpha = 0.3, width = 0.05)+
  facet_wrap(~species, nrow = 2, scales = "free_y") +
  theme_bw()
                                           