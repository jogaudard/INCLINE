##################################################################
### Script for cleaning raw seeds per capsule data for INCLINE ###
##################################################################

#### Libraries ####
library(tidyverse)
library(dataDownloader)
library(osfr)
library(lubridate)
library(conflicted)
library(stringr)

#### Select preferences for conflicts ####

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

#### Downloading data from OSF ####

#osf_auth(token = "get from my document every time you run the code") 

# get_file(node = "zhk3m",
#          file = "Seeds_per_capsule.csv",
#          path = "data/Demography",
#          remote_path = "RawData/Demography")

#### Load data ####

Seeds_per_capsule <- read_csv2("data/Demography/Seeds_per_capsule.csv")

#### Cleaning data ####

Seeds_per_capsule <- Seeds_per_capsule %>%  
  mutate(Site = case_when(Site == "LAV" ~ "Lav",
                            Site == "ULV" ~ "Ulv",
                            Site == "GUD" ~ "Gud",
                            Site == "SKJ" ~ "Skj")) %>%
  mutate(uniqueID = paste0(Species, "_", Site, "_",  Individual),
         capsuleID = paste0(uniqueID, "_", Capsule_ID)) %>%
  mutate(siteID = case_when(Site == "Lav" ~ "Lavisdalen",
                            Site == "Ulv" ~ "Ulvehaugen",
                            Site == "Gud" ~ "Gudmedalen",
                            Site == "Skj" ~ "Skjellingahaugen")) %>% 
  select(-Site, -Individual, -Capsule_ID) %>% 
  rename(date = Collection_date, species = Species, leaf_stalk_length = Leaf_stock_length_mm, shoot_height = Shoot_height_mm, leaf_length = Leaf_length_mm, leaf_width = Leaf_width_mm, number_of_leaves = Number_of_leaves, number_of_capsules = Number_of_capsules, number_of_seeds = Number_of_seeds, comments = Comment, number_of_flower_stems = "Number of flower stems") %>% 
  mutate(date = dmy(date))

Seeds_per_capsule <- Seeds_per_capsule %>% 
  pivot_longer(cols = c(leaf_stalk_length, shoot_height, leaf_length, leaf_width, number_of_leaves, number_of_capsules, number_of_flower_stems),
               names_to = "demographic_trait",
               values_to = "value") %>% 
  filter(!is.na(value)) %>% 
  select(species, siteID, date, uniqueID, comments, demographic_trait, value, capsuleID, number_of_seeds)

### Save data to upload as cleaned on OSF

#write.csv(Seeds_per_capsule, file = "data/cleaned_data/INCLINE_seeds_per_capsule.csv", row.names = FALSE)
