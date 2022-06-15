##########################################################
### Script for cleaning raw seed bank data for INCLINE ###
##########################################################

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
#           file = "Seed_bank_survival.csv",
#           path = "data/Demography",
#           remote_path = "RawData/Demography")

#### Load data ####

seed_bank <- read_csv2("data/Demography/Seed_bank_survival.csv") 

#### Cleaning seed bank data ####

seed_bank <- seed_bank %>% 
  rename(missing = "missing/dissentegrated") %>% 
  mutate(comment_survival = case_when(missing == "Yes" ~ "Dissentigrated in the field",
                                      germinated == "Yes" ~ "Seedling germinated under germination trail",
                                      embryo_in_seed == "Yes" ~ "Viable embryo in cuttest after failed germination trail",
                                      dead == "Yes" ~ "Dead embryo in cuttest after failed germination trail")) %>% 
  mutate(seeds_dead_in_soil_bank = case_when(missing == "Yes" ~ 1,
                                             missing == "No" ~ 0),
         seeds_germinate = case_when(germinated == "Yes" ~ 1,
                                     TRUE ~ 0),
         seeds_alive_not_germ = case_when(embryo_in_seed == "Yes" ~ 1,
                                          TRUE ~ 0),
         seeds_dead_later = case_when(dead == "Yes" ~1,
                                      TRUE ~ 0)) %>%
  mutate(survival_in_seedbank = seeds_germinate + seeds_alive_not_germ) %>% 
  select(plotID, siteID, date_in, date_out, species, seed_number, comment, survival_in_seedbank, warming, comment_survival) %>% 
  mutate(siteID = case_when(siteID == "LAV" ~ "Lavisdalen",
                            siteID == "ULV" ~ "Ulvehaugen",
                            siteID == "GUD" ~ "Gudmedalen",
                            siteID == "SKJ" ~ "Skjellingahaugen")) %>% 
  mutate(site = substr(siteID, 1, 3),
         blockID = substr(plotID, 4, 5),
         blockID = paste0(site, blockID),
         plot = substr(plotID, 6, 9),
         plotID = paste0(blockID, plot)) %>% 
  select(-plot, -site) %>% 
  mutate(uniqueID = paste0(species, "_", plotID, "_", seed_number)) %>% 
  select(siteID, blockID, plotID, warming, species, uniqueID, date_in, date_out, survival_in_seedbank, comment, comment_survival) %>% 
  rename(comments = comment) %>% 
  mutate(date_out = as.character(date_out)) %>%
  mutate(date_in = dmy(date_in)) %>% 
  mutate(date_out = dmy(date_out))

#### Save the cleaned file for OSF ####

#write.csv(seed_bank, file = "data/cleaned_data/INCLINE_seedbank_survival.csv", row.names = FALSE)
  