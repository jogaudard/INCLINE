###########################################################
### Script for cleaning raw demography data for INCLINE ###
###########################################################

#### Libraries ####
library(tidyverse)
library(dataDownloader)
library(osfr)
library(lubridate)
library(conflicted)
library(here)

#### Select preferences for conflicts ####

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

#### Downloading data from OSF ####

#osf_auth(token = "get from my document every time you run the code") 

# get_file(node = "zhk3m",
#          file = "Sib_pro_2018-2021.csv",
#          path = "data/Demography",
#          remote_path = "RawData/Demography")
# 
# get_file(node = "zhk3m",
#          file = "Ver_alp_2018-2021.csv",
#          path = "data/Demography",
#          remote_path = "RawData/Demography")
# 
# get_file(node = "zhk3m",
#          file = "INCLINE_metadata_LoggerDates.csv",
#          path = "data",
#          remote_path = "RawData")

#### Load data ####

Sib_pro <- read_csv2("data/Demography/Sib_pro_2018-2021.csv")
Ver_alp <- read_csv2("data/Demography/Ver_alp_2018-2021.csv")
INCLINE_metadata <- read_delim("data/INCLINE_metadata_LoggerDates.csv", delim = ";")

#### Cleaning variables names in dataset ####

# Cleaning metadata so I only have the treatment info and plotID to merge with demopgraphy data
INCLINE_metadata <- INCLINE_metadata %>% 
  select(plotID, OTC, treatment)

Sib_pro <- Sib_pro %>% 
  rename(siteID = Site, block = Block, plot = Plot, year = Year, date = Date, regitrator = Registrator, seedling = seedl) %>%  #Rename to lower capital, and correct naming convention for the INCLINE project
  mutate(plotID = paste0(siteID, "_", block, "_", plot)) %>% #creating unique plotID variable
  mutate(blockID = paste0(siteID, "_", block)) %>%  #creating unique blockID variable
  mutate(unique_IDS = paste0(plotID, "_", IDS)) %>% #creating unique individual ID
  left_join(INCLINE_metadata, by = "plotID") %>% #adding treatment info from INCLINE metadata file
  select(!Treat) %>% #removing treatment column from the original dataset
  select(!NC8) %>% 
  select(!NAC5) %>% #removing columns number of capsules 8 (NC8) and number of aborted capsules 5 (NAC5) because there are no entries in them
  filter(!(is.na(LSL) & is.na(NL) & is.na(LL) & is.na(NFS))) %>%  #remove any individuals that are dead in that year (they might still be in the dataset because we made a comment that the species is gone)
  mutate(seedling = case_when(is.na(seedling) ~ "no", #replacing NAs in the seedling and juvenile column with NO, assuming we just forgot to enter something there.
                              seedling == "yes" ~ "yes",
                              seedling == "no" ~ "no")) %>% 
  mutate(juvenile = case_when(is.na(juvenile) ~ "no",
                              juvenile == "yes" ~ "yes",
                              juvenile == "no" ~ "no")) %>% 
  mutate(MS = case_when(MS %in% c(1:100) ~ paste0(plotID, "_", MS))) %>% 
  mutate(seedling = case_when(seedling == "yes" & LL > 10 ~ "no",
                              seedling == "yes" & NL > 4 ~ "no",
                              seedling == "yes" ~ seedling,
                              seedling == "no" ~ seedling)) #Removing individuals from the seedling category if they have to large leaf length or to many leaves as we do not think these are actually seedlings.

Ver_alp <- Ver_alp %>% 
  rename(siteID = Site, block = Block, plot = Plot, year = Year, date = Date, registrator = Registrator, seedling = seedl) %>%  #Rename to lower capital, and correct naming convention for the INCLINE project
  mutate(plotID = paste0(siteID, "_", block, "_", plot)) %>% #creating unique plotID variable
  mutate(blockID = paste0(siteID, "_", block)) %>%  #creating unique blockID variable
  mutate(unique_IDS = paste0(plotID, "_", IDS)) %>% #creating unique individual ID
  left_join(INCLINE_metadata, by = "plotID") %>% #adding treatment info from INCLINE metadata file
  select(!Treat) %>% #removing treatment column from the original dataset
  filter(!(is.na(SH) & is.na(NL) & is.na(LL) & is.na(WL))) %>% #remove any individuals that are dead in that year (they might still be in the dataset because we made a comment that the species is gone)
  mutate(seedling = case_when(is.na(seedling) ~ "no", #replacing NAs in the seedling and juvenile column with NO, assuming we just forgot to enter something there.
                              seedling == "yes" ~ "yes",
                              seedling == "no" ~ "no")) %>% 
  mutate(juvenile = case_when(is.na(juvenile) ~ "no",
                              juvenile == "yes" ~ "yes",
                              juvenile == "no" ~ "no")) %>% 
  mutate(MS = case_when(MS %in% c(1:100) ~ paste0(plotID, "_", MS))) %>% 
  mutate(seedling = case_when(seedling == "yes" & SH > 20 ~ "no",
                              seedling == "yes" & NL > 6 ~ "no",
                              seedling == "yes" ~ seedling,
                              seedling == "no" ~ seedling)) #Removing individuals from the seedling category if they have to large shoot height or to many leaves as we do not think these are actually seedlings.

#### Changing variable types ####

Sib_pro <- Sib_pro %>% 
  mutate(date = dmy(date), #changing the date to actual date format
         siteID = as.factor(siteID), #changing siteID, OTC and treatment to factor
         OTC = as.factor(OTC),
         treatment = as.factor(treatment))

Ver_alp <- Ver_alp %>% 
  mutate(date = dmy(date), #changing the date to actual date format
         siteID = as.factor(siteID), #changing siteID, OTC and treatment to factor
         OTC = as.factor(OTC),
         treatment = as.factor(treatment))

#### Making data set ready for OSF storage - changing variable names and pivot longer ####

INCLINE_demography_Sib_pro <- Sib_pro %>% 
  select(siteID, blockID, plotID, year, date, regitrator, OTC, treatment, unique_IDS, IDG, MS, X, Y, LSL, NL, LL, NFS, NFL, NB, NC, NAC, seedling, juvenile, comment_registrator, comment_transcription) %>% 
  rename(registrator = regitrator, unique_ID = unique_IDS, genet_ID = IDG, mother_shoot = MS, leaf_stalk_length = LSL, number_of_leaves = NL, leaf_length = LL, number_of_flower_stems = NFS, number_of_flowers = NFL, number_of_buds = NB, number_of_capsules = NC, number_of_aborted_capsules = NAC, comment_field = comment_registrator) %>% 
  pivot_longer(cols = c(leaf_stalk_length, number_of_leaves, leaf_length, number_of_flower_stems, number_of_flowers, number_of_buds, number_of_capsules, number_of_aborted_capsules), names_to = "demographic_trait", values_to = "demographic_value")

INCLINE_demography_Ver_alp <- Ver_alp %>% 
  select(siteID, blockID, plotID, year, date, registrator, OTC, treatment, unique_IDS, IDG, MS, X, Y, SH, NL, LL, WL, NFL, NB, NC, NAC, seedling, juvenile, comment, comment_transcription) %>% 
  rename(genet_ID = IDG, mother_shoot = MS, shoot_height = SH, number_of_leaves = NL, leaf_length = LL, leaf_width = WL, number_of_flowers = NFL, number_of_buds = NB, number_of_capsules = NC, number_of_aborted_capsules = NAC, comment_field = comment) %>% 
  pivot_longer(cols = c(shoot_height, number_of_leaves, leaf_length, leaf_width, number_of_flowers, number_of_buds, number_of_capsules, number_of_aborted_capsules), names_to = "demographic_trait", values_to = "demographic_value")

#write.csv(INCLINE_demography_Sib_pro, file = "data/cleaned_data/INCLINE_demography_Sib_pro.csv", row.names = FALSE)
#write.csv(INCLINE_demography_Ver_alp, file = "data/cleaned_data/INCLINE_demography_Ver_alp.csv", row.names = FALSE)
