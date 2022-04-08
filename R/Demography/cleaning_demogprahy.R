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
#          file = "INCLINE_metadata.csv",
#          path = "data",
#          remote_path = "RawData")

#### Load data ####

Sib_pro <- read.csv2("data/Demography/Sib_pro_2018-2021.csv")
Ver_alp <- read.csv2("data/Demography/Ver_alp_2018-2021.csv")
INCLINE_metadata <- read_delim("data/INCLINE_metadata.csv", delim = ";")

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

#### Changing numbers to integers as we only measured in full mm ####

Sib_pro <- Sib_pro %>% 
  mutate(LSL = case_when(LSL == 0.5 ~ 1, #1 mm is the lowest value we record, changing 0.5 mm to 1 mm
                         TRUE ~ LSL)) %>% 
  mutate(LL = round(LL, digits = 0))

Ver_alp1 <- Ver_alp  # Need to fix things that were supposed to be mm but is cm,
  #mutate(SH = case_when(SH == 0.5 ~ 1, #1 mm is the lowest value we record, changing 0.5 mm to 1 mm
                         #TRUE ~ SH)) %>% 
  #mutate(LL = round(LL, digits = 0))


#### Imputing missing data for seedlings  ####

#Sibbaldia procumbens
seedling_constants <- Sib_pro %>% 
  filter(seedling == "yes") %>% 
  mutate(mean_LSL =mean(LSL, na.rm = TRUE),
         sd_LSL = sd(LSL, na.rm = TRUE),
         mean_NL = mean(NL, na.rm = TRUE),
         sd_NL = sd(NL, na.rm = TRUE),
         mean_LL = mean(LL, na.rm = TRUE), 
         sd_LL = sd(LL, na.rm = TRUE)) %>% 
  select(mean_LSL, sd_LSL, mean_NL, sd_NL, mean_LL, sd_LL) %>% 
  unique()

Sib_pro_seedling_fix <- Sib_pro %>% 
  filter(seedling == "yes") %>% 
  mutate(LSL = case_when(!is.na(LSL) ~ LSL,
                         is.na(LSL) ~ round(rnorm(length(LSL), mean = seedling_constants$mean_LSL), digits = 0))) %>% 
  mutate(NL = case_when(!is.na(NL) ~ NL,
                        (is.na(NL) & comment_registrator == "only cotyledon") ~ 0L,
                         (is.na(NL) & comment_registrator != "only cotyledon") ~ abs(as.integer(round(rnorm(length(NL), mean = seedling_constants$mean_NL), digits = 0))))) %>% #using absolute value because we get a few -1 values from the rnorm calculation 
  mutate(LL = case_when(!is.na(LL) ~ LL,
                        (is.na(LL) & comment_registrator == "only cotyledon") ~ 0,
                        (is.na(LL) & comment_registrator != "only cotyledon") ~ round(rnorm(length(LL), mean = seedling_constants$mean_LL), digits = 0))) 

# Veronica alpina
seedling_constants_VA <- Ver_alp %>% 
  filter(seedling == "yes") %>% 
  mutate(mean_SH =mean(SH, na.rm = TRUE),
         mean_NL = mean(NL, na.rm = TRUE),
         mean_LL = mean(LL, na.rm = TRUE),
         mean_WL = mean(WL, na.rm = TRUE)) %>% 
  select(mean_SH, mean_NL, mean_LL, mean_WL) %>% 
  unique()

Ver_alp_seedling_fix <- Ver_alp %>% 
  filter(seedling == "yes") %>% 
  mutate(SH = case_when(!is.na(SH) ~ SH,
                         is.na(SH) ~ round(rnorm(length(SH), mean = seedling_constants_VA$mean_SH), digits = 0))) %>% 
  mutate(NL = case_when(!is.na(NL) ~ NL,
                        (is.na(NL) & comment %in% c("only cotyledons, 5-7 same TP", "only cotyledons")) ~ 0L,
                        (is.na(NL) & !comment %in% c("only cotyledons, 5-7 same TP", "only cotyledons")) ~ as.integer(round(rnorm(length(NL), mean = seedling_constants_VA$mean_NL), digits = 0)))) %>% #using absolute value because we get a few -1 values from the rnorm calculation 
  mutate(LL = case_when(!is.na(LL) ~ LL,
                        (is.na(LL) & comment %in% c("only cotyledons, 5-7 same TP", "only cotyledons")) ~ 0,
                        (is.na(LL) & !comment %in% c("only cotyledons, 5-7 same TP", "only cotyledons")) ~ round(rnorm(length(LL), mean = seedling_constants_VA$mean_LL), digits = 0))) %>% 
  mutate(WL = case_when(!is.na(WL) ~ WL,
                        (is.na(WL) & comment %in% c("only cotyledons, 5-7 same TP", "only cotyledons")) ~ 0,
                        (is.na(WL) & !comment %in% c("only cotyledons, 5-7 same TP", "only cotyledons")) ~ round(rnorm(length(WL), mean = seedling_constants_VA$mean_WL), digits = 0)))

#### Replacing the empty rows of seedlings with computed numbers ####
Sib_pro <- Sib_pro %>%
  filter(seedling %in% c("no", NA)) %>% 
  bind_rows(Sib_pro_seedling_fix)

Ver_alp <- Ver_alp %>%
  filter(seedling %in% c("no", NA)) %>% 
  bind_rows(Ver_alp_seedling_fix)
