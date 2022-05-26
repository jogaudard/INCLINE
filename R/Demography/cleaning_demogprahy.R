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
#
# get_file(node = "zhk3m",
#          file = "VA_comments.csv",
#          path = "data/Demography",
#          remote_path = "RawData/Demography")
# 
# get_file(node = "zhk3m",
#          file = "SP_comments.csv",
#          path = "data/Demography",
#          remote_path = "RawData/Demography")


#### Load data ####

Sib_pro <- read.csv2("data/Demography/Sib_pro_2018-2021.csv")
Ver_alp <- read.csv2("data/Demography/Ver_alp_2018-2021.csv")
INCLINE_metadata <- read_delim("data/INCLINE_metadata.csv", delim = ";")
VA_comment_dict <- read.csv2("data/Demography/VA_comments.csv")
SP_comment_dict <- read.csv2("data/Demography/SP_comments.csv")

#### Cleaning variables names in dataset ####

# Cleaning metadata so I only have the treatment info and plotID to merge with demopgraphy data
INCLINE_metadata <- INCLINE_metadata %>% 
  select(plotID, OTC, treatment)

Sib_pro <- Sib_pro %>% 
  rename(siteID = Site, block = Block, plot = Plot, year = Year, date = Date, regitrator = Registrator, seedling = seedl) %>%  #Rename to lower capital, and correct naming convention for the INCLINE project
  mutate(siteID = if_else(siteID == "Ulv ", "Ulv", siteID)) %>% 
  mutate(plotID = paste0(siteID, "_", block, "_", plot)) %>% #creating unique plotID variable
  mutate(blockID = paste0(siteID, "_", block)) %>%  #creating unique blockID variable
  mutate(unique_IDS = paste0(plotID, "_", IDS)) %>% #creating unique individual ID
  left_join(INCLINE_metadata, by = "plotID") %>% #adding treatment info from INCLINE metadata file
  select(!Treat) %>% #removing treatment column from the original dataset
  select(!NC8) %>% 
  select(!NAC5) %>% #removing columns number of capsules 8 (NC8) and number of aborted capsules 5 (NAC5) because there are no entries in them
  #Removing this code for now - I don't think we want to replace NAs with NO, because those are individuals that are dead.
  # mutate(seedling = case_when(is.na(seedling) ~ "no", #replacing NAs in the seedling and juvenile column with NO, assuming we just forgot to enter something there.
  #                             seedling == "yes" ~ "yes",
  #                             seedling == "no" ~ "no")) %>% 
  # mutate(juvenile = case_when(is.na(juvenile) ~ "no",
  #                             juvenile == "yes" ~ "yes",
  #                             juvenile == "no" ~ "no")) %>% 
  mutate(MS = case_when(MS %in% c(1:500) ~ paste0(plotID, "_", MS))) %>% 
  mutate(seedling = case_when(seedling == "yes" & LL > 10 ~ "no",
                              seedling == "yes" & NL > 4 ~ "no",
                              seedling == "yes" ~ seedling,
                              seedling == "no" ~ seedling)) #Removing individuals from the seedling category if they have to large leaf length or to many leaves as we do not think these are actually seedlings.

Ver_alp <- Ver_alp %>% 
  rename(siteID = Site, block = Block, plot = Plot, year = Year, date = Date, registrator = Registrator, seedling = seedl) %>%  #Rename to lower capital, and correct naming convention for the INCLINE project
  mutate(siteID = if_else(siteID == "Lav ", "Lav", siteID)) %>% 
  mutate(plotID = paste0(siteID, "_", block, "_", plot)) %>% #creating unique plotID variable
  mutate(blockID = paste0(siteID, "_", block)) %>%  #creating unique blockID variable
  mutate(unique_IDS = paste0(plotID, "_", IDS)) %>% #creating unique individual ID
  left_join(INCLINE_metadata, by = "plotID") %>% #adding treatment info from INCLINE metadata file
  select(!Treat) %>% #removing treatment column from the original dataset
  #Removing this code for now - I don't think we want to replace NAs with NO, because those are individuals that are dead.
  # mutate(seedling = case_when(is.na(seedling) ~ "no", #replacing NAs in the seedling and juvenile column with NO, assuming we just forgot to enter something there.
  #                             seedling == "yes" ~ "yes",
  #                             seedling == "no" ~ "no")) %>% 
  # mutate(juvenile = case_when(is.na(juvenile) ~ "no",
  #                             juvenile == "yes" ~ "yes",
  #                             juvenile == "no" ~ "no")) %>% 
  mutate(MS = case_when(MS %in% c(1:100) ~ paste0(plotID, "_", MS))) %>% 
  mutate(seedling = case_when(seedling == "yes" & SH > 20 ~ "no",
                              seedling == "yes" & NL > 6 ~ "no",
                              seedling == "yes" ~ seedling,
                              seedling == "no" ~ seedling)) #Changing individuals from the seedling category if they have to large shoot height or to many leaves as we do not think these are actually seedlings.

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
  mutate(LSL = round(LSL + 0.01, digits = 0),
         LL = round(LL + 0.01, digits = 0)) %>% 
  mutate(NFS = if_else(NFS == "?", NA_integer_, as.integer(NFS)),
         LL = as.integer(LL)) 

Ver_alp <- Ver_alp %>%  
  mutate(SH = round(SH + 0.01, digits = 0),
         LL = round(LL + 0.01, digits = 0),
         WL = round(WL + 0.01, digits = 0)) %>% 
  mutate(SH = as.integer(SH),
         LL = as.integer(LL),
         WL = as.integer(WL))


#### Imputing missing data for seedlings  ####

#Sibbaldia procumbens
seedling_constants <- Sib_pro %>% 
  filter(seedling == "yes") %>% 
  mutate(mean_LSL =mean(LSL, na.rm = TRUE),
         mean_NL = mean(NL, na.rm = TRUE),
         mean_LL = mean(LL, na.rm = TRUE)) %>% 
  select(mean_LSL, mean_NL, mean_LL,) %>% 
  unique()

Sib_pro_seedling_fix <- Sib_pro %>% 
  filter(seedling == "yes") %>% 
  mutate(LSL = case_when(!is.na(LSL) ~ LSL,
                         is.na(LSL) ~ round(rnorm(length(LSL), mean = seedling_constants$mean_LSL), digits = 0))) %>% 
  mutate(NL = case_when((comment_registrator == "only cotyledons") ~ 0L, #Every seedling with only cotyledons should not have any leaf measurements (this removes measurements for around 5 individuals)
                        !is.na(NL) ~ NL,
                         is.na(NL) ~ abs(as.integer(round(rnorm(length(NL), mean = seedling_constants$mean_NL), digits = 0))))) %>% #using absolute value because we get a few -1 values from the rnorm calculation 
  mutate(NL = case_when((NL == 0L & comment_registrator != "only cotyledons") ~ 1L, #Changing the seedlings with 0 leaves to 1 leaf because it is not possible except for seedlings with only cotyledons to have 0 leaves.
                        NL>= 0 ~ NL)) %>% 
  mutate(LL = case_when((comment_registrator == "only cotyledons") ~ 0L,
                        !is.na(LL) ~ LL,
                        (is.na(LL) & comment_registrator != "only cotyledons") ~ as.integer(round(rnorm(length(LL), mean = seedling_constants$mean_LL), digits = 0)))) 

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
                         is.na(SH) ~ as.integer(round(rnorm(length(SH), mean = seedling_constants_VA$mean_SH), digits = 0)))) %>% 
  mutate(NL = case_when(!is.na(NL) ~ NL,
                        (is.na(NL) & comment %in% c("only cotyledons, 5-7 same TP", "only cotyledons")) ~ 0L,
                        (is.na(NL) & !comment %in% c("only cotyledons, 5-7 same TP", "only cotyledons")) ~ as.integer(round(rnorm(length(NL), mean = seedling_constants_VA$mean_NL), digits = 0)))) %>% 
  mutate(LL = case_when(!is.na(LL) ~ LL,
                        (is.na(LL) & comment %in% c("only cotyledons, 5-7 same TP", "only cotyledons")) ~ 0L,
                        (is.na(LL) & !comment %in% c("only cotyledons, 5-7 same TP", "only cotyledons")) ~ as.integer(round(rnorm(length(LL), mean = seedling_constants_VA$mean_LL), digits = 0)))) %>% 
  mutate(WL = case_when(!is.na(WL) ~ WL,
                        (is.na(WL) & comment %in% c("only cotyledons, 5-7 same TP", "only cotyledons")) ~ 0L,
                        (is.na(WL) & !comment %in% c("only cotyledons, 5-7 same TP", "only cotyledons")) ~ as.integer(round(rnorm(length(WL), mean = seedling_constants_VA$mean_WL), digits = 0))))

#### Replacing the empty rows of seedlings with computed numbers ####
Sib_pro <- Sib_pro %>%
  filter(seedling %in% c("no", NA)) %>% 
  bind_rows(Sib_pro_seedling_fix)

Ver_alp <- Ver_alp %>%
  filter(seedling %in% c("no", NA)) %>% 
  bind_rows(Ver_alp_seedling_fix)

#### Removing individuals from data set based of comments ####
#Individuals that might be other species, or that died during transplant or data collection

Ver_alp <- Ver_alp %>% 
  left_join(VA_comment_dict, by = "comment") %>% 
  group_by(unique_IDS) %>% 
  fill(flag, .direction = "downup") %>% 
  filter(is.na(flag)) %>% 
  select(-flag)

Sib_pro <- Sib_pro %>% 
  left_join(SP_comment_dict, by = "comment_registrator") %>%
  group_by(unique_IDS) %>% 
  fill(flag, .direction = "downup") %>% 
  filter(is.na(flag)) %>% 
  select(-flag)


#### Remove unneeded datasets ####
rm(seedling_constants)
rm(seedling_constants_VA)
rm(Sib_pro_seedling_fix)
rm(Ver_alp_seedling_fix)
rm(SP_comment_dict)
rm(VA_comment_dict)

#### Making data set ready for OSF storage - changing variable names and pivot longer ####

INCLINE_demography_Sib_pro <- Sib_pro %>% 
  select(siteID, blockID, plotID, year, date, regitrator, OTC, treatment, unique_IDS, IDG, MS, X, Y, LSL, NL, LL, NFS, NFL, NB, NC, NAC, seedling, juvenile) %>% 
  rename(registrator = regitrator, unique_ID = unique_IDS, genet_ID = IDG, mother_shoot = MS, leaf_stalk_length = LSL, number_of_leaves = NL, leaf_length = LL, number_of_flower_stems = NFS, number_of_flowers = NFL, number_of_buds = NB, number_of_capsules = NC, number_of_aborted_capsules = NAC) %>% 
  pivot_longer(cols = c(leaf_stalk_length, number_of_leaves, leaf_length, number_of_flower_stems, number_of_flowers, number_of_buds, number_of_capsules, number_of_aborted_capsules), names_to = "demographic_trait", values_to = "demographic_value") %>% 
  filter(!is.na(demographic_value))

INCLINE_demography_Ver_alp <- Ver_alp %>% 
  select(siteID, blockID, plotID, year, date, registrator, OTC, treatment, unique_IDS, IDG, MS, X, Y, SH, NL, LL, WL, NFL, NB, NC, NAC, seedling, juvenile,) %>% 
  rename(genet_ID = IDG, mother_shoot = MS, shoot_height = SH, number_of_leaves = NL, leaf_length = LL, leaf_width = WL, number_of_flowers = NFL, number_of_buds = NB, number_of_capsules = NC, number_of_aborted_capsules = NAC) %>% 
  pivot_longer(cols = c(shoot_height, number_of_leaves, leaf_length, leaf_width, number_of_flowers, number_of_buds, number_of_capsules, number_of_aborted_capsules), names_to = "demographic_trait", values_to = "demographic_value") %>% 
  filter(!is.na(demographic_value))

#write.csv(INCLINE_demography_Sib_pro, file = "data/cleaned_data/INCLINE_demography_Sib_pro.csv", row.names = FALSE)
#write.csv(INCLINE_demography_Ver_alp, file = "data/cleaned_data/INCLINE_demography_Ver_alp.csv", row.names = FALSE)
