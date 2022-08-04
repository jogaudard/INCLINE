#### Creating values for INCLINE data documentation read me ####

# libraries ---------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(dataDownloader)

# data import -------------------------------------------------------------
source("data_doc/INCLINE_download_read_cleandata.R")

ndvi <- ndvi %>%
   # mutate(a = substr(siteID, 1, 3),
   #        b = substr(plotID, 4, 7)) %>%
   # mutate(plotID = paste0(a, b))  %>%
   # select(-a, -b) %>%
  left_join((INCLINE_metadata %>% select(plotID, OTC, treatment)), by = "plotID")

# creating values for the data documentation ------------------------------

# dataset i: Biomass removal ----------------------------------------------

total_biomass_removed <- round(sum(biomass_removal$value, na.rm = TRUE), digits = 0)
graminoid_total <-  round(sum((biomass_removal %>% filter(functional_group == "Graminoid") %>% select(value)), na.rm = TRUE), digits = 0)
bryophyte_total <- round(sum((biomass_removal %>% filter(functional_group == "Bryophyte") %>% select(value)), na.rm = TRUE), digits = 0)
forb_total <- round(sum((biomass_removal %>% filter(functional_group == "Forb") %>% select(value)), na.rm = TRUE), digits = 0)
litter_total <- round(sum((biomass_removal %>% filter(functional_group == "Litter") %>% select(value)), na.rm = TRUE), digits = 0)
biomass_2019 <- round(sum((biomass_removal %>% filter(year == 2019) %>% select(value)), na.rm = TRUE), digits = 0)
biomass_2020 <- round(sum((biomass_removal %>% filter(year == 2020) %>% select(value)), na.rm = TRUE), digits = 0)
biomass_2021 <- round(sum((biomass_removal %>% filter(year == 2021) %>% select(value)), na.rm = TRUE), digits = 0)

biomass_r_plots <- length(unique(biomass_removal$plotID))

# dataset ii: Vascular plant community composition -------------------------

# dataset iii: Population dynamics of alpine focal species -----------------

SP_n <- length(unique(demography_Sib_pro$uniqueID))
VA_n <- length(unique(demography_Ver_alp$uniqueID))
SP_n_leaves <- demography_Sib_pro %>% filter(demographic_trait == "number_of_leaves") %>% mutate(mean = mean(demographic_value)) %>% select(mean) %>% unique() %>% as.numeric() %>% round(digits = 0)
SP_LSL <- demography_Sib_pro %>% filter(demographic_trait == "leaf_stalk_length") %>% mutate(mean = mean(demographic_value)) %>% select(mean) %>% unique() %>% as.numeric() %>% round(digits = 0)
SP_leaf_length <- demography_Sib_pro %>% filter(demographic_trait == "leaf_length") %>% mutate(mean = mean(demographic_value)) %>% select(mean) %>% unique() %>% as.numeric() %>% round(digits = 0)
SP_n_flowers <- demography_Sib_pro %>% filter(demographic_trait %in% c("number_of_flowers", "number_of_buds", "number of capsules")) %>% group_by(uniqueID) %>% mutate(total_reproductive = sum(demographic_value)) %>% ungroup() %>%  mutate(mean = mean(total_reproductive)) %>% select(mean) %>% unique() %>% as.numeric() %>% round(digits = 0)

VA_SH <- demography_Ver_alp %>% filter(demographic_trait == "shoot_height") %>% mutate(mean = mean(demographic_value)) %>% select(mean) %>% unique() %>% as.numeric() %>% round(digits = 0)
VA_n_leaves <- demography_Ver_alp %>% filter(demographic_trait == "number_of_leaves") %>% mutate(mean = mean(demographic_value)) %>% select(mean) %>% unique() %>% as.numeric() %>% round(digits = 0)
VA_leaf_length <- demography_Ver_alp %>% filter(demographic_trait == "leaf_length") %>% mutate(mean = mean(demographic_value)) %>% select(mean) %>% unique() %>% as.numeric() %>% round(digits = 0)
VA_leaf_width <- demography_Ver_alp %>% filter(demographic_trait == "leaf_width") %>% mutate(mean = mean(demographic_value)) %>% select(mean) %>% unique() %>% as.numeric() %>% round(digits = 0)
VA_n_flowers <- demography_Ver_alp %>% filter(demographic_trait %in% c("number_of_flowers", "number_of_buds", "number of capsules")) %>% group_by(uniqueID) %>% mutate(total_reproductive = sum(demographic_value)) %>% ungroup() %>%  mutate(mean = mean(total_reproductive)) %>% select(mean) %>% unique() %>% as.numeric() %>% round(digits = 0)

demo_SP_plots <- length(unique(demography_Sib_pro$plotID))
demo_VA_plots <- length(unique(demography_Ver_alp$plotID))

# dataset iv: Species-level biomass allocation ---------------------------

sp_biomass_n_SP <-species_level_biomass_allocation %>% select(species, uniqueID) %>% unique() %>% count(species) %>% filter(species == "Sib_pro") %>% select(n) %>% as.numeric()
sp_biomass_n_VA <- species_level_biomass_allocation %>% select(species, uniqueID) %>% unique() %>% count(species) %>% filter(species == "Ver_alp") %>% select(n) %>% as.numeric()
sp_biomass_n_VO <- species_level_biomass_allocation %>% select(species, uniqueID) %>% unique() %>% count(species) %>% filter(species == "Ver_off") %>% select(n) %>% as.numeric()
sp_biomass_n_VC <- species_level_biomass_allocation %>% select(species, uniqueID) %>% unique() %>% count(species) %>% filter(species == "Vio_can") %>% select(n) %>% as.numeric()
sp_biomass_n_SucP <- species_level_biomass_allocation %>% select(species, uniqueID) %>% unique() %>% count(species) %>% filter(species == "Suc_pra") %>% select(n) %>% as.numeric()
sp_biomass_n_HM <- species_level_biomass_allocation %>% select(species, uniqueID) %>% unique() %>% count(species) %>% filter(species == "Hyp_mac") %>% select(n) %>% as.numeric()

mean_abg_biomass_SP <- species_level_biomass_allocation %>% filter(trait == "aboveground_biomass") %>% select(species, value) %>% group_by(species) %>%  mutate(mean = round(mean(value, na.rm = TRUE), digits = 3)) %>% ungroup() %>%  select(species, mean) %>% unique() %>% filter(species == "Sib_pro") %>% select(mean) %>% as.numeric()
sd_abg_biomass_SP <- species_level_biomass_allocation %>% filter(trait == "aboveground_biomass") %>% select(species, value) %>% group_by(species) %>%  mutate(sd = round(sd(value, na.rm = TRUE), digits = 3)) %>% ungroup() %>%  select(species, sd) %>% unique() %>% filter(species == "Sib_pro") %>% select(sd) %>% as.numeric()

mean_abg_biomass_VA <- species_level_biomass_allocation %>% filter(trait == "aboveground_biomass") %>% select(species, value) %>% group_by(species) %>%  mutate(mean = round(mean(value, na.rm = TRUE), digits = 3)) %>% ungroup() %>%  select(species, mean) %>% unique() %>% filter(species == "Ver_alp") %>% select(mean) %>% as.numeric()
sd_abg_biomass_VA <- species_level_biomass_allocation %>% filter(trait == "aboveground_biomass") %>% select(species, value) %>% group_by(species) %>%  mutate(sd = round(sd(value, na.rm = TRUE), digits = 3)) %>% ungroup() %>%  select(species, sd) %>% unique() %>% filter(species == "Ver_alp") %>% select(sd) %>% as.numeric()

mean_abg_biomass_VO <- species_level_biomass_allocation %>% filter(trait == "aboveground_biomass") %>% select(species, value) %>% group_by(species) %>%  mutate(mean = round(mean(value, na.rm = TRUE), digits = 2)) %>% ungroup() %>%  select(species, mean) %>% unique() %>% filter(species == "Ver_off") %>% select(mean) %>% as.numeric()
sd_abg_biomass_VO <- species_level_biomass_allocation %>% filter(trait == "aboveground_biomass") %>% select(species, value) %>% group_by(species) %>%  mutate(sd = round(sd(value, na.rm = TRUE), digits = 3)) %>% ungroup() %>%  select(species, sd) %>% unique() %>% filter(species == "Ver_off") %>% select(sd) %>% as.numeric()

mean_abg_biomass_VC <- species_level_biomass_allocation %>% filter(trait == "aboveground_biomass") %>% select(species, value) %>% group_by(species) %>%  mutate(mean = round(mean(value, na.rm = TRUE), digits = 3)) %>% ungroup() %>%  select(species, mean) %>% unique() %>% filter(species == "Vio_can") %>% select(mean) %>% as.numeric()
sd_abg_biomass_VC <- species_level_biomass_allocation %>% filter(trait == "aboveground_biomass") %>% select(species, value) %>% group_by(species) %>%  mutate(sd = round(sd(value, na.rm = TRUE), digits = 3)) %>% ungroup() %>%  select(species, sd) %>% unique() %>% filter(species == "Vio_can") %>% select(sd) %>% as.numeric()

mean_abg_biomass_SucP <- species_level_biomass_allocation %>% filter(trait == "aboveground_biomass") %>% select(species, value) %>% group_by(species) %>%  mutate(mean = round(mean(value, na.rm = TRUE), digits = 2)) %>% ungroup() %>%  select(species, mean) %>% unique() %>% filter(species == "Suc_pra") %>% select(mean) %>% as.numeric()
sd_abg_biomass_SucP <- species_level_biomass_allocation %>% filter(trait == "aboveground_biomass") %>% select(species, value) %>% group_by(species) %>%  mutate(sd = round(sd(value, na.rm = TRUE), digits = 3)) %>% ungroup() %>%  select(species, sd) %>% unique() %>% filter(species == "Suc_pra") %>% select(sd) %>% as.numeric()

mean_abg_biomass_HM <- species_level_biomass_allocation %>% filter(trait == "aboveground_biomass") %>% select(species, value) %>% group_by(species) %>%  mutate(mean = round(mean(value, na.rm = TRUE), digits = 2)) %>% ungroup() %>%  select(species, mean) %>% unique() %>% filter(species == "Hyp_mac") %>% select(mean) %>% as.numeric()
sd_abg_biomass_HM <- species_level_biomass_allocation %>% filter(trait == "aboveground_biomass") %>% select(species, value) %>% group_by(species) %>%  mutate(sd = round(sd(value, na.rm = TRUE), digits = 3)) %>% ungroup() %>%  select(species, sd) %>% unique() %>% filter(species == "Hyp_mac") %>% select(sd) %>% as.numeric()

mean_bg_biomass_SP <- species_level_biomass_allocation %>% filter(trait == "belowground_biomass") %>% select(species, value) %>% group_by(species) %>%  mutate(mean = round(mean(value, na.rm = TRUE), digits = 3)) %>% ungroup() %>%  select(species, mean) %>% unique() %>% filter(species == "Sib_pro") %>% select(mean) %>% as.numeric()
sd_bg_biomass_SP <- species_level_biomass_allocation %>% filter(trait == "belowground_biomass") %>% select(species, value) %>% group_by(species) %>%  mutate(sd = round(sd(value, na.rm = TRUE), digits = 3)) %>% ungroup() %>%  select(species, sd) %>% unique() %>% filter(species == "Sib_pro") %>% select(sd) %>% as.numeric()

mean_bg_biomass_VA <- species_level_biomass_allocation %>% filter(trait == "belowground_biomass") %>% select(species, value) %>% group_by(species) %>%  mutate(mean = round(mean(value, na.rm = TRUE), digits = 3)) %>% ungroup() %>%  select(species, mean) %>% unique() %>% filter(species == "Ver_alp") %>% select(mean) %>% as.numeric()
sd_bg_biomass_VA <- species_level_biomass_allocation %>% filter(trait == "belowground_biomass") %>% select(species, value) %>% group_by(species) %>%  mutate(sd = round(sd(value, na.rm = TRUE), digits = 3)) %>% ungroup() %>%  select(species, sd) %>% unique() %>% filter(species == "Ver_alp") %>% select(sd) %>% as.numeric()

mean_bg_biomass_VO <- species_level_biomass_allocation %>% filter(trait == "belowground_biomass") %>% select(species, value) %>% group_by(species) %>%  mutate(mean = round(mean(value, na.rm = TRUE), digits = 2)) %>% ungroup() %>%  select(species, mean) %>% unique() %>% filter(species == "Ver_off") %>% select(mean) %>% as.numeric()
sd_bg_biomass_VO <- species_level_biomass_allocation %>% filter(trait == "belowground_biomass") %>% select(species, value) %>% group_by(species) %>%  mutate(sd = round(sd(value, na.rm = TRUE), digits = 3)) %>% ungroup() %>%  select(species, sd) %>% unique() %>% filter(species == "Ver_off") %>% select(sd) %>% as.numeric()

mean_bg_biomass_VC <- species_level_biomass_allocation %>% filter(trait == "belowground_biomass") %>% select(species, value) %>% group_by(species) %>%  mutate(mean = round(mean(value, na.rm = TRUE), digits = 3)) %>% ungroup() %>%  select(species, mean) %>% unique() %>% filter(species == "Vio_can") %>% select(mean) %>% as.numeric()
sd_bg_biomass_VC <- species_level_biomass_allocation %>% filter(trait == "belowground_biomass") %>% select(species, value) %>% group_by(species) %>%  mutate(sd = round(sd(value, na.rm = TRUE), digits = 3)) %>% ungroup() %>%  select(species, sd) %>% unique() %>% filter(species == "Vio_can") %>% select(sd) %>% as.numeric()

mean_bg_biomass_SucP <- species_level_biomass_allocation %>% filter(trait == "belowground_biomass") %>% select(species, value) %>% group_by(species) %>%  mutate(mean = round(mean(value, na.rm = TRUE), digits = 2)) %>% ungroup() %>%  select(species, mean) %>% unique() %>% filter(species == "Suc_pra") %>% select(mean) %>% as.numeric()
sd_bg_biomass_SucP <- species_level_biomass_allocation %>% filter(trait == "belowground_biomass") %>% select(species, value) %>% group_by(species) %>%  mutate(sd = round(sd(value, na.rm = TRUE), digits = 3)) %>% ungroup() %>%  select(species, sd) %>% unique() %>% filter(species == "Suc_pra") %>% select(sd) %>% as.numeric()

mean_bg_biomass_HM <- species_level_biomass_allocation %>% filter(trait == "belowground_biomass") %>% select(species, value) %>% group_by(species) %>%  mutate(mean = round(mean(value, na.rm = TRUE), digits = 2)) %>% ungroup() %>%  select(species, mean) %>% unique() %>% filter(species == "Hyp_mac") %>% select(mean) %>% as.numeric()
sd_bg_biomass_HM <- species_level_biomass_allocation %>% filter(trait == "belowground_biomass") %>% select(species, value) %>% group_by(species) %>%  mutate(sd = round(sd(value, na.rm = TRUE), digits = 3)) %>% ungroup() %>%  select(species, sd) %>% unique() %>% filter(species == "Hyp_mac") %>% select(sd) %>% as.numeric()

# dataset v: Seeds per capsule for the alpine focal species --------------

seeds_per_capsule_SP <-  seeds_per_capsule %>% mutate(species = case_when(species == "SIb_pro" ~ "Sib_pro", species == "Sib_pro" ~ "Sib_pro", species == "Ver_alp" ~ "Ver_alp")) %>% filter(species == "Sib_pro") %>% select(capsuleID, number_of_seeds) %>% unique() %>% mutate(mean = mean(number_of_seeds, na.rm = TRUE)) %>% select(mean) %>% unique() %>% mutate(mean = round(mean, digits = 2)) %>%  as.numeric()

seeds_per_capsule_VA <-  seeds_per_capsule %>% filter(species == "Ver_alp") %>% select(capsuleID, number_of_seeds) %>% unique() %>% mutate(mean = mean(number_of_seeds, na.rm = TRUE)) %>% select(mean) %>% unique() %>% mutate(mean = round(mean, digits = 2)) %>%  as.numeric()

# dataset vi: Germination of alpine focal species -------------------------

n_seedlings_SP <- seedling_data_alpine %>% select(species, uniqueID) %>% unique() %>% group_by(species) %>% count() %>% ungroup() %>% filter(species == "Sib_pro") %>%  select(n) %>% as.numeric()
n_seedlings_VA <- seedling_data_alpine %>% select(species, uniqueID) %>% unique() %>% group_by(species) %>% count() %>% ungroup() %>% filter(species == "Ver_alp") %>%  select(n) %>% as.numeric()

n_sampled_seedlings_SP <- seedling_traits_alpine %>% filter(species == "Sib_pro") %>% select(uniqueID) %>% unique() %>% count() %>% as.numeric()
n_sampled_seedlings_VA <- seedling_traits_alpine %>% filter(species == "Ver_alp") %>% select(uniqueID) %>% unique() %>% count() %>% as.numeric()

mean_abg_biomass_seedling_SP <- seedling_traits_alpine %>% filter(trait == "weight_dry_whole") %>% filter(species == "Sib_pro") %>%  select(values) %>% mutate(mean = mean(values, na.rm = TRUE)*1000) %>% mutate(mean = round(mean, digits = 3)) %>%  select(mean) %>% unique() %>% as.numeric()
sd_abg_biomass_seedling_SP <- seedling_traits_alpine %>% filter(trait == "weight_dry_whole") %>% filter(species == "Sib_pro") %>%  select(values) %>% mutate(sd = sd(values, na.rm = TRUE)*1000) %>% mutate(sd = round(sd, digits = 3)) %>%  select(sd) %>% unique() %>% as.numeric()
mean_abg_biomass_seedling_VA <- seedling_traits_alpine %>% filter(trait == "weight_dry_whole") %>% filter(species == "Ver_alp") %>%  select(values) %>% mutate(mean = mean(values, na.rm = TRUE)*1000) %>% mutate(mean = round(mean, digits = 3)) %>%  select(mean) %>% unique() %>% as.numeric()
sd_abg_biomass_seedling_VA <- seedling_traits_alpine %>% filter(trait == "weight_dry_whole") %>% filter(species == "Ver_alp") %>%  select(values) %>% mutate(sd = sd(values, na.rm = TRUE)*1000) %>% mutate(sd = round(sd, digits = 3)) %>%  select(sd) %>% unique() %>% as.numeric()

germ_alpine_plots <- seedling_data_alpine %>% filter(experiment == "E") %>% select(plotID) %>% unique() %>% mutate(n = length(plotID)) %>% select(n) %>% unique() %>% as.numeric()

# dataset vii: Germination of sub-alpine focal species ---------------------

n_seedlings_Ver_off <- seedling_data_subalpine %>% select(species, uniqueID, total_seedlings_emerged) %>% unique() %>% filter(species == "Ver_off") %>% mutate(sum = sum(total_seedlings_emerged, na.rm = TRUE)) %>%  select(sum) %>% unique() %>% as.numeric()
n_seedlings_Vio_can <- seedling_data_subalpine %>% select(species, uniqueID, total_seedlings_emerged) %>% unique() %>% filter(species == "Vio_can") %>% mutate(sum = sum(total_seedlings_emerged, na.rm = TRUE)) %>%  select(sum) %>% unique() %>% as.numeric()
n_seedlings_Car_pil <- seedling_data_subalpine %>% select(species, uniqueID, total_seedlings_emerged) %>% unique() %>% filter(species == "Car_pil") %>% mutate(sum = sum(total_seedlings_emerged, na.rm = TRUE)) %>%  select(sum) %>% unique() %>% as.numeric()
n_seedlings_Suc_pra <- seedling_data_subalpine %>% select(species, uniqueID, total_seedlings_emerged) %>% unique() %>% filter(species == "Suc_pra") %>% mutate(sum = sum(total_seedlings_emerged, na.rm = TRUE)) %>%  select(sum) %>% unique() %>% as.numeric()
n_seedlings_Hyp_mac <- seedling_data_subalpine %>% select(species, uniqueID, total_seedlings_emerged) %>% unique() %>% filter(species == "Hyp_mac") %>% mutate(sum = sum(total_seedlings_emerged, na.rm = TRUE)) %>%  select(sum) %>% unique() %>% as.numeric()
n_seedlings_Car_pal <- seedling_data_subalpine %>% select(species, uniqueID, total_seedlings_emerged) %>% unique() %>% filter(species == "Car_pal") %>% mutate(sum = sum(total_seedlings_emerged, na.rm = TRUE)) %>%  select(sum) %>% unique() %>% as.numeric()

seedling_survived_Ver_off <- seedling_data_subalpine %>% select(species, uniqueID, total_seedlings_survived) %>% unique() %>% filter(species == "Ver_off") %>% mutate(sum = sum(total_seedlings_survived, na.rm = TRUE)) %>% select(sum) %>% unique() %>% as.numeric()
seedling_survived_Vio_can <- seedling_data_subalpine %>% select(species, uniqueID, total_seedlings_survived) %>% unique() %>% filter(species == "Vio_can") %>% mutate(sum = sum(total_seedlings_survived, na.rm = TRUE)) %>% select(sum) %>% unique() %>% as.numeric()
seedling_survived_Car_pil <- seedling_data_subalpine %>% select(species, uniqueID, total_seedlings_survived) %>% unique() %>% filter(species == "Car_pil") %>% mutate(sum = sum(total_seedlings_survived, na.rm = TRUE)) %>% select(sum) %>% unique() %>% as.numeric()
seedling_survived_Suc_pra <- seedling_data_subalpine %>% select(species, uniqueID, total_seedlings_survived) %>% unique() %>% filter(species == "Suc_pra") %>% mutate(sum = sum(total_seedlings_survived, na.rm = TRUE)) %>% select(sum) %>% unique() %>% as.numeric()
seedling_survived_Hyp_mac <- seedling_data_subalpine %>% select(species, uniqueID, total_seedlings_survived) %>% unique() %>% filter(species == "Hyp_mac") %>% mutate(sum = sum(total_seedlings_survived, na.rm = TRUE)) %>% select(sum) %>% unique() %>% as.numeric()
seedling_survived_Car_pal <- seedling_data_subalpine %>% select(species, uniqueID, total_seedlings_survived) %>% unique() %>% filter(species == "Car_pal") %>% mutate(sum = sum(total_seedlings_survived, na.rm = TRUE)) %>% select(sum) %>% unique() %>% as.numeric()

germ_sub_alpine_plots <- length(unique(seedling_data_subalpine$plot_seedling_ID))

# dataset viii: Seed bank survival of alpine focal species -----------------

n_seeds_SP <- seedbank_survival %>% filter(species == "Sib_pro") %>% select(uniqueID) %>% unique() %>% mutate(n = length(uniqueID)) %>% select(n) %>% unique() %>% as.numeric()

n_seeds_VA <- seedbank_survival %>% filter(species == "Ver_alp") %>% select(uniqueID) %>% unique() %>% mutate(n = length(uniqueID)) %>% select(n) %>% unique() %>% as.numeric()

n_survived_seeds_SP <- seedbank_survival %>% filter(species == "Sib_pro") %>% select(survival_in_seedbank) %>%
  mutate(survival_in_seedbank = case_when(survival_in_seedbank == "1" ~ 1,
                                          survival_in_seedbank == "0" ~ 0)) %>% 
  mutate(n = sum(survival_in_seedbank)) %>% select(n) %>% unique() %>% as.numeric()

n_survived_seeds_VA <- seedbank_survival %>% filter(species == "Ver_alp") %>% select(survival_in_seedbank) %>%
  mutate(survival_in_seedbank = case_when(survival_in_seedbank == "1" ~ 1,
                                          survival_in_seedbank == "0" ~ 0)) %>% 
  mutate(n = sum(survival_in_seedbank)) %>% select(n) %>% unique() %>% as.numeric()

seed_bank_survival_rate_C_SP <- seedbank_survival %>% filter(species == "Sib_pro") %>% filter(warming == "C") %>% group_by(plotID) %>% mutate(total = n()) %>% mutate(survival_in_seedbank = case_when(survival_in_seedbank == "1" ~1, survival_in_seedbank == "0" ~ 0)) %>% mutate(bla = sum(survival_in_seedbank)) %>% mutate(prop = bla/total *100) %>% select(prop, plotID) %>% ungroup() %>%  unique() %>% mutate(mean = round(mean(prop), digits = 1)) %>% select(mean) %>%  unique() %>% as.numeric()

seed_bank_survival_rate_W_SP <- seedbank_survival %>% filter(species == "Sib_pro") %>% filter(warming == "OTC") %>% group_by(plotID) %>% mutate(total = n()) %>% mutate(survival_in_seedbank = case_when(survival_in_seedbank == "1" ~1, survival_in_seedbank == "0" ~ 0)) %>% mutate(bla = sum(survival_in_seedbank)) %>% mutate(prop = bla/total *100) %>% select(prop, plotID) %>% ungroup() %>%  unique() %>% mutate(mean = round(mean(prop), digits = 1)) %>% select(mean) %>%  unique() %>% as.numeric()

seed_bank_survival_rate_C_VA <- seedbank_survival %>% filter(species == "Ver_alp") %>% filter(warming == "C") %>% group_by(plotID) %>% mutate(total = n()) %>% mutate(survival_in_seedbank = case_when(survival_in_seedbank == "1" ~1, survival_in_seedbank == "0" ~ 0)) %>% mutate(bla = sum(survival_in_seedbank)) %>% mutate(prop = bla/total *100) %>% select(prop, plotID) %>% ungroup() %>%  unique() %>% mutate(mean = round(mean(prop), digits = 1)) %>% select(mean) %>%  unique() %>% as.numeric()

seed_bank_survival_rate_W_VA <- seedbank_survival %>% filter(species == "Ver_alp") %>% filter(warming == "OTC") %>% group_by(plotID) %>% mutate(total = n()) %>% mutate(survival_in_seedbank = case_when(survival_in_seedbank == "1" ~1, survival_in_seedbank == "0" ~ 0)) %>% mutate(bla = sum(survival_in_seedbank)) %>% mutate(prop = bla/total *100) %>% select(prop, plotID) %>% ungroup() %>%  unique() %>% mutate(mean = round(mean(prop), digits = 1)) %>% select(mean) %>%  unique() %>% as.numeric()

# dataset ix: Flowering of alpine plant community --------------------------

n_species_flowering <- length(unique(flowering$species))
n_species_length_of_inflorescence <- length(unique((flowering %>% filter(flowering_measurement_type == "Length of inflorescence"))$species))
n_species_n_inflorescence_units <- length(unique((flowering %>% filter(flowering_measurement_type == "Number of inflorescence units"))$species))
n_species_n_flower <- length(unique((flowering %>% filter(flowering_measurement_type == "Flower count"))$species))
n_species_n_flowering_individuals <- length(unique((flowering %>% filter(flowering_measurement_type == "Number of flowering individuals"))$species))
n_species_cover <- length(unique((flowering %>% filter(flowering_measurement_type == "Percent cover"))$species))
n_species_n_flower_head <- length(unique((flowering %>% filter(flowering_measurement_type == "Flower head count"))$species))

mean_length_of_inflorescence <-round(mean((flowering %>% filter(flowering_measurement_type == "Length of inflorescence"))$flowering_value), digits = 0)
mean_n_inflorescence_units <- round(mean((flowering %>% filter(flowering_measurement_type == "Number of inflorescence units"))$flowering_value), digits = 0)
mean_n_flower <- round(mean((flowering %>% filter(flowering_measurement_type == "Flower count"))$flowering_value), digits = 0)
mean_n_flowering_individuals <- round(mean((flowering %>% filter(flowering_measurement_type == "Number of flowering individuals"))$flowering_value), digits = 0)
mean_cover <- round(mean((flowering %>% filter(flowering_measurement_type == "Percent cover"))$flowering_value), digits = 0)
mean_n_flower_head <-round(mean((flowering %>% filter(flowering_measurement_type == "Flower head count"))$flowering_value), digits = 0)

flowering_plots <- length(unique(flowering$plotID))

# dataset x: Reflactance (NDVI) --------------------------------------------

n_NDVI <- length(ndvi$NDVI)
mean_NDVI_SKJ <- round(mean((ndvi %>% filter(siteID == "Skjelingahaugen"))$NDVI, na.rm = TRUE), digits = 3)
mean_NDVI_GUD <- round(mean((ndvi %>% filter(siteID == "Gudmedalen"))$NDVI, na.rm = TRUE), digits = 3)
mean_NDVI_LAV <- round(mean((ndvi %>% filter(siteID == "Lavisdalen"))$NDVI, na.rm = TRUE), digits = 3)
mean_NDVI_ULV <- round(mean((ndvi %>% filter(siteID == "Ulvhaugen"))$NDVI, na.rm = TRUE), digits = 3)

mean_NDVI_C <- round(mean((ndvi %>% filter(treatment == "C") %>% filter(OTC == "C"))$NDVI, na.rm = TRUE), digits = 3)
sd_NDVI_C <- round(sd((ndvi %>% filter(treatment == "C") %>% filter(OTC == "C"))$NDVI, na.rm = TRUE), digits = 3)
mean_NDVI_W <- round(mean((ndvi %>% filter(treatment == "C") %>% filter(OTC == "W"))$NDVI, na.rm = TRUE), digits = 3)
sd_NDVI_W <- round(sd((ndvi %>% filter(treatment == "C") %>% filter(OTC == "W"))$NDVI, na.rm = TRUE), digits = 3)

ndvi_plots <- length(unique(ndvi$plotID))

# dataset xi: Ecosystem carbon fluxes ---------------------------------------

cflux_2020_plots <- length(unique(cflux$turfID))

# dataset xii: Microclimate -------------------------------------------------

n_data_soil_moisture <- length((microclimate_soil_moisture %>%  filter(!is.na(soil_moisture)))$soil_moisture)
n_data_soil_temp <- length((microclimate_soil_temperature %>%  filter(!is.na(soil_temperature)))$soil_temperature)
n_data_ground_temp <- length((microclimate_ground_temperature %>% filter(!is.na(ground_temperature)))$ground_temperature)
n_data_air_temp <- length((microclimate_air_temperature %>% filter(!is.na(air_temperature)))$air_temperature)
n_microclimate_loggers <- length(unique(microclimate_soil_moisture$loggerID))


min_soil_moisture <- round(min(microclimate_soil_moisture$soil_moisture, na.rm = TRUE), digits = 10)
max_soil_moisture <- round(max(microclimate_soil_moisture$soil_moisture, na.rm = TRUE), digits = 3)

mean_soil_moisture_SKJ <- round(mean((microclimate_soil_moisture %>% filter(siteID == "Skjellingahaugen"))$soil_moisture, na.rm = TRUE), digits = 3)
sd_soil_moisture_SKJ <- round(sd((microclimate_soil_moisture %>% filter(siteID == "Skjellingahaugen"))$soil_moisture, na.rm = TRUE), digits = 3)
mean_soil_moisture_GUD <- round(mean((microclimate_soil_moisture %>% filter(siteID == "Gudmedalen"))$soil_moisture, na.rm = TRUE), digits = 3)
sd_soil_moisture_GUD <- round(sd((microclimate_soil_moisture %>% filter(siteID == "Gudmedalen"))$soil_moisture, na.rm = TRUE), digits = 3)
mean_soil_moisture_LAV <- round(mean((microclimate_soil_moisture %>% filter(siteID == "Lavisdalen"))$soil_moisture, na.rm = TRUE), digits = 3)
sd_soil_moisture_LAV <- round(sd((microclimate_soil_moisture %>% filter(siteID == "Lavisdalen"))$soil_moisture, na.rm = TRUE), digits =3)
mean_soil_moisture_ULV <- round(mean((microclimate_soil_moisture %>% filter(siteID == "Ulvehaugen"))$soil_moisture, na.rm = TRUE), digits = 3)
sd_soil_moisture_ULV <- round(sd((microclimate_soil_moisture %>% filter(siteID == "Ulvehaugen"))$soil_moisture, na.rm = TRUE), digits = 3)

mean_soil_temperature <- round(mean(microclimate_soil_temperature$soil_temperature, na.rm = TRUE), digits = 2)
sd_soil_temperature <- round(sd(microclimate_soil_temperature$soil_temperature, na.rm = TRUE), digits = 2)
mean_ground_temperature <- round(mean(microclimate_ground_temperature$ground_temperature, na.rm = TRUE), digits = 2)
sd_ground_temperature <- round(sd(microclimate_ground_temperature$ground_temperature, na.rm = TRUE), digits = 2)
mean_air_temperature <- round(mean(microclimate_air_temperature$air_temperature, na.rm = TRUE), digits = 2)
sd_air_temperature <- round(sd(microclimate_air_temperature$air_temperature, na.rm = TRUE), digits = 2)


microclimate_plots <- length(unique(microclimate_soil_moisture$plotID))
