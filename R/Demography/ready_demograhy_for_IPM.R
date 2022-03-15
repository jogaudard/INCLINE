################################################################
### Script for making demography data ready for IPM analysis ###
################################################################

#source("R/Demography/cleaning_demography.R")

#### Libraries ####
library(tidyverse)
library(lme4)
library(lmerTest)
library(lubridate)

#### Downloading data from OSF ####

#osf_auth(token = "get from my document every time you run the code") 

get_file(node = "zhk3m",
         file = "Seeds_per_capsule.csv",
         path = "data/Demography",
         remote_path = "RawData/Demography")

#### Load data ####

Seeds_per_capsule <- read_csv2("data/Demography/Seeds_per_capsule.csv")
biomass_Sib_pro <- read_csv2("data/Demography/Biomass_Sib_pro.csv")
biomass_Ver_alp <- read_delim("data/Demography/SeedClim_Ver_alp_biomass_regression.txt")
biomass_Ver_alp_INCLINE <- read_csv2("data/Demography/SG.19_above-below_allocation.csv")
seedling_est <- read.csv2("data/Demography/INCLINE_seedling_data.csv")
biomass_Ver_alp_INCLINE2 <- read_csv2("data/Demography/VeronicaAlpina_Biomass_Seedclim_edited.csv")


#### Biomass regressions ####
#This section calculate the biomass regressions that and find the coefficients used to calculate the estimated biomass of each individual

##### Sibbaldia procumbens #####
biomass_Sib_pro <- biomass_Sib_pro %>% 
  rename(siteID = Lok, plot = Plot, subplot = Rute, individual = Ind, date = Dat, comment = ...22, root_mass = R, leaf_mass = L, leaf_stalk_mass = LS, flower_mass = RF, bud_mass = RB, capsule_mass = RC, flower_stem_mass = RIF) %>%   #Rename to lower capital, and correct naming convention for the INCLINE project, and spell out names so that they make sense for outsiders
  select(-prec) %>% 
  mutate(vegetative_mass = root_mass + leaf_mass + leaf_stalk_mass) %>% 
  mutate(vegetative_mass = log2(vegetative_mass)) %>% 
  mutate(full_leaf_mass = leaf_mass+leaf_stalk_mass) %>% 
  mutate(full_leaf_mass = log2(full_leaf_mass)) %>% 
  filter(!is.na(siteID))

Sib_pro_biomass_regression <- lmer(full_leaf_mass ~ LSL + NL + LL + (1|siteID), data = biomass_Sib_pro) #Removed LSL (leaf stalk length) because it was not significantly important for the biomass

summary(Sib_pro_biomass_regression)

Sib_pro_coef <- coef(Sib_pro_biomass_regression)$siteID

Sib_pro_coef <- Sib_pro_coef %>% 
  rownames_to_column() %>% 
  mutate(siteID = case_when(rowname == "Gudmedalen" ~ "Gud",
                             rowname == "Lavisdalen" ~ "Lav",
                             rowname == "Skjellingahaugen" ~ "Skj",
                             rowname == "Ulvehaugen" ~ "Ulv")) %>% 
  select(!rowname) %>% 
  rename(Intercept = "(Intercept)", LSL_coef = LSL, NL_coef = NL, LL_coef = LL)


##### Veronica alpina #####

#Using placeholder data from SeedClim for the biomass regressions there
Ver_alp_coef <- biomass_Ver_alp %>% 
  filter(species == "valp") %>% 
  select(!species) %>% 
  rename(Intercept = "(Intercept)", SH_coef = SH, NL_coef = NL, LL_coef = LL, WL_coef = WL)

#INCLINE data for biomass regressions - we do not trust this data, using old data from SeedClim
# biomass_Ver_alp_INCLINE1 <- biomass_Ver_alp_INCLINE %>% 
#   filter(Species == "Veralp") %>% 
#   select(Site, SH, NL, LL, WL, AB, BB) %>% 
#   filter(!is.na(AB)) %>% 
#   mutate(AB = log2(AB))
# 
# Ver_alp_biomass_regression <- lm(AB ~ SH + NL + LL + WL, data = biomass_Ver_alp_INCLINE1)
# Ver_alp_biomass_regression <- lm(AB ~ SH + NL, data = biomass_Ver_alp_INCLINE1)
# 
# summary(Ver_alp_biomass_regression)

#### Seeds per capsules coefficients ####
#This section calculate the amount of seeds per capsule, and test if that is related to size of the mother or site. If it is related make regression and find coefficients for calculating it later (Veronica), if not, use the mean seed number per capsule (Sibbaldia).

###### Sibbaldia procumbens ######

Seeds_per_capsule_SP <- Seeds_per_capsule %>% 
  filter(Species == "Sib_pro") %>% 
  mutate(mean_seeds = mean(Number_of_seeds, na.rm = TRUE)) %>% 
  mutate(Site = case_when(Site == "LAV" ~ "Lav",
                          Site == "ULV" ~ "Ulv",
                          Site == "GUD" ~ "Gud",
                          Site == "SKJ" ~ "Skj")) %>% 
  left_join(Sib_pro_coef, by = c("Site" = "siteID")) %>% 
  mutate(size = Intercept + Leaf_stock_length_mm*LSL_coef + Number_of_leaves*NL_coef + Leaf_length_mm* LL_coef)

seed1 <- lm(Number_of_seeds ~ size, data = Seeds_per_capsule_SP) #Testing if seeds per capsule depends on biomass, it does not.
summary(seed1)
seed2 <- lm(Number_of_seeds ~ Site, data = Seeds_per_capsule_SP) #Testing if seeds per capsule depends on location, it does not.
summary(seed2)

Seeds_per_capsule_SP <- Seeds_per_capsule_SP %>% 
  select(mean_seeds) %>% 
  unique()

Seeds_per_capsule_SP <- Seeds_per_capsule_SP$mean_seeds

###### Veronica alpina ######

Seeds_per_capsule_VA <- Seeds_per_capsule %>% 
  filter(Species == "Ver_alp") %>% 
  mutate(mean_seeds = mean(Number_of_seeds, na.rm = TRUE)) %>%
  add_column(Ver_alp_coef) %>% 
  mutate(size = Intercept + Shoot_height_mm * SH_coef + Number_of_leaves * NL_coef + Leaf_length_mm * LL_coef + Leaf_width_mm * WL_coef) #Making biomass estimate with intercept and coefficients from biomass regression

seed_VA_1 <- lm(Number_of_seeds ~ Site, data = Seeds_per_capsule_VA)
summary(seed_VA_1)
seed_VA_2 <- lm(Number_of_seeds ~ size, data = Seeds_per_capsule_VA)
summary(seed_VA_2)

Seeds_per_capsule_VA_coef <- coef(seed_VA_2) %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  pivot_wider(names_from = "rowname", values_from = ".") %>% 
  rename(Intercept_seeds = "(Intercept)", size_seed = size)
  
#### Seedling establishment coefficients ####
#This section calculate the seedling establishment rate for each species in the warmed and unwarmed plots (using the data from the vegetated plots further in the analysis)

seedling_est <- seedling_est %>% 
  filter(Year == 2020) %>% 
  filter(Plot %in% c("C", "OTC")) %>% 
  select(Site, Block, Plot, PlotID, Date, Species, ID, Vegetation, Present) %>% 
  mutate(Date = dmy(Date)) %>% 
  mutate(campaign_number = if_else(Date == "2020-06-29" | Date == "2020-06-30" | Date == "2020-07-13" | Date == "2020-07-14" | Date == "2020-07-17", "first",
                                   if_else(Date == "2020-08-05" | Date == "2020-08-07" | Date == "2020-08-12" | Date == "2020-08-13" | Date == "2020-08-14", "second",
                                         if_else(Date == "2020-08-26" | Date == "2020-08-24" | Date == "2020-08-25" | Date == "2020-08-27", "third", "NA"))))

###### Veronica alpina ######

seedling_est_VA <- seedling_est %>% 
  filter(Species == "Ver_alp") %>% 
  filter(campaign_number == "second") %>%
  filter(Present == "yes") %>% 
  group_by(Site, Block, Plot, PlotID, Vegetation) %>% 
  mutate(total_germinated = n()) %>% 
  ungroup() %>% 
  mutate(total_seeds = 20) %>% 
  mutate(germination_percentage = total_germinated/total_seeds) %>% 
  select(-ID) %>% 
  unique() 

model1 <- lm(germination_percentage ~ Plot + Vegetation, data = seedling_est_VA)
summary(model1)

seedling_est_VA <- seedling_est_VA%>% 
  select(Vegetation, germination_percentage) %>% 
  group_by(Vegetation) %>% 
  mutate(germination_percentage = mean(germination_percentage)) %>% 
  unique()

seedling_est_VA_NoVeg <- seedling_est_VA %>% 
  filter(Vegetation == "no")

seedling_est_VA_NoVeg <- seedling_est_VA_NoVeg$germination_percentage

seedling_est_VA_Veg <- seedling_est_VA %>% 
  filter(Vegetation == "yes")

seedling_est_VA_Veg <- seedling_est_VA_Veg$germination_percentage
  
###### Sibbaldia procumbens ######
seedling_est_SP <- seedling_est %>% 
  filter(Species == "Sib_pro") %>% 
  filter(campaign_number == "second") %>%
  filter(Present == "yes") %>% 
  group_by(Site, Block, Plot, PlotID, Vegetation) %>% 
  mutate(total_germinated = n()) %>% 
  ungroup() %>% 
  mutate(total_seeds = 20) %>% 
  mutate(germination_percentage = total_germinated/total_seeds) %>% 
  select(-ID) %>% 
  unique() 

model2 <- lm(germination_percentage ~ Plot + Vegetation, data = seedling_est_SP)
summary(model2)

seedling_est_SP <- seedling_est_SP%>% 
  select(Plot, germination_percentage) %>% 
  group_by(Plot) %>% 
  mutate(germination_percentage = mean(germination_percentage)) %>% 
  unique()

seedling_est_SP_W <- seedling_est_SP %>% 
  filter(Plot == "OTC")

seedling_est_SP_W <- seedling_est_SP_W$germination_percentage

seedling_est_SP_C <- seedling_est_SP %>% 
  filter(Plot == "C")

seedling_est_SP_C <- seedling_est_SP_C$germination_percentage

#### Making seedling information ####
#This section calculates the average seedlings size of each species, and adding in the seedling establishment rate in the same dataset to have all seedling data together

###### Sibaldia procumbens ######

Seedling_info_SP <- Sib_pro %>% 
  filter(seedling == "yes") %>% 
  left_join(Sib_pro_coef, by = "siteID") %>% 
  mutate(size = Intercept + LSL * LSL_coef + NL * NL_coef + LL * LL_coef) #Making biomass estimate with intercept and coefficients from biomass regression

model_seedling <- lmer(size ~ treatment + (1|siteID), data = Seedling_info_SP)

summary(model_seedling) #Warming does not affect the size of the seedling, but treatment does (extant and novel)


Seedling_info_SP <- Seedling_info_SP %>%
  group_by(treatment) %>% 
  mutate(seeds_cap = mean(size, na.rm = TRUE),
         seeds_cap_sd = sd(size, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(seedling_establishment_rate = if_else(OTC == "C", seedling_est_SP_C,
                                               if_else(OTC == "W", seedling_est_SP_W, 0))) %>% 
  select(OTC, treatment, seeds_cap, seeds_cap_sd, seedling_establishment_rate) %>% 
  distinct()

###### Veronica alpina ######

Seedling_info_VA <- Ver_alp %>% 
  filter(seedling == "yes") %>% 
  add_column(Ver_alp_coef) %>% 
  mutate(size = Intercept + SH * SH_coef + NL * NL_coef + LL * LL_coef + WL * WL_coef) #Making biomass estimate with intercept and coefficients from biomass regression

model_seedling_VA <- lmer(size ~ treatment + (1|siteID), data = Seedling_info_VA)

summary(model_seedling_VA)

Seedling_info_VA <- Seedling_info_VA %>% 
  group_by(treatment) %>% 
  mutate(seeds_cap = mean(size, na.rm = TRUE),
         seeds_cap_sd = sd(size, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(seedling_establishment_rate = if_else(treatment == "C" | treatment == "E" | treatment == "N", seedling_est_VA_Veg,
                                               if_else(treatment == "R", seedling_est_VA_NoVeg, 0))) %>% 
  select(treatment, seeds_cap, seeds_cap_sd, seedling_establishment_rate) %>% 
  distinct()

#### Making transitions ####
#This section calculates the size of individuals, estimates of seed number. And cleaning the data so that we have the correct variables, and variable names for the analysis.


###### Sibaldia procumbens ######

Sib_pro_2018 <- Sib_pro %>% 
  filter(year == 2018) %>% 
  select(siteID, plotID, unique_IDS, OTC, treatment, year, LSL, NL, LL, NFL, NB, NC, NAC, seedling, juvenile)

Sib_pro_2019 <- Sib_pro %>% 
  filter(year == 2019) %>% 
  select(siteID, plotID, unique_IDS, OTC, treatment, year, LSL, NL, LL, NFL, NB, NC, NAC, seedling, juvenile)

Sib_pro_2020 <- Sib_pro %>% 
  filter(year == 2020) %>% 
  select(siteID, plotID, unique_IDS, OTC, treatment, year, LSL, NL, LL, NFL, NB, NC, NAC, seedling, juvenile)

Sib_pro_2021 <- Sib_pro %>% 
  filter(year == 2021) %>% 
  select(siteID, plotID, unique_IDS, OTC, treatment, year, LSL, NL, LL, NFL, NB, NC, NAC, seedling, juvenile)


Sib_pro_2018_2019 <- Sib_pro_2018 %>% 
  left_join(Sib_pro_coef, by = "siteID") %>% 
  full_join(Sib_pro_2019, by = c("unique_IDS", "plotID", "OTC", "treatment"), suffix = c("_2018", "_2019")) %>% 
  mutate(size = Intercept + NL_2018 * NL_coef + LL_2018 * LL_coef,
         sizeNext = Intercept + NL_2019 * NL_coef + LL_2019 * LL_coef,
         fec = (Seeds_per_capsule_SP * NFL_2018) + (Seeds_per_capsule_SP * NB_2018) + (Seeds_per_capsule_SP * NC_2018), 
         surv = ifelse(size > 0 & is.na(sizeNext), 0,
                       ifelse(size > 0 & sizeNext > 0, 1, NA))) %>% 
  mutate(flo.no = rowSums(dplyr::select(., NB_2018, NFL_2018, NC_2018), na.rm=TRUE),
         flo.if = ifelse(flo.no > 0, 1, 0)) %>%
  mutate(offspringNext = ifelse(seedling_2019 == "yes" & is.na(size), "sexual",
                                ifelse(juvenile_2019 == "yes" & is.na(size), "sexual",
                                       ifelse(is.na(size) & sizeNext>0, "clone", NA)))) %>% 
  ## Make clonal information (clo.if, clo.no and transfer the size of the mother to size)
  select(unique_IDS, OTC, treatment, size, sizeNext, fec, surv, flo.no, flo.if, offspringNext, seedling_2019, juvenile_2019) %>% 
  rename(seedlingNext = seedling_2019, juvenileNext = juvenile_2019) %>% 
  mutate(transition = "2018-2019")

Sib_pro_2019_2020 <- Sib_pro_2019 %>% 
  left_join(Sib_pro_coef, by = "siteID") %>% 
  full_join(Sib_pro_2020, by = c("unique_IDS", "plotID", "OTC", "treatment"), suffix = c("_2019", "_2020")) %>% 
  mutate(size = Intercept + NL_2019 * NL_coef + LL_2019 * LL_coef,
         sizeNext = Intercept + NL_2020 * NL_coef + LL_2020 * LL_coef,
         fec = (Seeds_per_capsule_SP * NFL_2019) + (Seeds_per_capsule_SP * NB_2019) + (Seeds_per_capsule_SP * NC_2019), 
         surv = ifelse(size > 0 & is.na(sizeNext), 0,
                       ifelse(size > 0 & sizeNext > 0, 1, NA))) %>% 
  mutate(flo.no = rowSums(dplyr::select(., NB_2019, NFL_2019, NC_2019), na.rm=TRUE),
         flo.if = ifelse(flo.no > 0, 1, 0)) %>%
  mutate(offspringNext = ifelse(seedling_2020 == "yes" & is.na(size), "sexual",
                                ifelse(juvenile_2020 == "yes" & is.na(size), "sexual",
                                       ifelse(is.na(size) & sizeNext>0, "clone", NA)))) %>% 
  ## Make clonal information (clo.if, clo.no and transfer the size of the mother to size)
  select(unique_IDS, OTC, treatment, size, sizeNext, fec, surv, flo.no, flo.if, offspringNext, seedling_2020, juvenile_2020) %>% 
  rename(seedlingNext = seedling_2020, juvenileNext = juvenile_2020) %>% 
  mutate(transition = "2019-2020")

Sib_pro_2020_2021 <- Sib_pro_2020 %>% 
  left_join(Sib_pro_coef, by = "siteID") %>% 
  full_join(Sib_pro_2021, by = c("unique_IDS", "plotID", "OTC", "treatment"), suffix = c("_2020", "_2021")) %>% 
  mutate(size = Intercept + NL_2020 * NL_coef + LL_2020 * LL_coef,
         sizeNext = Intercept + NL_2021 * NL_coef + LL_2021 * LL_coef,
         fec = (Seeds_per_capsule_SP * NFL_2020) + (Seeds_per_capsule_SP * NB_2020) + (Seeds_per_capsule_SP * NC_2020), 
         surv = ifelse(size > 0 & is.na(sizeNext), 0,
                       ifelse(size > 0 & sizeNext > 0, 1, NA))) %>% 
  mutate(flo.no = rowSums(dplyr::select(., NB_2020, NFL_2020, NC_2020), na.rm=TRUE),
         flo.if = ifelse(flo.no > 0, 1, 0)) %>%
  mutate(offspringNext = ifelse(seedling_2021 == "yes" & is.na(size), "sexual",
                                ifelse(juvenile_2021 == "yes" & is.na(size), "sexual",
                                       ifelse(is.na(size) & sizeNext>0, "clone", NA)))) %>% 
  ## Make clonal information (clo.if, clo.no and transfer the size of the mother to size)
  select(unique_IDS, OTC, treatment, size, sizeNext, fec, surv, flo.no, flo.if, offspringNext, seedling_2021, juvenile_2021) %>% 
  rename(seedlingNext = seedling_2021, juvenileNext = juvenile_2021)%>% 
  mutate(transition = "2020-2021")



Sib_pro_2018_2021 <- bind_rows(Sib_pro_2018_2019, Sib_pro_2019_2020, Sib_pro_2020_2021)

#Some plots fro visualization/checking
Sib_pro_2018_2021 %>% ggplot(aes(y = sizeNext, x = size, color = flo.if)) + geom_point() + geom_abline()
Sib_pro_2018_2021 %>% ggplot(aes(y = sizeNext, x = size, color = seedlingNext)) + geom_point() + geom_abline()

##### Veronica alpina #####

Ver_alp_2018 <- Ver_alp %>% 
  filter(year == 2018) %>% 
  select(plotID, unique_IDS, OTC, treatment, year, SH, NL, LL, WL, NFL, NB, NC, NAC, seedling, juvenile)

Ver_alp_2019 <- Ver_alp %>% 
  filter(year == 2019) %>% 
  select(plotID, unique_IDS, OTC, treatment, year, SH, NL, LL, WL, NFL, NB, NC, NAC, seedling, juvenile)

Ver_alp_2020 <- Ver_alp %>% 
  filter(year == 2020) %>% 
  select(plotID, unique_IDS, OTC, treatment, year, SH, NL, LL, WL, NFL, NB, NC, NAC, seedling, juvenile)

Ver_alp_2021 <- Ver_alp %>% 
  filter(year == 2021) %>% 
  select(plotID, unique_IDS, OTC, treatment, year, SH, NL, LL, WL, NFL, NB, NC, NAC, seedling, juvenile)


Ver_alp_2018_2019 <- Ver_alp_2018 %>% 
  add_column(Ver_alp_coef) %>% 
  add_column(Seeds_per_capsule_VA_coef) %>% 
  full_join(Ver_alp_2019, by = c("unique_IDS", "plotID", "OTC", "treatment"), suffix = c("_2018", "_2019")) %>% 
  mutate(size = Intercept + SH_2018 * SH_coef + NL_2018 * NL_coef + LL_2018 * LL_coef + WL_2018 * WL_coef, 
         sizeNext = Intercept + SH_2019 * SH_coef + NL_2019 * NL_coef + LL_2019 * LL_coef + WL_2019 * WL_coef,
         surv = ifelse(size > 0 & is.na(sizeNext), 0,
                       ifelse(size > 0 & sizeNext > 0, 1, NA))) %>% 
  mutate(flo.no = rowSums(dplyr::select(., NB_2018, NFL_2018, NC_2018), na.rm=TRUE),
         flo.if = ifelse(flo.no > 0, 1, 0),
         fec = ((Intercept_seeds + size * size_seed)  * flo.no)) %>%
  mutate(offspringNext = ifelse(seedling_2019 == "yes" & is.na(size), "sexual",
                                ifelse(juvenile_2019 == "yes" & is.na(size), "sexual",
                                       ifelse(is.na(size) & sizeNext>0, "clone", NA)))) %>% 
  ## Make clonal information (clo.if, clo.no and transfer the size of the mother to size)
  select(unique_IDS, OTC, treatment, size, sizeNext, fec, surv, flo.no, flo.if, offspringNext, seedling_2019, juvenile_2019) %>% 
  rename(seedlingNext = seedling_2019, juvenileNext = juvenile_2019) %>% 
  mutate(transition = "2018-2019")

Ver_alp_2019_2020 <- Ver_alp_2019 %>% 
  add_column(Ver_alp_coef) %>% 
  add_column(Seeds_per_capsule_VA_coef) %>% 
  full_join(Ver_alp_2020, by = c("unique_IDS", "plotID", "OTC", "treatment"), suffix = c("_2019", "_2020")) %>% 
  mutate(size = Intercept + SH_2019 * SH_coef + NL_2019 * NL_coef + LL_2019 * LL_coef + WL_2019 * WL_coef, 
         sizeNext = Intercept + SH_2020 * SH_coef + NL_2020 * NL_coef + LL_2020 * LL_coef + WL_2020 * WL_coef, 
         surv = ifelse(size > 0 & is.na(sizeNext), 0,
                       ifelse(size > 0 & sizeNext > 0, 1, NA))) %>% 
  mutate(flo.no = rowSums(dplyr::select(., NB_2019, NFL_2019, NC_2019), na.rm=TRUE),
         flo.if = ifelse(flo.no > 0, 1, 0),
         fec = ((Intercept_seeds + size * size_seed)  * flo.no)) %>%
  mutate(offspringNext = ifelse(seedling_2020 == "yes" & is.na(size), "sexual",
                                ifelse(juvenile_2020 == "yes" & is.na(size), "sexual",
                                       ifelse(is.na(size) & sizeNext>0, "clone", NA)))) %>% 
  ## Make clonal information (clo.if, clo.no and transfer the size of the mother to size)
  select(unique_IDS, OTC, treatment, size, sizeNext, fec, surv, flo.no, flo.if, offspringNext, seedling_2020, juvenile_2020) %>% 
  rename(seedlingNext = seedling_2020, juvenileNext = juvenile_2020) %>% 
  mutate(transition = "2019-2020")

Ver_alp_2020_2021 <- Ver_alp_2020 %>% 
  add_column(Ver_alp_coef) %>% 
  add_column(Seeds_per_capsule_VA_coef) %>% 
  full_join(Ver_alp_2021, by = c("unique_IDS", "plotID", "OTC", "treatment"), suffix = c("_2020", "_2021")) %>% 
  mutate(size = Intercept + SH_2020 * SH_coef + NL_2020 * NL_coef + LL_2020 * LL_coef + WL_2020 * WL_coef, 
         sizeNext = Intercept + SH_2021 * SH_coef + NL_2021 * NL_coef + LL_2021 * LL_coef + WL_2021 * WL_coef, 
         surv = ifelse(size > 0 & is.na(sizeNext), 0,
                       ifelse(size > 0 & sizeNext > 0, 1, NA))) %>% 
  mutate(flo.no = rowSums(dplyr::select(., NB_2020, NFL_2020, NC_2020), na.rm=TRUE),
         flo.if = ifelse(flo.no > 0, 1, 0),
         fec = ((Intercept_seeds + size * size_seed)  * flo.no)) %>%
  mutate(offspringNext = ifelse(seedling_2021 == "yes" & is.na(size), "sexual",
                                ifelse(juvenile_2021 == "yes" & is.na(size), "sexual",
                                       ifelse(is.na(size) & sizeNext>0, "clone", NA)))) %>% 
  ## Make clonal information (clo.if, clo.no and transfer the size of the mother to size)
  select(unique_IDS, OTC, treatment, size, sizeNext, fec, surv, flo.no, flo.if, offspringNext, seedling_2021, juvenile_2021) %>% 
  rename(seedlingNext = seedling_2021, juvenileNext = juvenile_2021)%>% 
  mutate(transition = "2020-2021")


Ver_alp_2018_2021 <- bind_rows(Ver_alp_2018_2019, Ver_alp_2019_2020, Ver_alp_2020_2021)

