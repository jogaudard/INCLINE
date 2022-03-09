################################################################
### Script for making demography data ready for IPM analysis ###
################################################################

#source("R/Demography/cleaning_demography.R")

#### Libraries ####
library(tidyverse)
library(lme4)
library(lmerTest)

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
#Need biomass regression datasets as well

#### Biomass regressions ####
#This section will calculate the biomass regressions that will find the constants used to calculate the estimated biomass of each individual.

biomass_Sib_pro <- biomass_Sib_pro %>% 
  rename(siteID = Lok, plot = Plot, subplot = Rute, individual = Ind, date = Dat, comment = ...22, root_mass = R, leaf_mass = L, leaf_stalk_mass = LS, flower_mass = RF, bud_mass = RB, capsule_mass = RC, flower_stem_mass = RIF) %>%   #Rename to lower capital, and correct naming convention for the INCLINE project, and spell out names so that they make sense for outsiders
  select(-prec) %>% 
  mutate(vegetative_mass = root_mass + leaf_mass + leaf_stalk_mass) %>% 
  mutate(vegetative_mass = log2(vegetative_mass)) %>% 
  mutate(full_leaf_mass = leaf_mass+leaf_stalk_mass) %>% 
  mutate(full_leaf_mass = log2(full_leaf_mass)) %>% 
  filter(!is.na(siteID))

Sib_pro_biomass_regression <- lmer(vegetative_mass ~ NL + LL + (1|siteID), data = biomass_Sib_pro) #Removed LSL (leaf stalk length) because it was not significantly important for the biomass

summary(Sib_pro_biomass_regression)

Sib_pro_coef <- coef(Sib_pro_biomass_regression)$siteID

Sib_pro_coef <- Sib_pro_coef %>% 
  rownames_to_column() %>% 
  mutate(siteID = case_when(rowname == "Gudmedalen" ~ "Gud",
                             rowname == "Lavisdalen" ~ "Lav",
                             rowname == "Skjellingahaugen" ~ "Skj",
                             rowname == "Ulvehaugen" ~ "Ulv")) %>% 
  select(!rowname) %>% 
  rename(Intercept = "(Intercept)", NL_coef = NL, LL_coef = LL)


#Using placeholder data from SeedClim for the biomass regressions there
Ver_alp_coef <- biomass_Ver_alp %>% 
  filter(species == "valp") %>% 
  select(!species) %>% 
  rename(Intercept = "(Intercept)", SH_coef = SH, NL_coef = NL, LL_coef = LL, WL_coef = WL)
  

#### Seeds per capsules ####
#This section will be calculating the amount of seeds per capsule based of the size of the mother, need the biomass regressions first

##### Sibbaldia procumbens #####

#### Making seedling information ####

Seedling_info_SP <- Sib_pro %>% 
  filter(seedling == "yes") %>% 
  left_join(Sib_pro_coef, by = "siteID") %>% 
  mutate(size = Intercept + NL * NL_coef + LL * LL_coef) %>% #Making biomass estimate with intercept and coefficients from biomass regression
  mutate(seeds_cap = mean(size, na.rm = TRUE),
         seeds_cap_sd = sd(size, na.rm = TRUE),
         seedling_establishment_rate = 0.6) %>% # 60% chance of germinating in the lab with seeds from sibbaldia at Lavsidalen
  select(seeds_cap, seeds_cap_sd, seedling_establishment_rate) %>% 
  distinct()

#### Making transitions ####

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
         fec = (4.38 * NFL_2018) + (4.38 * NB_2018) + (4.38 * NC_2018), #Average seeds per flower at Skjellingahaugen was 4.38
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
         fec = (4.38 * NFL_2019) + (4.38 * NB_2019) + (4.38 * NC_2019), #Average seeds per flower at Skjellingahaugen was 4.38
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
         fec = (4.38 * NFL_2020) + (4.38 * NB_2020) + (4.38 * NC_2020), #Average seeds per flower at Skjellingahaugen was 4.38
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

Sib_pro_2018_2021 %>% ggplot(aes(x = sizeNext, y = size, color = flo.if)) + geom_point() + geom_abline()

##### Veronica alpina #####

#### Making seedling information ####

Seedling_info_VA <- Ver_alp %>% 
  filter(seedling == "yes") %>% 
  mutate(size = 2.625811097 + SH * 0.005558019 + NL * 0.069472337 + LL * 0.066783627 + WL*0.05) %>% #Mock numbers 
  mutate(seeds_cap = mean(size, na.rm = TRUE),
         seeds_cap_sd = sd(size, na.rm = TRUE),
         seedling_establishment_rate = 0.6) %>% # Mock number
  select(seeds_cap, seeds_cap_sd, seedling_establishment_rate) %>% 
  distinct()

#### Making transitions ####

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
  full_join(Ver_alp_2019, by = c("unique_IDS", "plotID", "OTC", "treatment"), suffix = c("_2018", "_2019")) %>% 
  mutate(size = 2.625811097 + SH_2018 * 0.005558019 + NL_2018 * 0.069472337 + LL_2018 * 0.066783627 + WL_2018*0.05, #Mock numbers 
         sizeNext = 2.625811097 + SH_2019 * 0.005558019 + NL_2019 * 0.069472337 + LL_2019 * 0.066783627 + WL_2019*0.05, #Mock numbers 
         fec = (4.38 * NFL_2018) + (4.38 * NB_2018) + (4.38 * NC_2018), #Mock number of seeds per flower
         surv = ifelse(size > 0 & is.na(sizeNext), 0,
                       ifelse(size > 0 & sizeNext > 0, 1, NA)),
         flo.no = NB_2018 + NFL_2018 + NC_2018,
         flo.if = ifelse(flo.no > 0, 1, 0)) %>%
  mutate(offspringNext = ifelse(seedling_2019 == "yes" & is.na(size), "sexual",
                                ifelse(juvenile_2019 == "yes" & is.na(size), "sexual",
                                       ifelse(is.na(size) & sizeNext>0, "clone", NA)))) %>% 
  ## Make clonal information (clo.if, clo.no and transfer the size of the mother to size)
  select(unique_IDS, OTC, treatment, size, sizeNext, fec, surv, flo.no, flo.if, offspringNext, seedling_2019, juvenile_2019) %>% 
  rename(seedlingNext = seedling_2019, juvenileNext = juvenile_2019) %>% 
  mutate(transition = "2018-2019")



Ver_alp_2019_2020 <- Ver_alp_2019 %>% 
  full_join(Ver_alp_2020, by = c("unique_IDS", "plotID", "OTC", "treatment"), suffix = c("_2019", "_2020")) %>% 
  mutate(size = 2.625811097 + SH_2019 * 0.005558019 + NL_2019 * 0.069472337 + LL_2019 * 0.066783627 + WL_2019*0.05, #Mock numbers 
         sizeNext = 2.625811097 + SH_2020 * 0.005558019 + NL_2020 * 0.069472337 + LL_2020 * 0.066783627 + WL_2020*0.05, #Mock numbers 
         fec = (4.38 * NFL_2019) + (4.38 * NB_2019) + (4.38 * NC_2019), #Average seeds per flower at Skjellingahaugen was 4.38
         surv = ifelse(size > 0 & is.na(sizeNext), 0,
                       ifelse(size > 0 & sizeNext > 0, 1, NA)),
         flo.no = NB_2019 + NFL_2019 + NC_2019,
         flo.if = ifelse(flo.no > 0, 1, 0)) %>%
  mutate(offspringNext = ifelse(seedling_2020 == "yes" & is.na(size), "sexual",
                                ifelse(juvenile_2020 == "yes" & is.na(size), "sexual",
                                       ifelse(is.na(size) & sizeNext>0, "clone", NA)))) %>% 
  ## Make clonal information (clo.if, clo.no and transfer the size of the mother to size)
  select(unique_IDS, OTC, treatment, size, sizeNext, fec, surv, flo.no, flo.if, offspringNext, seedling_2020, juvenile_2020) %>% 
  rename(seedlingNext = seedling_2020, juvenileNext = juvenile_2020) %>% 
  mutate(transition = "2019-2020")

Ver_alp_2020_2021 <- Ver_alp_2020 %>% 
  full_join(Ver_alp_2021, by = c("unique_IDS", "plotID", "OTC", "treatment"), suffix = c("_2020", "_2021")) %>% 
  mutate(size = 2.625811097 + SH_2020 * 0.005558019 + NL_2020 * 0.069472337 + LL_2020 * 0.066783627 + WL_2020*0.05, #Mock numbers 
         sizeNext = 2.625811097 + SH_2021 * 0.005558019 + NL_2021 * 0.069472337 + LL_2021 * 0.066783627 + WL_2020*0.05, #Mock numbers
         fec = (4.38 * NFL_2020) + (4.38 * NB_2020) + (4.38 * NC_2020), #Average seeds per flower at Skjellingahaugen was 4.38
         surv = ifelse(size > 0 & is.na(sizeNext), 0,
                       ifelse(size > 0 & sizeNext > 0, 1, NA)),
         flo.no = NB_2020 + NFL_2020 + NC_2020,
         flo.if = ifelse(flo.no > 0, 1, 0)) %>%
  mutate(offspringNext = ifelse(seedling_2021 == "yes" & is.na(size), "sexual",
                                ifelse(juvenile_2021 == "yes" & is.na(size), "sexual",
                                       ifelse(is.na(size) & sizeNext>0, "clone", NA)))) %>% 
  ## Make clonal information (clo.if, clo.no and transfer the size of the mother to size)
  select(unique_IDS, OTC, treatment, size, sizeNext, fec, surv, flo.no, flo.if, offspringNext, seedling_2021, juvenile_2021) %>% 
  rename(seedlingNext = seedling_2021, juvenileNext = juvenile_2021)%>% 
  mutate(transition = "2020-2021")



Ver_alp_2018_2021 <- bind_rows(Ver_alp_2018_2019, Ver_alp_2019_2020, Ver_alp_2020_2021)

