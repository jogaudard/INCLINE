################################################################
### Script for making demography data ready for IPM analysis ###
################################################################

#source("R/Demography/cleaning_demography.R")

#### Libraries ####
library(tidyverse)

#### Downloading data from OSF ####

#osf_auth(token = "get from my document every time you run the code") 

get_file(node = "zhk3m",
         file = "Seeds_per_capsule.csv",
         path = "data/Demography",
         remote_path = "RawData/Demography")

##### Sibbaldia procumbens #####

#### Making seedling information ####

Seedling_info_SP <- Sib_pro %>% 
  filter(seedling == "yes") %>% 
  mutate(size = 2.625811097 + LSL * 0.005558019 + NL * 0.069472337 + LL * 0.066783627) %>% #Mock numbers from Seedclim data and another species
  mutate(seeds_cap = mean(size, na.rm = TRUE),
         seeds_cap_sd = sd(size, na.rm = TRUE),
         seedling_establishment_rate = 0.6) %>% # 60% chance of germinating in the lab with seeds from sibbaldia at Lavsidalen
  select(seeds_cap, seeds_cap_sd, seedling_establishment_rate) %>% 
  distinct()

#### Making transitions ####

Sib_pro_2018 <- Sib_pro %>% 
  filter(year == 2018) %>% 
  select(plotID, unique_IDS, OTC, treatment, year, LSL, NL, LL, NFL, NB, NC, NAC, seedling, juvenile)

Sib_pro_2019 <- Sib_pro %>% 
  filter(year == 2019) %>% 
  select(plotID, unique_IDS, OTC, treatment, year, LSL, NL, LL, NFL, NB, NC, NAC, seedling, juvenile)

Sib_pro_2020 <- Sib_pro %>% 
  filter(year == 2020) %>% 
  select(plotID, unique_IDS, OTC, treatment, year, LSL, NL, LL, NFL, NB, NC, NAC, seedling, juvenile)

Sib_pro_2021 <- Sib_pro %>% 
  filter(year == 2021) %>% 
  select(plotID, unique_IDS, OTC, treatment, year, LSL, NL, LL, NFL, NB, NC, NAC, seedling, juvenile)


Sib_pro_2018_2019 <- Sib_pro_2018 %>% 
  full_join(Sib_pro_2019, by = c("unique_IDS", "plotID", "OTC", "treatment"), suffix = c("_2018", "_2019")) %>% 
  mutate(size = 2.625811097 + LSL_2018 * 0.005558019 + NL_2018 * 0.069472337 + LL_2018 * 0.066783627, #Mock numbers from Seedclim data and another species
         sizeNext = 2.625811097 + LSL_2019 * 0.005558019 + NL_2019 * 0.069472337 + LL_2019 * 0.066783627, #Mock numbers from Seedclim data and another species
         fec = (4.38 * NFL_2018) + (4.38 * NB_2018) + (4.38 * NC_2018), #Average seeds per flower at Skjellingahaugen was 4.38
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



Sib_pro_2019_2020 <- Sib_pro_2019 %>% 
  full_join(Sib_pro_2020, by = c("unique_IDS", "plotID", "OTC", "treatment"), suffix = c("_2019", "_2020")) %>% 
  mutate(size = 2.625811097 + LSL_2019 * 0.005558019 + NL_2019 * 0.069472337 + LL_2019 * 0.066783627, #Mock numbers from Seedclim data and another species
         sizeNext = 2.625811097 + LSL_2020 * 0.005558019 + NL_2020 * 0.069472337 + LL_2020 * 0.066783627, #Mock numbers from Seedclim data and another species
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

Sib_pro_2020_2021 <- Sib_pro_2020 %>% 
  full_join(Sib_pro_2021, by = c("unique_IDS", "plotID", "OTC", "treatment"), suffix = c("_2020", "_2021")) %>% 
  mutate(size = 2.625811097 + LSL_2020 * 0.005558019 + NL_2020 * 0.069472337 + LL_2020 * 0.066783627, #Mock numbers from Seedclim data and another species
         sizeNext = 2.625811097 + LSL_2021 * 0.005558019 + NL_2021 * 0.069472337 + LL_2021 * 0.066783627, #Mock numbers from Seedclim data and another species
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



Sib_pro_2018_2021 <- bind_rows(Sib_pro_2018_2019, Sib_pro_2019_2020, Sib_pro_2020_2021)

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

