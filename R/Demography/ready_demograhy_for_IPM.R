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

get_file(node = "zhk3m",
         file = "INCLINE_seedling_data.csv",
         path = "data/Demography",
         remote_path = "RawData/Germination_field_experiment")

get_file(node = "zhk3m",
         file = "Biomass_Sib_pro.csv",
         path = "data/Demography",
         remote_path = "RawData/Demography")

#### Load data ####

Seeds_per_capsule <- read_csv2("data/Demography/Seeds_per_capsule.csv")
biomass_Sib_pro <- read_csv2("data/Demography/Biomass_Sib_pro.csv")
#biomass_Ver_alp <- read_delim("data/Demography/SeedClim_Ver_alp_biomass_regression.txt")
#biomass_Ver_alp_INCLINE <- read_csv2("data/Demography/SG.19_above-below_allocation.csv") #Not using this because some of the biomass rotted while collecting data, so biomass might not be correct
seedling_est <- read.csv2("data/Demography/INCLINE_seedling_data.csv") 
biomass_Ver_alp <- read_csv2("data/Demography/VeronicaAlpina_Biomass_Seedclim_edited.csv") #from SeedClim not on INCLINE OSF


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

Sib_pro_biomass_regression <- lmer(full_leaf_mass ~ LSL + NL + LL + (1|siteID), data = biomass_Sib_pro)

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
# Ver_alp_coef <- biomass_Ver_alp %>% 
#   filter(species == "valp") %>% 
#   select(!species) %>% 
#   rename(Intercept = "(Intercept)", SH_coef = SH, NL_coef = NL, LL_coef = LL, WL_coef = WL)

# biomass Ver_alp

biomass_Ver_alp <- biomass_Ver_alp %>% 
  select(siteID, IDS, SH, NL, LL, WL, ag) %>% 
  filter(!ag == 0) %>% 
  mutate(ag = log2(ag))

# Ver_alp_biomass_regression <- lmer(ag ~ SH + NL + LL + WL + (1|siteID), data = biomass_Ver_alp2)  #Not using this as it came with a singularity warning. Mixed effect model and linear model gives the same intercept and slopes for each variable.
# summary(Ver_alp_biomass_regression)

Ver_alp_biomass_regression_lm <- lm(ag ~ SH + NL + LL + WL , data = biomass_Ver_alp) 
summary(Ver_alp_biomass_regression_lm)

Ver_alp_coef <- coef(Ver_alp_biomass_regression_lm) %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  pivot_wider(names_from = "rowname", values_from = ".") %>% 
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
  mutate(size = Intercept + Leaf_stock_length_mm*LSL_coef + Number_of_leaves*NL_coef + Leaf_length_mm* LL_coef) %>% 
  mutate(ID = paste0(Site, "_", Species, "_", Individual))

seed1 <- lmer(Number_of_seeds ~ size +(1|ID), data = Seeds_per_capsule_SP) #Testing if seeds per capsule depends on biomass, it does not.
summary(seed1)
seed2 <- lmer(Number_of_seeds ~ Site + (1|ID), data = Seeds_per_capsule_SP) #Testing if seeds per capsule depends on location, it does not.
summary(seed2)
seed_3 <- lmer(Number_of_seeds ~ Number_of_capsules + (1|ID), data = Seeds_per_capsule_SP) #Testing if seeds per capsule depends on number of capsule for each individual, it does not.
summary(seed_3)

Seeds_per_capsule_SP %>%  ggplot(aes(x = size, y = Number_of_seeds)) + geom_point(aes(color = Site)) + geom_smooth(method = "lm", linetype = "dashed") + ggtitle("Number of seeds by size for Sibbaldia procumbens") + xlab("log2(size)") + ylab("Seed per individual") + scale_color_viridis_d()

Seeds_per_capsule_SP <- Seeds_per_capsule_SP %>% 
  select(mean_seeds) %>% 
  unique()

Seeds_per_capsule_SP <- Seeds_per_capsule_SP$mean_seeds

###### Veronica alpina ######

Seeds_per_capsule_VA <- Seeds_per_capsule %>% 
  filter(Species == "Ver_alp") %>% 
  mutate(mean_seeds = mean(Number_of_seeds, na.rm = TRUE)) %>%
  bind_cols(Ver_alp_coef) %>%
  mutate(size = Intercept + (Shoot_height_mm * SH_coef) + (Number_of_leaves * NL_coef) + (Leaf_length_mm * LL_coef) + (Leaf_width_mm * WL_coef)) %>%  #Making biomass estimate with intercept and coefficients from biomass regression
  mutate(ID = paste0(Site, "_", Species, "_", Individual))

seed_VA_1 <- lmer(Number_of_seeds ~ size + Site + (1|ID), data = Seeds_per_capsule_VA) #Testing if seeds per capsule depends on biomass and site, it does not. But size is close to significant, so trying a model with only that
summary(seed_VA_1)
seed_VA_2 <- lmer(Number_of_seeds ~ size + (1|ID), data = Seeds_per_capsule_VA) #Testing if seeds per capsule depends on biomass alone, it does not.
summary(seed_VA_2)
seed_VA_3 <- lmer(Number_of_seeds ~ Number_of_capsules + (1|ID), data = Seeds_per_capsule_VA) #Testing if seeds per capsule depends on number of capsule for each individual, it does not.
summary(seed_VA_3)


Seeds_per_capsule_VA %>%  ggplot(aes(x = size, y = Number_of_seeds)) + geom_point(aes(color = Site)) + geom_smooth(method = "lm", linetype = "dashed") + ggtitle("Number of seeds by size for Veronica_alpina") + xlab("log2(size)") + ylab("Seed per individual") + scale_color_viridis_d()

Seeds_per_capsule_VA <- Seeds_per_capsule_VA %>% 
  select(mean_seeds) %>% 
  unique()

Seeds_per_capsule_VA <- Seeds_per_capsule_VA$mean_seeds
  
#### Seedling establishment coefficients ####
#This section calculate the seedling establishment rate for each species in the warmed and unwarmed plots (using the data from the vegetated plots further in the analysis)

#seedling_est <- read.csv2("data/Demography/INCLINE_seedling_data.csv") 

seedling_est1 <- seedling_est %>% 
  filter(Year == 2020) %>% 
  select(Site, Block, Plot, PlotID, Date, Species, ID, Vegetation, Present) %>% 
  mutate(Date = dmy(Date)) %>% 
  mutate(campaign_number = if_else(Date == "2020-06-29" | Date == "2020-06-30" | Date == "2020-07-13" | Date == "2020-07-14" | Date == "2020-07-17", "first",
                                   if_else(Date == "2020-08-05" | Date == "2020-08-07" | Date == "2020-08-12" | Date == "2020-08-13" | Date == "2020-08-14", "second",
                                         if_else(Date == "2020-08-26" | Date == "2020-08-24" | Date == "2020-08-25" | Date == "2020-08-27", "third", "NA")))) %>% 
  mutate(Site = case_when(Site == "LAV" ~ "Lav",
                          Site == "ULV" ~ "Ulv",
                          Site == "GUD" ~ "Gud",
                          Site == "SKJ" ~ "Skj")) %>% 
  mutate(plotID = paste0(Site, "_", Block, "_", Plot)) %>% 
  select(-PlotID)

seedling_est_background <- seedling_est1 %>% 
  filter(!Plot %in% c("C", "OTC")) %>% 
  filter(campaign_number == "second") %>% 
  filter(Present == "yes") %>% 
  left_join(INCLINE_metadata, by = "plotID") %>% 
  group_by(Species, Site, OTC, treatment, plotID) %>% 
  summarise(n()) %>%
  rename(number = "n()") %>% 
  ungroup() %>% 
  group_by(Species, OTC, treatment) %>% 
  mutate(seedling_est_rate = mean(number)) #Need to make a decision for how to incorporate background germination - NOT DONE

seedling_est <- seedling_est1 %>% 
  filter(Plot %in% c("C", "OTC")) %>% 
  rename(Warming = Plot)

###### Veronica alpina ######

seedling_est_VA <- seedling_est %>% 
  filter(Species == "Ver_alp") %>% 
  filter(campaign_number == "second") %>% 
  group_by(Site, Block, Warming, plotID, Vegetation) %>% 
  mutate(count = case_when(Present == "yes" ~ 1,
                           Present == "no" ~ 0)) %>% 
  mutate(total_germinated = sum(count)) %>% 
  ungroup() %>% 
  mutate(total_seeds = 20) %>% 
  mutate(germination_percentage = total_germinated/total_seeds) %>% 
  select(-ID) %>% 
  unique() %>% 
  mutate(Vegetation = case_when(Vegetation == "yes" ~ "Veg",
                                Vegetation == "no" ~ "NoVeg")) %>% 
  mutate(Treatment = paste0(Warming, "_", Vegetation)) %>% 
  mutate(blockID = paste0(Site, "_", Block))

model1 <- lmer(germination_percentage ~ Warming + Vegetation +(1|Site) + (1|blockID), data = seedling_est_VA)
summary(model1)

seedling_est_VA %>% ggplot(aes(x = Warming, y = germination_percentage, fill = Warming)) + geom_violin() + geom_jitter(alpha = 0.5, width = 0.10) + facet_grid(~Vegetation) + theme_bw() + ggtitle("Germination success in different treatments for Veronica alpina") + scale_fill_manual(values = c("lightblue", "darkred"))

seedling_est_VA <- seedling_est_VA%>% 
  select(Vegetation, Warming, germination_percentage) %>% 
  group_by(Vegetation, Warming) %>% 
  mutate(germination_percentage = mean(germination_percentage)) %>% 
  unique()

# Need to make this in a format that can be added in the model later

# seedling_est_VA_NoVeg <- seedling_est_VA %>% 
#   filter(Vegetation == "NoVeg")
# 
# seedling_est_VA_NoVeg <- seedling_est_VA_NoVeg$germination_percentage
# 
# seedling_est_VA_Veg <- seedling_est_VA %>% 
#   filter(Vegetation == "Veg")
# 
# seedling_est_VA_Veg <- seedling_est_VA_Veg$germination_percentage
  
###### Sibbaldia procumbens ######
seedling_est_SP <- seedling_est %>% 
  filter(Species == "Sib_pro") %>% 
  filter(campaign_number == "second") %>%
  group_by(Site, Block, Warming, plotID, Vegetation) %>% 
  mutate(count = case_when(Present == "yes" ~ 1,
                           Present == "no" ~ 0)) %>% 
  mutate(total_germinated = sum(count)) %>% 
  ungroup() %>% 
  mutate(total_seeds = 20) %>% 
  mutate(germination_percentage = total_germinated/total_seeds) %>% 
  select(-ID) %>% 
  unique() %>% 
  mutate(Vegetation = case_when(Vegetation == "yes" ~ "Veg",
                                Vegetation == "no" ~ "NoVeg")) %>% 
  mutate(Treatment = paste0(Warming, "_", Vegetation)) %>% 
  #mutate(blockID = paste0(Site, "_", Block)) #Tested with the blockID as random effect, but it is not able to pick up anything on that - still det same results as if it was just site, so removed blockID from the model

model2 <- lmer(germination_percentage ~ Warming + Vegetation + (1|Site), data = seedling_est_SP)
summary(model2)

seedling_est_SP %>% ggplot(aes(x = Warming, y = germination_percentage, fill = Warming)) + geom_violin() + geom_jitter(alpha = 0.5, width = 0.10) + facet_grid(~Vegetation) + theme_bw() + ggtitle("Germination success in different treatments for Sibbaldia procumbens") + scale_fill_manual(values = c("lightblue", "darkred"))

seedling_est_SP <- seedling_est_SP%>% 
  select(Vegetation, germination_percentage) %>% 
  group_by(Vegetation) %>% 
  mutate(germination_percentage = mean(germination_percentage)) %>% 
  unique()

# Need to make this in a format that can be added in the model later

# seedling_est_SP_W <- seedling_est_SP %>% 
#   filter(Warming == "OTC")
# 
# seedling_est_SP_W <- seedling_est_SP_W$germination_percentage
# 
# seedling_est_SP_C <- seedling_est_SP %>% 
#   filter(Warming == "C")
# 
# seedling_est_SP_C <- seedling_est_SP_C$germination_percentage

#### Making seedling information ####
#This section calculates the average seedlings size of each species, and adding in the seedling establishment rate in the same dataset to have all seedling data together

###### Sibaldia procumbens ######

Seedling_info_SP <- Sib_pro %>% 
  filter(seedling == "yes") %>%  #There are some seedlings that lack information in LSL, NL or LL. Do we need to gap fill the data here?
  group_by(OTC, treatment) %>% 
  mutate(mean_LSL = round(mean(LSL, na.rm = TRUE)),
         mean_NL = round(mean(NL, na.rm = TRUE)),
         mean_LL = round(mean(LL, na.rm = TRUE))) %>%
  mutate(LSL = case_when(LSL == 0 ~ mean_LSL,   #Filling in gaps with the mean for each treatments - might have to attack this differently later
                         LSL > 0 ~ LSL)) %>% 
  mutate(NL = case_when(NL == 0 ~ mean_NL,
                        NL > 0 ~ NL)) %>% 
  mutate(LL = case_when(LL == 0 ~ mean_LL,
                        LL > 0 ~ LL)) %>%
  left_join(Sib_pro_coef, by = "siteID") %>% 
  mutate(size = Intercept + LSL * LSL_coef + NL * NL_coef + LL * LL_coef) #Making biomass estimate with intercept and coefficients from biomass regression

model1_seedling <- lmer(size ~ OTC + treatment + (1|siteID/plotID), data = Seedling_info_SP)
summary(model1_seedling) #Seedlings grow larger in extant and novel transplant, but smaller in removal transplant

model_seedling <- lmer(size ~ treatment + (1|siteID/plotID), data = Seedling_info_SP)
summary(model_seedling) #Seedlings grow larger in extant and novel transplant, but smaller in removal transplant

Seedling_info_SP %>%  ggplot(aes(x = treatment, y = size)) +  geom_jitter(alpha= 0.2) + geom_violin(aes(fill = treatment, alpha = 0.5), draw_quantiles = c(0.25, 0.5, 0.75)) + facet_wrap(OTC~siteID, nrow = 2) + ggtitle("Seedling size by treatment for Sibbaldia procumbens") + ylab("size") + scale_fill_viridis_d() + theme_bw()

Seedling_info_SP <- Seedling_info_SP %>%
  ungroup() %>% 
  mutate(seeds_cap = mean(size, na.rm = TRUE),
         seeds_cap_sd = sd(size, na.rm = TRUE)) %>%
  # mutate(seedling_establishment_rate = if_else(OTC == "C", seedling_est_SP_C,
  #                                              if_else(OTC == "W", seedling_est_SP_W, 0))) %>% 
  select(seeds_cap, seeds_cap_sd) %>% 
  distinct()

###### Veronica alpina ######

Seedling_info_VA <- Ver_alp %>% 
  filter(seedling == "yes") %>% 
  group_by(OTC, treatment) %>% 
  mutate(mean_SH = round(mean(SH, na.rm = TRUE)),
         mean_NL = round(mean(NL, na.rm = TRUE)),
         mean_LL = round(mean(LL, na.rm = TRUE)),
         mean_WL = round(mean(WL, na.rm = TRUE))) %>%
  mutate(LSL = case_when(SH == 0 ~ mean_SH,   #Filling in gaps with the mean for each treatments - might have to attack this differently later
                         SH > 0 ~ SH)) %>% 
  mutate(NL = case_when(NL == 0 ~ mean_NL,
                        NL > 0 ~ NL)) %>% 
  mutate(LL = case_when(LL == 0 ~ mean_LL,
                        LL > 0 ~ LL)) %>% 
  mutate(LL = case_when(WL == 0 ~ mean_WL,
                        WL > 0 ~ WL)) %>% 
  add_column(Ver_alp_coef) %>%
  mutate(size = Intercept + SH * SH_coef + NL * NL_coef + LL * LL_coef + WL * WL_coef) #Making biomass estimate with intercept and coefficients from biomass regression

model_seedling_VA <- lmer(size ~ treatment + (1|siteID/blockID/plotID), data = Seedling_info_VA)
summary(model_seedling_VA)

Seedling_info_VA %>%  ggplot(aes(x = treatment, y = size)) +  geom_jitter(alpha= 0.2) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + ggtitle("Seedling size by treatment for Veronica alpina") + ylab("size") + scale_fill_manual(values = c("lightblue", "darkred")) + theme_bw()

Seedling_info_VA <- Seedling_info_VA %>% 
  group_by(treatment) %>% 
  mutate(seeds_cap = mean(size, na.rm = TRUE),
         seeds_cap_sd = sd(size, na.rm = TRUE)) %>% 
  ungroup() %>% 
  # mutate(seedling_establishment_rate = if_else(treatment == "C" | treatment == "E" | treatment == "N", seedling_est_VA_Veg,
  #                                              if_else(treatment == "R", seedling_est_VA_NoVeg, 0))) %>% 
  select(treatment, seeds_cap, seeds_cap_sd) %>% 
  distinct()

#### Making transitions ####
#This section calculates the size of individuals, estimates of seed number. And cleaning the data so that we have the correct variables, and variable names for the analysis.

# Clone function to match new individuals with parents and calculate the distance between child and parent

# For testing the function:
# child <- Sib_pro_2018_2019 %>% 
#   filter(is.na(size) & sizeNext > 0) %>% 
#   filter(plotID == "Gud_2_4") 
# 
# parent <- Sib_pro_2018_2019 %>% 
#   filter(plotID == "Gud_2_4") %>% 
#   filter(seedling == "no", juvenile == "no") 

clone_function <- function(child, parent){
  
  child <- child %>% 
    select(unique_IDS, X_next, Y_next, sizeNext) %>% 
    rename(unique_IDS_child = unique_IDS)
  
  child2 <- child %>% 
    select(X_next, Y_next) %>% 
    rename(X = X_next, Y = Y_next) %>% 
    filter(!is.na(X) & !is.na(Y))
  
  parent <- parent %>% 
    select(unique_IDS, X, Y, size) %>% 
    rename(unique_IDS_parent = unique_IDS, size_parent = size) %>% 
    rename(X_parent = X, Y_parent = Y)
  
  parent2 <- parent %>% 
    rename(X = X_parent, Y = Y_parent) %>% 
    select(X, Y)
    
  
  
  if(nrow(child) == 0){
    return(child %>% 
             mutate(distance_parent = NA) %>% 
             bind_cols(parent[0,]))
  }
  
  if(nrow(parent) == 0){
    
    parent <- parent[1:nrow(child),]
    
    return(child %>% 
             mutate(distance_parent = NA) %>% 
             bind_cols(add_row(parent[0,])))
  }
  
  
  dis_matrix <- bind_rows(child2, parent2) %>%
    dist() %>%  #calculates distance between child and potential mother
    as.matrix()
  
  dis_matrix <- dis_matrix[-(1:nrow(child2)), 1:nrow(child2), drop = FALSE]
  
  matched <- dis_matrix %>% 
    apply(MARGIN = 2, which.min) #finds the right parent to the right child
  
  child %>% 
    mutate(distance_parent = apply(dis_matrix, MARGIN = 2, min)) %>% 
    #rename(X_next = X, Y_next = Y) %>% #Need to change names so that the names don't crash
    bind_cols(parent[matched, ]) #combine data set of children and mothers
  
}



###### Sibaldia procumbens ######

Sib_pro_2018 <- Sib_pro %>% 
  filter(year == 2018) %>% 
  select(siteID, blockID, plotID, unique_IDS, X, Y, MS, OTC, treatment, year, LSL, NL, LL, NFL, NB, NC, NAC, seedling, juvenile)

Sib_pro_2019 <- Sib_pro %>% 
  filter(year == 2019) %>% 
   select(siteID, blockID, plotID, unique_IDS, X, Y, MS, OTC, treatment, year, LSL, NL, LL, NFL, NB, NC, NAC, seedling, juvenile)

Sib_pro_2020 <- Sib_pro %>% 
  filter(year == 2020) %>% 
  select(siteID, blockID, plotID, unique_IDS, X, Y, MS, OTC, treatment, year, LSL, NL, LL, NFL, NB, NC, NAC, seedling, juvenile) 

Sib_pro_2021 <- Sib_pro %>% 
  filter(year == 2021) %>% 
  select(siteID, blockID, plotID, unique_IDS, X, Y, MS, OTC, treatment, year, LSL, NL, LL, NFL, NB, NC, NAC, seedling, juvenile)


#2018-2019 transition
Sib_pro_2018_2019 <- Sib_pro_2018 %>% 
  full_join(Sib_pro_2019, by = c("unique_IDS", "plotID", "OTC", "treatment", "siteID", "blockID"), suffix = c("_2018", "_2019")) %>%
  rename(X = X_2018, Y = Y_2018, X_next = X_2019, Y_next = Y_2019, seedling = seedling_2018, juvenile = juvenile_2018, seedling_next = seedling_2019, juvenile_next = juvenile_2019, MS = MS_2018, MS_next = MS_2019) %>% 
  left_join(Sib_pro_coef, by = "siteID") %>% 
  mutate(size = Intercept + (NL_2018 * NL_coef) + (LL_2018 * LL_coef),
         sizeNext = Intercept + (NL_2019 * NL_coef) + (LL_2019 * LL_coef),
         fec = (Seeds_per_capsule_SP * NFL_2018) + (Seeds_per_capsule_SP * NB_2018) + (Seeds_per_capsule_SP * NC_2018),
         surv = ifelse(size > 0 & is.na(sizeNext), 0,
                       ifelse(size > 0 & sizeNext > 0, 1, NA))) %>% 
  mutate(flo.no = rowSums(dplyr::select(., NB_2018, NFL_2018, NC_2018), na.rm=TRUE),
        flo.if = ifelse(flo.no > 0, 1, 0)) %>%
  mutate(offspringNext = ifelse(seedling_next == "yes" & is.na(size), "sexual",
                              ifelse(juvenile_next == "yes" & is.na(size), "sexual",
                                     ifelse(is.na(size) & sizeNext>0, "clone", NA)))) %>%
## Make clonal information (clo.if, clo.no and transfer the size of the mother to size)
  select(siteID, blockID, plotID, unique_IDS, X, Y, X_next, Y_next, OTC, treatment, size, sizeNext, fec, surv, flo.no, flo.if, offspringNext, seedling, juvenile, seedling_next, juvenile_next, MS, MS_next) %>%
  mutate(transition = "2018-2019")

#2019-2020 transition
Sib_pro_2019_2020 <- Sib_pro_2019 %>% 
  full_join(Sib_pro_2020, by = c("unique_IDS", "plotID", "OTC", "treatment", "siteID", "blockID"), suffix = c("_2019", "_2020")) %>% 
  rename(X = X_2019, Y = Y_2019, X_next = X_2020, Y_next = Y_2020, seedling = seedling_2019, juvenile = juvenile_2019, seedling_next = seedling_2020, juvenile_next = juvenile_2020, MS = MS_2019, MS_next = MS_2020) %>% 
  left_join(Sib_pro_coef, by = "siteID") %>% 
  mutate(size = Intercept + NL_2019 * NL_coef + LL_2019 * LL_coef,
         sizeNext = Intercept + NL_2020 * NL_coef + LL_2020 * LL_coef,
         fec = (Seeds_per_capsule_SP * NFL_2019) + (Seeds_per_capsule_SP * NB_2019) + (Seeds_per_capsule_SP * NC_2019), 
         surv = ifelse(size > 0 & is.na(sizeNext), 0,
                       ifelse(size > 0 & sizeNext > 0, 1, NA))) %>% 
  mutate(flo.no = rowSums(dplyr::select(., NB_2019, NFL_2019, NC_2019), na.rm=TRUE),
         flo.if = ifelse(flo.no > 0, 1, 0)) %>%
  mutate(offspringNext = ifelse(seedling_next == "yes" & is.na(size), "sexual",
                                ifelse(juvenile_next == "yes" & is.na(size), "sexual",
                                       ifelse(is.na(size) & sizeNext>0, "clone", NA)))) %>%
  ## Make clonal information (clo.if, clo.no and transfer the size of the mother to size)
  select(siteID, blockID, plotID, unique_IDS, X, Y, X_next, Y_next, OTC, treatment, size, sizeNext, fec, surv, flo.no, flo.if, offspringNext, seedling, juvenile, seedling_next, juvenile_next, MS, MS_next) %>%
  mutate(transition = "2019-2020")

#2020-2021 transition
Sib_pro_2020_2021 <- Sib_pro_2020 %>% 
  full_join(Sib_pro_2021, by = c("unique_IDS", "plotID", "OTC", "treatment", "siteID", "blockID"), suffix = c("_2020", "_2021")) %>% 
  rename(X = X_2020, Y = Y_2020, X_next = X_2021, Y_next = Y_2021, seedling = seedling_2020, juvenile = juvenile_2020, seedling_next = seedling_2021, juvenile_next = juvenile_2021, MS = MS_2020, MS_next = MS_2021) %>% 
  left_join(Sib_pro_coef, by = "siteID") %>% 
  mutate(size = Intercept + NL_2020 * NL_coef + LL_2020 * LL_coef,
         sizeNext = Intercept + NL_2021 * NL_coef + LL_2021 * LL_coef,
         fec = (Seeds_per_capsule_SP * NFL_2020) + (Seeds_per_capsule_SP * NB_2020) + (Seeds_per_capsule_SP * NC_2020), 
         surv = ifelse(size > 0 & is.na(sizeNext), 0,
                       ifelse(size > 0 & sizeNext > 0, 1, NA))) %>% 
  mutate(flo.no = rowSums(dplyr::select(., NB_2020, NFL_2020, NC_2020), na.rm=TRUE),
         flo.if = ifelse(flo.no > 0, 1, 0)) %>%
  mutate(offspringNext = ifelse(seedling_next == "yes" & is.na(size), "sexual",
                                ifelse(juvenile_next == "yes" & is.na(size), "sexual",
                                       ifelse(is.na(size) & sizeNext>0, "clone", NA)))) %>% 
  ## Make clonal information (clo.if, clo.no and transfer the size of the mother to size)
  select(siteID, blockID, plotID, unique_IDS, X, Y, X_next, Y_next, OTC, treatment, size, sizeNext, fec, surv, flo.no, flo.if, offspringNext, seedling, juvenile, seedling_next, juvenile_next, MS, MS_next) %>% 
  mutate(transition = "2020-2021")


#Combining all transitions together
Sib_pro_2018_2021 <- bind_rows(Sib_pro_2018_2019, Sib_pro_2019_2020, Sib_pro_2020_2021)

#adding clonal information
clones_SP <- Sib_pro_2018_2021 %>% 
  group_by(transition, plotID) %>% 
  nest() %>% 
  mutate(clonal_information = map(data, ~ {
    child <- .x %>% 
      filter(is.na(size) & sizeNext > 0) %>% 
      filter(offspringNext == "clone")
    
    parent <- .x %>% 
      filter(seedling == "no", juvenile == "no") %>% 
      select(unique_IDS, X, Y, size) %>% 
      filter(size > (Seedling_info_SP$seeds_cap + 2*Seedling_info_SP$seeds_cap_sd))
    
    
    clone_function(child, parent)
  }))

clonal_information_SP <- clones_SP %>%
  unnest(cols = clonal_information) %>% 
  select(-data)

Sib_pro_2018_2021 <- clones_SP %>% 
  select(-clonal_information) %>% 
  unnest(data)

Sib_pro_2018_2021 <- Sib_pro_2018_2021 %>% 
  left_join(clonal_information_SP, by = c("plotID", "transition", "unique_IDS" = "unique_IDS_child", "X_next", "Y_next", "sizeNext"))

Sib_pro_test <- Sib_pro_2018_2021 %>% 
  mutate(size = case_when((offspringNext == "clone" & distance_parent < 10) ~ size_parent,
                          (offspringNext == "clone" & distance_parent > 10) ~ size,
                          offspringNext %in% c(NA, "sexual") ~ size))


#Some plots fro visualization/checking
Sib_pro_2018_2021 %>% ggplot(aes(y = sizeNext, x = size, color = flo.if)) + geom_point() + geom_abline()
Sib_pro_2018_2021 %>% filter(seedling == "yes") %>% ggplot(aes(y = sizeNext, x = size)) + geom_point() + geom_abline()
Sib_pro_2018_2021 %>% filter(offspringNext == "clone") %>% filter(distance_parent < 15) %>% ggplot(aes(y = sizeNext, x = size_parent, col = distance_parent)) + geom_point() + geom_abline()
Sib_pro_test %>% ggplot(aes(y = sizeNext, x = size, col = offspringNext, alpha = 0.5)) + geom_point() + geom_abline()
Sib_pro_test %>% ggplot(aes(x = sizeNext, fill = offspringNext, alpha = 0.5)) + geom_density()


##### Veronica alpina #####

Ver_alp_2018 <- Ver_alp %>% 
  filter(year == 2018) %>% 
  select(siteID, blockID, plotID, unique_IDS, X, Y, MS, OTC, treatment, year, SH, NL, LL, WL, NFL, NB, NC, NAC, seedling, juvenile)

Ver_alp_2019 <- Ver_alp %>% 
  filter(year == 2019) %>% 
  select(siteID, blockID, plotID, unique_IDS, X, Y, MS, OTC, treatment, year, SH, NL, LL, WL, NFL, NB, NC, NAC, seedling, juvenile)

Ver_alp_2020 <- Ver_alp %>% 
  filter(year == 2020) %>% 
  select(siteID, blockID, plotID, unique_IDS, X, Y, MS, OTC, treatment, year, SH, NL, LL, WL, NFL, NB, NC, NAC, seedling, juvenile)

Ver_alp_2021 <- Ver_alp %>% 
  filter(year == 2021) %>% 
  select(siteID, blockID, plotID, unique_IDS, X, Y, MS, OTC, treatment, year, SH, NL, LL, WL, NFL, NB, NC, NAC, seedling, juvenile)

#2018-2019 transition
Ver_alp_2018_2019 <- Ver_alp_2018 %>% 
  full_join(Ver_alp_2019, by = c("unique_IDS", "plotID", "OTC", "treatment", "siteID", "blockID"), suffix = c("_2018", "_2019")) %>% 
  rename(X = X_2018, Y = Y_2018, X_next = X_2019, Y_next = Y_2019, seedling = seedling_2018, juvenile = juvenile_2018, seedling_next = seedling_2019, juvenile_next = juvenile_2019, MS = MS_2018, MS_next = MS_2019) %>% 
  add_column(Ver_alp_coef) %>% 
  add_column(Seeds_per_capsule_VA_coef) %>% 
  mutate(size = Intercept + (SH_2018 * SH_coef) + (NL_2018 * NL_coef) + (LL_2018 * LL_coef) + (WL_2018 * WL_coef), 
         sizeNext = Intercept + (SH_2019 * SH_coef) + (NL_2019 * NL_coef) + (LL_2019 * LL_coef) + (WL_2019 * WL_coef),
         surv = ifelse(size > 0 & is.na(sizeNext), 0,
                       ifelse(size > 0 & sizeNext > 0, 1, NA))) %>% 
  mutate(flo.no = rowSums(dplyr::select(., NB_2018, NFL_2018, NC_2018), na.rm=TRUE),
         flo.if = ifelse(flo.no > 0, 1, 0),
         fec = ((Intercept_seeds + size * size_seed)  * flo.no)) %>%
  mutate(offspringNext = ifelse(seedling_next == "yes" & is.na(size), "sexual",
                                ifelse(juvenile_next == "yes" & is.na(size), "sexual",
                                       ifelse(is.na(size) & sizeNext>0, "clone", NA)))) %>% 
  ## Make clonal information (clo.if, clo.no and transfer the size of the mother to size)
  select(siteID, blockID, plotID, unique_IDS, X, Y, X_next, Y_next, OTC, treatment, size, sizeNext, fec, surv, flo.no, flo.if, offspringNext, seedling, juvenile, seedling_next, juvenile_next, MS, MS_next) %>%
  mutate(transition = "2018-2019")

#2019-2020 transition
Ver_alp_2019_2020 <- Ver_alp_2019 %>% 
  full_join(Ver_alp_2020, by = c("unique_IDS", "plotID", "OTC", "treatment", "siteID", "blockID"), suffix = c("_2019", "_2020")) %>% 
  rename(X = X_2019, Y = Y_2019, X_next = X_2020, Y_next = Y_2020, seedling = seedling_2019, juvenile = juvenile_2019, seedling_next = seedling_2020, juvenile_next = juvenile_2020, MS = MS_2019, MS_next = MS_2020) %>% 
  add_column(Ver_alp_coef) %>% 
  add_column(Seeds_per_capsule_VA_coef) %>% 
  mutate(size = Intercept + (SH_2019 * SH_coef) + (NL_2019 * NL_coef) + (LL_2019 * LL_coef) + (WL_2019 * WL_coef), 
         sizeNext = Intercept + (SH_2020 * SH_coef) + (NL_2020 * NL_coef) + (LL_2020 * LL_coef) + (WL_2020 * WL_coef), 
         surv = ifelse(size > 0 & is.na(sizeNext), 0,
                       ifelse(size > 0 & sizeNext > 0, 1, NA))) %>% 
  mutate(flo.no = rowSums(dplyr::select(., NB_2019, NFL_2019, NC_2019), na.rm=TRUE),
         flo.if = ifelse(flo.no > 0, 1, 0),
         fec = ((Intercept_seeds + size * size_seed)  * flo.no)) %>%
  mutate(offspringNext = ifelse(seedling_next == "yes" & is.na(size), "sexual",
                                ifelse(juvenile_next == "yes" & is.na(size), "sexual",
                                       ifelse(is.na(size) & sizeNext>0, "clone", NA)))) %>% 
  ## Make clonal information (clo.if, clo.no and transfer the size of the mother to size)
  select(siteID, blockID, plotID, unique_IDS, X, Y, X_next, Y_next, OTC, treatment, size, sizeNext, fec, surv, flo.no, flo.if, offspringNext, seedling, juvenile, seedling_next, juvenile_next, MS, MS_next) %>%
  mutate(transition = "2019-2020")

#2020-2021 transition
Ver_alp_2020_2021 <- Ver_alp_2020 %>% 
  full_join(Ver_alp_2021, by = c("unique_IDS", "plotID", "OTC", "treatment", "siteID", "blockID"), suffix = c("_2020", "_2021")) %>% 
  rename(X = X_2020, Y = Y_2020, X_next = X_2021, Y_next = Y_2021, seedling = seedling_2020, juvenile = juvenile_2020, seedling_next = seedling_2021, juvenile_next = juvenile_2021, MS = MS_2020, MS_next = MS_2021) %>% 
  add_column(Ver_alp_coef) %>% 
  add_column(Seeds_per_capsule_VA_coef) %>% 
  mutate(size = Intercept + (SH_2020 * SH_coef) + (NL_2020 * NL_coef) + (LL_2020 * LL_coef) + (WL_2020 * WL_coef), 
         sizeNext = Intercept + (SH_2021 * SH_coef) + (NL_2021 * NL_coef) + (LL_2021 * LL_coef) + (WL_2021 * WL_coef), 
         surv = ifelse(size > 0 & is.na(sizeNext), 0,
                       ifelse(size > 0 & sizeNext > 0, 1, NA))) %>% 
  mutate(flo.no = rowSums(dplyr::select(., NB_2020, NFL_2020, NC_2020), na.rm=TRUE),
         flo.if = ifelse(flo.no > 0, 1, 0),
         fec = ((Intercept_seeds + size * size_seed)  * flo.no)) %>%
  mutate(offspringNext = ifelse(seedling_next == "yes" & is.na(size), "sexual",
                                ifelse(juvenile_next == "yes" & is.na(size), "sexual",
                                       ifelse(is.na(size) & sizeNext>0, "clone", NA)))) %>% 
  select(siteID, blockID, plotID, unique_IDS, X, Y, X_next, Y_next, OTC, treatment, size, sizeNext, fec, surv, flo.no, flo.if, offspringNext, seedling, juvenile, seedling_next, juvenile_next, MS, MS_next) %>% 
  mutate(transition = "2020-2021")


#Combining all transitions together
Ver_alp_2018_2021 <- bind_rows(Ver_alp_2018_2019, Ver_alp_2019_2020, Ver_alp_2020_2021)

#adding clonal information
clones_VA <- Ver_alp_2018_2021 %>% 
  group_by(transition, plotID) %>% 
  nest() %>% 
  mutate(clonal_information = map(data, ~ {
    child <- .x %>% 
      filter(is.na(size) & sizeNext > 0) %>% 
      filter(offspringNext == "clone")
    
    parent <- .x %>% 
      filter(seedling == "no", juvenile == "no") %>% 
      select(unique_IDS, X, Y, size) %>% 
      filter(size > (Seedling_info_SP$seeds_cap + 2*Seedling_info_SP$seeds_cap_sd))
    
    
    clone_function(child, parent)
  }))

clonal_information_VA <- clones_VA %>%
  unnest(cols = clonal_information) %>% 
  select(-data)

Ver_alp_2018_2021 <- clones_VA %>% 
  select(-clonal_information) %>% 
  unnest(data)

Ver_alp_2018_2021 <- Ver_alp_2018_2021 %>% 
  left_join(clonal_information_VA, by = c("plotID", "transition", "unique_IDS" = "unique_IDS_child", "X_next", "Y_next", "sizeNext"))

Ver_alp_test <- Ver_alp_2018_2021 %>% 
  mutate(size = case_when((offspringNext == "clone" & distance_parent < 10) ~ size_parent,
                          (offspringNext == "clone" & distance_parent > 10) ~ size,
                          offspringNext %in% c(NA, "sexual") ~ size))


#Some plots fro visualization/checking
Ver_alp_2018_2021 %>% ggplot(aes(y = sizeNext, x = size, color = flo.if)) + geom_point() + geom_abline()
Ver_alp_2018_2021 %>% filter(seedling == "yes") %>% ggplot(aes(y = sizeNext, x = size)) + geom_point() + geom_abline()
Ver_alp_2018_2021 %>% filter(offspringNext == "clone") %>% filter(distance_parent < 15) %>% ggplot(aes(y = sizeNext, x = size_parent, col = distance_parent)) + geom_point() + geom_abline()
Ver_alp_test %>% ggplot(aes(y = sizeNext, x = size, col = offspringNext, alpha = 0.5)) + geom_point() + geom_abline()
Ver_alp_test %>% ggplot(aes(x = sizeNext, fill = offspringNext, alpha = 0.5)) + geom_density()

