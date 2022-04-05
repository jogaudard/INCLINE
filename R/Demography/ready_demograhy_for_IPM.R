################################################################
### Script for making demography data ready for IPM analysis ###
################################################################

source("R/Demography/cleaning_demogprahy.R")

#### Libraries ####
library(tidyverse)
library(lme4)
library(lmerTest)
library(lubridate)
library(conflicted)

#### Select preferences for conflicts ####

conflict_prefer("select", "dplyr")
conflict_prefer("lmer", "lmerTest")


#### Downloading data from OSF ####

#osf_auth(token = "get from my document every time you run the code") 

# get_file(node = "zhk3m",
#          file = "Seeds_per_capsule.csv",
#          path = "data/Demography",
#          remote_path = "RawData/Demography")
# 
# get_file(node = "zhk3m",
#          file = "INCLINE_seedling_data.csv",
#          path = "data/Demography",
#          remote_path = "RawData/Germination_field_experiment")
# 
# get_file(node = "zhk3m",
#          file = "Biomass_Sib_pro.csv",
#          path = "data/Demography",
#          remote_path = "RawData/Demography")
# 
# get_file(node = "zhk3m",
#           file = "Seed_bank_survival.csv",
#           path = "data/Demography",
#           remote_path = "RawData/Demography")

#### Load data ####

Seeds_per_capsule <- read_csv2("data/Demography/Seeds_per_capsule.csv")
biomass_Sib_pro <- read_csv2("data/Demography/Biomass_Sib_pro.csv")
#biomass_Ver_alp_INCLINE <- read_csv2("data/Demography/SG.19_above-below_allocation.csv") #Not using this because some of the biomass rotted while collecting data, so biomass might not be correct
seedling_est <- read.csv2("data/Demography/INCLINE_seedling_data.csv") 
biomass_Ver_alp <- read_csv2("data/Demography/VeronicaAlpina_Biomass_Seedclim_edited.csv") #from SeedClim not on INCLINE OSF
seed_bank <- read_csv("data/Demography/Seed_bank_survival.csv") 


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
   filter(!is.na(siteID)) %>% 
  mutate(NL = round(NL/3))  #Had counted leaflets, but we want to count leaves. So it is divided by 3. Round to give integers, in case some leaves had two leaflets.

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

 # Ver_alp_biomass_regression <- lmer(ag ~ SH + NL + LL + WL + (1|siteID), data = biomass_Ver_alp)  #Not using this as it came with a singularity warning. Mixed effect model and linear model gives the same intercept and slopes for each variable.
 # summary(Ver_alp_biomass_regression)
# Ver_alp_biomass_regression_lm <- lm(ag ~ SH + NL + LL + WL + siteID, data = biomass_Ver_alp) #biomass is not significantly different between sites is not significantly different.
# summary(Ver_alp_biomass_regression_lm)

Ver_alp_biomass_regression_lm <- lm(ag ~ SH + NL + LL + WL , data = biomass_Ver_alp) 
summary(Ver_alp_biomass_regression_lm)

Ver_alp_coef <- coef(Ver_alp_biomass_regression_lm) %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  pivot_wider(names_from = "rowname", values_from = ".") %>% 
  rename(Intercept = "(Intercept)", SH_coef = SH, NL_coef = NL, LL_coef = LL, WL_coef = WL)


#### Seeds per capsules coefficients ####
#This section calculate the amount of seeds per capsule, and test if that is related to size of the mother or site. If it is related make regression and find coefficients for calculating it later (Veronica), if not, use the mean seed number per capsule (Sibbaldia).

###### Sibbaldia procumbens ######

Seeds_per_capsule_SP_dat <- Seeds_per_capsule %>% 
  filter(Species == "Sib_pro") %>% 
  mutate(mean_seeds = mean(Number_of_seeds, na.rm = TRUE)) %>% 
  mutate(Site = case_when(Site == "LAV" ~ "Lav",
                          Site == "ULV" ~ "Ulv",
                          Site == "GUD" ~ "Gud",
                          Site == "SKJ" ~ "Skj")) %>% 
  left_join(Sib_pro_coef, by = c("Site" = "siteID")) %>% 
  mutate(size = Intercept + Leaf_stock_length_mm*LSL_coef + Number_of_leaves*NL_coef + Leaf_length_mm* LL_coef) %>% 
  mutate(ID = paste0(Site, "_", Species, "_", Individual))

# seed_SP1 <- glmer(Number_of_seeds ~ size + Site + (1|Site) + (1|ID), data = Seeds_per_capsule_SP_dat, family = poisson(link = "log"))
# summary(seed_SP1) #Removed site as random effect because model complains about singularity
# 
# seed_SP2 <- glmer(Number_of_seeds ~ size + Site + (1|ID), data = Seeds_per_capsule_SP_dat, family = poisson(link = "log"))
# summary(seed_SP2) #Testing if seeds per capsule depends on size for each individual, or site - it does not. Although the model gives a warning that it fails to converge, but it looks good from inspection.
# 
# seed_SP3 <- glmer(Number_of_seeds ~ Number_of_capsules + (1|Site) + (1|ID), data = Seeds_per_capsule_SP_dat, family = poisson(link = "log")) #Testing if seeds per capsule depends on number of capsule for each individual, it does not.
# summary(seed_SP3)
# 
# seed_SP4 <- glmer(Number_of_seeds ~ Number_of_capsules + (1|ID), data = Seeds_per_capsule_SP_dat, family = poisson(link = "log")) #Testing if seeds per capsule depends on number of capsule for each individual, it does not.
# summary(seed_SP4)

seed_SP_null <- glmer(Number_of_seeds ~ 1 + (1|ID), data = Seeds_per_capsule_SP_dat, family = poisson(link = "log"))
summary(seed_SP_null)

Seeds_per_capsule_SP <- as.numeric(exp(fixef(seed_SP_null)))


Seeds_per_capsule_SP_dat %>%  
  ggplot(aes(x = size, y = Number_of_seeds)) + 
  geom_point(aes(color = Site)) + 
  geom_hline(aes(yintercept = Seeds_per_capsule_SP)) + 
  ggtitle("Number of seeds by size for Sibbaldia procumbens") + 
  xlab("log2(size)") +
  ylab("Seed per capsule") + scale_color_viridis_d()

###### Veronica alpina ######

Seeds_per_capsule_VA_dat <- Seeds_per_capsule %>% 
  filter(Species == "Ver_alp") %>% 
  mutate(mean_seeds = mean(Number_of_seeds, na.rm = TRUE)) %>%
  bind_cols(Ver_alp_coef) %>%
  mutate(size = Intercept + (Shoot_height_mm * SH_coef) + (Number_of_leaves * NL_coef) + (Leaf_length_mm * LL_coef) + (Leaf_width_mm * WL_coef)) %>%  #Making biomass estimate with intercept and coefficients from biomass regression
  mutate(ID = paste0(Site, "_", Species, "_", Individual)) 
# 
# seed_VA_1 <- glmer(Number_of_seeds ~ size * Site + (1|Site) + (1|ID), data = Seeds_per_capsule_VA_dat, family = poisson(link = "log")) #Model failed to converge, remove Site as random effect
# summary(seed_VA_1)
# 
# seed_VA_2 <- glmer(Number_of_seeds ~ size * Site + (1|ID), data = Seeds_per_capsule_VA_dat, family = poisson(link = "log")) #Failed to converge, remove the interacation between size and Site.
# summary(seed_VA_2)
# 
# seed_VA_3 <- glmer(Number_of_seeds ~ size + Site + (1|ID), data = Seeds_per_capsule_VA_dat, family = poisson(link = "log")) #Testing if seeds per capsule depends on biomass and site - biomass is significant (0.0386).
# summary(seed_VA_3)
# 
# seed_VA_4 <- glmer(Number_of_seeds ~ size + (1|ID), data = Seeds_per_capsule_VA_dat, family = poisson(link = "log")) #Testing if seeds per capsule depends on biomass alone, right above significant (0.065)
# summary(seed_VA_4)

seed_VA_null <- glmer(Number_of_seeds ~ 1 + (1|ID), data = Seeds_per_capsule_VA_dat, family = poisson(link = "log")) #Using the null model because the near significant effect of size in model 4 dissapeared when we excluded the small sized outlier individual, with a p-value for size going from 0.065 to 0.2. Null model is also the visually best fit for the data. See plot below
summary(seed_VA_null)

# seed_VA_5 <- glmer(Number_of_seeds ~ Number_of_capsules + (1|ID), data = Seeds_per_capsule_VA_dat, family = poisson(link = "log")) #Testing if seeds per capsule depends on number of capsule for each individual, it does not.
# summary(seed_VA_5)


Seeds_per_capsule_VA_null <- as.numeric(exp(fixef(seed_VA_null)))

Seeds_per_capsule_VA_dat %>%  
  ggplot(aes(x = size, y = Number_of_seeds)) + 
  geom_point(aes(color = Site)) + 
  geom_hline(aes(yintercept = Seeds_per_capsule_VA_null)) + 
  #geom_abline(intercept = Seeds_per_capsule_VA$Intercept_seeds, slope = Seeds_per_capsule_VA$seed_number_coef) +
  ggtitle("Number of seeds by size for Veronica_alpina") + 
  xlab("log2(size)") + 
  ylab("Seed per capsule") + 
  scale_color_viridis_d()

  
#### Seedling establishment coefficients ####
#This section calculate the seedling establishment rate for each species in the warmed and unwarmed plots (using the data from the vegetated plots further in the analysis)

#seedling_est <- read.csv2("data/Demography/INCLINE_seedling_data.csv") 

#Function needed to transform back after binomial model used on data
expit <- function(L) exp(L) / (1+exp(L)) 

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
  left_join(INCLINE_metadata, by = c("plotID")) %>%
  mutate(treatment = case_when(is.na(treatment) ~ Vegetation,
                               treatment == "N" ~ Vegetation,
                               treatment == "C" ~ "yes",
                               treatment == "R" ~ "no")) %>% 
  select(-Vegetation) %>% 
  rename(Vegetation = treatment)
  

seedling_est_background <- seedling_est1 %>% 
  filter(!Plot %in% c("C", "OTC")) %>% 
  filter(campaign_number == "second") %>% 
  mutate(Present = case_when(Present == "yes" ~ 1,
                             Present == "no" ~ 0)) %>% 
  group_by(Species, Site, OTC, Vegetation, plotID) %>% 
  mutate(germinated = sum(Present)) %>% 
  ungroup() %>% 
  group_by(Species, Vegetation) %>% 
  mutate(background_germination = mean(germinated)) %>% 
  select(Species, Vegetation, background_germination) %>% 
  unique()

seedling_est2 <- seedling_est1 %>% 
  filter(Plot %in% c("C", "OTC")) %>% 
  rename(Warming = Plot) 

binomial_seedlings <- seedling_est2 %>%
  select(Site, Block, Warming, PlotID, plotID, Species, Vegetation, campaign_number) %>%
  filter(campaign_number == "second") %>%
  unique() %>%
  slice(rep(1:n(), each = 20)) %>% 
  group_by(Site, Block, Warming, PlotID, plotID, Species, Vegetation, campaign_number) %>%
  mutate(ID = c(1:20)) %>% 
  mutate(ID = as.character(ID))

binomial_seedling_data <- seedling_est2 %>% 
  full_join(binomial_seedlings, by = c("Site", "Block", "Warming", "PlotID", "plotID", "Species", "Vegetation", "campaign_number", "ID")) %>% 
  mutate(Present = case_when(is.na(Present) ~ "no",
                             !is.na(Present) ~ Present)) %>% 
  left_join(seedling_est_background, by = c("Species", "Vegetation")) %>% 
  group_by(Site, Block, Warming, PlotID, plotID, Species, Vegetation, campaign_number, OTC) %>%
  fill(Date, .direction = "downup") 

###### Veronica alpina ######

seedling_est_bi_VA_dat <- binomial_seedling_data %>% 
  filter(Species == "Ver_alp") %>% 
  filter(campaign_number == "second") %>% 
  group_by(Site, Block, Warming, plotID, Vegetation) %>% 
  mutate(count = case_when(Present == "yes" ~ 1,
                           Present == "no" ~ 0)) %>% 
  mutate(total_germinated = sum(count)) %>% 
  ungroup() %>% 
  mutate(total_germinated = total_germinated - background_germination) %>% 
  mutate(Vegetation = case_when(Vegetation == "yes" ~ "Veg",
                                Vegetation == "no" ~ "NoVeg")) %>% 
  mutate(Treatment = paste0(Warming, "_", Vegetation)) %>% 
  mutate(blockID = paste0(Site, "_", Block)) 

# model_seedl_VA1 <- glmer(count ~ Warming * Vegetation +(1|Site) + (1|blockID), family = binomial, data = seedling_est_bi_VA_dat)
# summary(model_seedl_VA1) #Intercation between warming and vegetation is not significant, removing it

model_seedl_VA2 <- glmer(count ~ Warming + Vegetation +(1|Site) + (1|blockID), family = binomial, data = seedling_est_bi_VA_dat)
summary(model_seedl_VA2)

seedling_est_VA <- fixef(model_seedl_VA2) %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  pivot_wider(names_from = "rowname", values_from = ".") %>% 
  rename(Intercept = "(Intercept)", OTC = WarmingOTC, Veg = VegetationVeg)

# Need to make this in a format that can be added in the model later

seedling_est_VA_C_NoVeg <- expit(seedling_est_VA$Intercept)

seedling_est_VA_C_Veg <- expit(seedling_est_VA$Intercept + seedling_est_VA$Veg)

seedling_est_VA_OTC_NoVeg <- expit(seedling_est_VA$Intercept + seedling_est_VA$OTC)

seedling_est_VA_OTC_Veg <- expit(seedling_est_VA$Intercept + seedling_est_VA$OTC + seedling_est_VA$Veg)

  
###### Sibbaldia procumbens ######

seedling_est_bi_SP_dat <- binomial_seedling_data %>% 
  filter(Species == "Sib_pro") %>% 
  filter(campaign_number == "second") %>% 
  group_by(Site, Block, Warming, plotID, Vegetation) %>% 
  mutate(count = case_when(Present == "yes" ~ 1,
                           Present == "no" ~ 0)) %>% 
  mutate(total_germinated = sum(count)) %>% 
  ungroup() %>% 
  mutate(total_germinated = total_germinated - background_germination) %>% 
  mutate(Vegetation = case_when(Vegetation == "yes" ~ "Veg",
                                Vegetation == "no" ~ "NoVeg")) %>% 
  mutate(Treatment = paste0(Warming, "_", Vegetation)) %>% 
  mutate(blockID = paste0(Site, "_", Block)) 

# model_seedl_SP1 <- glmer(count ~ Warming * Vegetation +(1|Site) + (1|blockID), family = binomial, data = seedling_est_bi_SP_dat)
# summary(model_seedl_SP1) #Neither interaction nor warming is significant. Remove intercation in the first step.

# model_seedl_SP2 <- glmer(count ~ Warming + Vegetation +(1|Site) + (1|blockID), family = binomial, data = seedling_est_bi_SP_dat)
# summary(model_seedl_SP2) #Warming is not significant, remove it in the next step

model_seedl_SP3 <- glmer(count ~ Vegetation +(1|Site) + (1|blockID), family = binomial, data = seedling_est_bi_SP_dat)
summary(model_seedl_SP3)

seedling_est_SP <- fixef(model_seedl_SP3) %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  pivot_wider(names_from = "rowname", values_from = ".") %>% 
  rename(Intercept = "(Intercept)", Veg = VegetationVeg)

# Need to make this in a format that can be added in the model later

seedling_est_SP_NoVeg <- expit(seedling_est_SP$Intercept)

seedling_est_SP_Veg <- expit(seedling_est_VA$Intercept + seedling_est_VA$Veg)


#### Making seedling size information ####

#This section calculates the average seedlings size of each species, and adding in the seedling establishment rate in the same dataset to have all seedling data together

###### Sibaldia procumbens ######

Seedling_info_SP_dat <- Sib_pro %>% 
  filter(seedling == "yes") %>%   #There are some seedlings that lack information in LSL, NL or LL. Do we need to gap fill the data here?
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
  mutate(size = Intercept + LSL * LSL_coef + NL * NL_coef + LL * LL_coef) %>%  #Making biomass estimate with intercept and coefficients from biomass regression
  ungroup() %>% 
  mutate(max_seedling_size = max(size, na.rm = TRUE)) %>% 
  mutate(Vegetation = case_when(treatment %in% c("C", "E", "N") ~ "Veg",
                                treatment == "R" ~ "NoVeg"))

# model_seedling_SP1 <- lmer(size ~ OTC * treatment + (1|siteID/blockID/plotID), data = Seedling_info_SP_dat)
# summary(model_seedling_SP1) #Nothing was significant, removing the interaction to see if there is something on the single factors
# 
# model_seedling_SP2 <- lmer(size ~ OTC + treatment + (1|siteID/blockID/plotID), data = Seedling_info_SP_dat)
# summary(model_seedling_SP2)
# 
# model_seedling_SP3 <- lmer(size ~ treatment + (1|siteID/blockID/plotID), data = Seedling_info_SP_dat)
# summary(model_seedling_SP3) 
#
# model_seedling_SP4 <- lmer(size ~ OTC + (1|siteID/blockID/plotID), data = Seedling_info_SP_dat)
# summary(model_seedling_SP4) 

 model_seedling_SP5 <- lmer(size ~ Vegetation + (1|siteID/blockID/plotID), data = Seedling_info_SP_dat)
 summary(model_seedling_SP5) 

Seedling_info_SP_mean <- fixef(model_seedling_SP5)%>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  pivot_wider(names_from = "rowname", values_from = ".") %>% 
  rename(Intercept = "(Intercept)", Veg = VegetationVeg) 

mean_NoVeg_SP <- Seedling_info_SP_mean$Intercept

mean_Veg_SP <- Seedling_info_SP_mean$Intercept + Seedling_info_SP_mean$Veg

sd_SP <- arm::sigma.hat(model_seedling_SP5)$sigma$data #can we get different standard diviations for different groups? google this with lmer.

Seedling_info_SP <- as.data.frame(mean_NoVeg_SP) %>% 
  add_column(mean_Veg_SP) %>% 
  add_column(sd_SP) %>% 
  rename(mean_NoVeg = mean_NoVeg_SP, mean_Veg = mean_Veg_SP, sd = sd_SP)

SP_max_seedling_size <- Seedling_info_SP_dat %>% 
  select(max_seedling_size) %>% 
  unique()

Seedling_info_SP_dat %>%  
  ggplot(aes(x = Vegetation, y = size, fill = Vegetation)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_jitter(alpha= 0.2) +
  geom_hline(yintercept = mean_NoVeg_SP,  size = 2, color = "lightgreen") +
  geom_hline(yintercept = mean_Veg_SP, size = 2, color = "darkgreen") +
  ggtitle("Seedling size by treatment for Sibbaldia procumbens") + ylab("size") +
  scale_fill_manual(values = c("lightgreen", "darkgreen")) +
  theme_bw()

###### Veronica alpina ######

Seedling_info_VA_dat <- Ver_alp %>% 
  filter(seedling == "yes") %>% 
  group_by(OTC, treatment) %>% 
  mutate(mean_SH = round(mean(SH, na.rm = TRUE)),
         mean_NL = round(mean(NL, na.rm = TRUE)),
         mean_LL = round(mean(LL, na.rm = TRUE)),
         mean_WL = round(mean(WL, na.rm = TRUE))) %>%
  mutate(SH = case_when(SH == 0 ~ mean_SH,   #Filling in gaps with the mean for each treatments - might have to attack this differently later
                         SH > 0 ~ SH)) %>% 
  mutate(NL = case_when(NL == 0 ~ mean_NL,
                        NL > 0 ~ NL)) %>% 
  mutate(LL = case_when(LL == 0 ~ mean_LL,
                        LL > 0 ~ LL)) %>% 
  mutate(LL = case_when(WL == 0 ~ mean_WL,
                        WL > 0 ~ WL)) %>% 
  add_column(Ver_alp_coef) %>%
  mutate(size = Intercept + SH * SH_coef + NL * NL_coef + LL * LL_coef + WL * WL_coef) %>%  #Making biomass estimate with intercept and coefficients from biomass regression
  ungroup() %>% 
  mutate(max_seedling_size = max(size, na.rm = TRUE)) %>% 
  mutate(Vegetation = case_when(treatment %in% c("C", "E", "N") ~ "Veg",
                                treatment == "R" ~ "NoVeg"))

# model_seedling_VA1 <- lmer(size ~ OTC * treatment + (1|siteID/blockID/plotID), data = Seedling_info_VA_dat)
# summary(model_seedling_VA1)
# 
# model_seedling_VA2 <- lmer(size ~ OTC * treatment + (1|blockID/plotID), data = Seedling_info_VA_dat)
# summary(model_seedling_VA2)
# 
# model_seedling_VA3 <- lmer(size ~ OTC * treatment + (1|plotID), data = Seedling_info_VA_dat)
# summary(model_seedling_VA3)
# 
# model_seedling_VA4 <- lmer(size ~ OTC + treatment + (1|plotID), data = Seedling_info_VA_dat)
# summary(model_seedling_VA4)
# 
# model_seedling_VA5 <- lmer(size ~ treatment + (1|plotID), data = Seedling_info_VA_dat)
# summary(model_seedling_VA5)

model_seedling_VA6 <- lmer(size ~ Vegetation + (1|plotID), data = Seedling_info_VA_dat)
summary(model_seedling_VA6)

# model_seedling_VA7 <- lmer(size ~ OTC + (1|plotID), data = Seedling_info_VA_dat)
# summary(model_seedling_VA7)

Seedling_info_VA_mean <- fixef(model_seedling_VA6)%>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  pivot_wider(names_from = "rowname", values_from = ".") %>% 
  rename(Intercept = "(Intercept)", Veg = VegetationVeg) 

# Need to make this in a format that can be added in the model later

mean_NoVeg_VA <- Seedling_info_VA_mean$Intercept

mean_Veg_VA <- Seedling_info_VA_mean$Intercept + Seedling_info_VA_mean$Veg

sd_VA <- arm::sigma.hat(model_seedling_VA6)$sigma$data #can we get different standard diviations for different groups? google this with lmer.

Seedling_info_VA <- as.data.frame(mean_NoVeg_VA) %>% 
  add_column(mean_Veg_VA) %>% 
  add_column(sd_VA) %>% 
  rename(mean_NoVeg = mean_NoVeg_VA, mean_Veg = mean_Veg_VA, sd = sd_VA)


Seedling_info_VA_dat %>%  
  ggplot(aes(x = Vegetation, y = size, fill = Vegetation)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_jitter(alpha= 0.2) +
  geom_hline(yintercept = mean_NoVeg_VA,  size = 2, color = "lightgreen") +
  geom_hline(yintercept = mean_Veg_VA, size = 2, color = "darkgreen") +
  ggtitle("Seedling size by treatment for Veronica alpina") + ylab("size") +
  scale_fill_manual(values = c("lightgreen", "darkgreen")) +
  theme_bw()

VA_max_seedling_size <- Seedling_info_VA_dat %>% 
  select(max_seedling_size) %>% 
  unique()

#### Seed bank ####

# seed_bank1 <- seed_bank %>% 
#   mutate(seeds_dead_in_soil_bank = case_when("missing/dissentegrated" == "Yes", 1,
#                                              "missing/dissentegrated" == "No", 0))


#### Making transitions ####
#This section calculates the size of individuals, estimates of seed number. And cleaning the data so that we have the correct variables, and variable names for the analysis.

# Clone function to match new individuals with parents and calculate the distance between child and parent

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
        flo.if = ifelse(flo.no > 0, 1, 0),
        flo.no = case_when(flo.no == 0 ~ NA_real_,
                           TRUE ~ flo.no)) %>%
  #mutate(flo.no = case_when(is.na(flo.if) ~ NA, !is.na(flo.if) ~ flo.no))
  mutate(offspringNext = ifelse(seedling_next == "yes" & is.na(size), "sexual",
                              ifelse(juvenile_next == "yes" & is.na(size), "sexual",
                                     ifelse(is.na(size) & sizeNext>0, "clone", NA)))) %>% 
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
         flo.if = ifelse(flo.no > 0, 1, 0),
         flo.no = case_when(flo.no == 0 ~ NA_real_,
                            TRUE ~ flo.no)) %>%
  mutate(offspringNext = ifelse(seedling_next == "yes" & is.na(size), "sexual",
                                ifelse(juvenile_next == "yes" & is.na(size), "sexual",
                                       ifelse(is.na(size) & sizeNext>0, "clone", NA)))) %>%
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
         flo.if = ifelse(flo.no > 0, 1, 0),
         flo.no = case_when(flo.no == 0 ~ NA_real_,
                            TRUE ~ flo.no)) %>%
  mutate(offspringNext = ifelse(seedling_next == "yes" & is.na(size), "sexual",
                                ifelse(juvenile_next == "yes" & is.na(size), "sexual",
                                       ifelse(is.na(size) & sizeNext>0, "clone", NA)))) %>% 
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
      filter(size > (SP_max_seedling_size$max_seedling_size))
    
    
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

Sib_pro_2018_2021 <- Sib_pro_2018_2021 %>% 
  mutate(size = case_when((offspringNext == "clone" & distance_parent < 5) ~ size_parent,
                          (offspringNext == "clone" & distance_parent > 5) ~ size,
                          offspringNext %in% c(NA, "sexual") ~ size)) %>% 
  mutate(distance_parent = case_when(distance_parent > 5 ~ NA_real_,
                                     distance_parent < 5 ~ distance_parent,
                                     distance_parent == 5 ~ distance_parent,
                                     TRUE ~ NA_real_)) %>% 
  mutate(unique_IDS_parent = case_when(is.na(distance_parent) ~ NA_character_,
                                       TRUE ~ unique_IDS_parent)) %>% 
  mutate(X_parent = case_when(is.na(distance_parent) ~ NA_real_,
                                       TRUE ~ X_parent)) %>% 
  mutate(Y_parent = case_when(is.na(distance_parent) ~ NA_real_,
                                       TRUE ~ Y_parent)) %>% 
  mutate(size_parent = case_when(is.na(distance_parent) ~ NA_real_,
                              TRUE ~ size_parent)) 

clone_information_SP <- Sib_pro_2018_2021 %>% 
  select(plotID, transition, unique_IDS_parent) %>% 
  filter(!is.na(unique_IDS_parent)) %>%
  group_by(plotID, transition, unique_IDS_parent) %>%
  summarise(n()) %>%
  rename(clo.no = "n()")

Sib_pro_2018_2021 <- Sib_pro_2018_2021 %>% 
  left_join(clone_information_SP, by = c("plotID", "transition", "unique_IDS" = "unique_IDS_parent")) %>% 
  mutate(clo.if = case_when(clo.no > 0.1 ~ 1,
                            is.na(clo.no) ~ 0))


#Some plots fro visualization/checking
Sib_pro_2018_2021 %>% ggplot(aes(y = sizeNext, x = size, color = flo.if)) + geom_point() + geom_abline()
Sib_pro_2018_2021 %>% filter(seedling == "yes") %>% ggplot(aes(y = sizeNext, x = size)) + geom_point() + geom_abline()
Sib_pro_2018_2021 %>% filter(offspringNext == "clone") %>% ggplot(aes(y = sizeNext, x = size_parent, col = distance_parent)) + geom_point() + geom_abline()
Sib_pro_2018_2021 %>% ggplot(aes(y = sizeNext, x = size, col = offspringNext, alpha = 0.5)) + geom_point() + geom_abline()
Sib_pro_2018_2021 %>% ggplot(aes(x = sizeNext, fill = offspringNext, alpha = 0.5)) + geom_density()


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
  add_column(Seeds_per_capsule_VA_null) %>% 
  mutate(size = Intercept + (SH_2018 * SH_coef) + (NL_2018 * NL_coef) + (LL_2018 * LL_coef) + (WL_2018 * WL_coef), 
         sizeNext = Intercept + (SH_2019 * SH_coef) + (NL_2019 * NL_coef) + (LL_2019 * LL_coef) + (WL_2019 * WL_coef),
         surv = ifelse(size > 0 & is.na(sizeNext), 0,
                       ifelse(size > 0 & sizeNext > 0, 1, NA))) %>% 
  mutate(flo.no = rowSums(dplyr::select(., NB_2018, NFL_2018, NC_2018), na.rm=TRUE),
         flo.if = ifelse(flo.no > 0, 1, 0),
         flo.no = case_when(flo.no == 0 ~ NA_real_,
                            TRUE ~ flo.no),
         fec = Seeds_per_capsule_VA_null * flo.no) %>%
  mutate(offspringNext = ifelse(seedling_next == "yes" & is.na(size), "sexual",
                                ifelse(juvenile_next == "yes" & is.na(size), "sexual",
                                       ifelse(is.na(size) & sizeNext>0, "clone", NA)))) %>% 
  select(siteID, blockID, plotID, unique_IDS, X, Y, X_next, Y_next, OTC, treatment, size, sizeNext, fec, surv, flo.no, flo.if, offspringNext, seedling, juvenile, seedling_next, juvenile_next, MS, MS_next) %>%
  mutate(transition = "2018-2019")

#2019-2020 transition
Ver_alp_2019_2020 <- Ver_alp_2019 %>% 
  full_join(Ver_alp_2020, by = c("unique_IDS", "plotID", "OTC", "treatment", "siteID", "blockID"), suffix = c("_2019", "_2020")) %>% 
  rename(X = X_2019, Y = Y_2019, X_next = X_2020, Y_next = Y_2020, seedling = seedling_2019, juvenile = juvenile_2019, seedling_next = seedling_2020, juvenile_next = juvenile_2020, MS = MS_2019, MS_next = MS_2020) %>% 
  add_column(Ver_alp_coef) %>% 
  add_column(Seeds_per_capsule_VA_null) %>% 
  mutate(size = Intercept + (SH_2019 * SH_coef) + (NL_2019 * NL_coef) + (LL_2019 * LL_coef) + (WL_2019 * WL_coef), 
         sizeNext = Intercept + (SH_2020 * SH_coef) + (NL_2020 * NL_coef) + (LL_2020 * LL_coef) + (WL_2020 * WL_coef), 
         surv = ifelse(size > 0 & is.na(sizeNext), 0,
                       ifelse(size > 0 & sizeNext > 0, 1, NA))) %>% 
  mutate(flo.no = rowSums(dplyr::select(., NB_2019, NFL_2019, NC_2019), na.rm=TRUE),
         flo.if = ifelse(flo.no > 0, 1, 0),
         flo.no = case_when(flo.no == 0 ~ NA_real_,
                            TRUE ~ flo.no),
         fec = Seeds_per_capsule_VA_null * flo.no) %>%
  mutate(offspringNext = ifelse(seedling_next == "yes" & is.na(size), "sexual",
                                ifelse(juvenile_next == "yes" & is.na(size), "sexual",
                                       ifelse(is.na(size) & sizeNext>0, "clone", NA)))) %>% 
  select(siteID, blockID, plotID, unique_IDS, X, Y, X_next, Y_next, OTC, treatment, size, sizeNext, fec, surv, flo.no, flo.if, offspringNext, seedling, juvenile, seedling_next, juvenile_next, MS, MS_next) %>%
  mutate(transition = "2019-2020")

#2020-2021 transition
Ver_alp_2020_2021 <- Ver_alp_2020 %>% 
  full_join(Ver_alp_2021, by = c("unique_IDS", "plotID", "OTC", "treatment", "siteID", "blockID"), suffix = c("_2020", "_2021")) %>% 
  rename(X = X_2020, Y = Y_2020, X_next = X_2021, Y_next = Y_2021, seedling = seedling_2020, juvenile = juvenile_2020, seedling_next = seedling_2021, juvenile_next = juvenile_2021, MS = MS_2020, MS_next = MS_2021) %>% 
  add_column(Ver_alp_coef) %>% 
  add_column(Seeds_per_capsule_VA_null) %>% 
  mutate(size = Intercept + (SH_2020 * SH_coef) + (NL_2020 * NL_coef) + (LL_2020 * LL_coef) + (WL_2020 * WL_coef), 
         sizeNext = Intercept + (SH_2021 * SH_coef) + (NL_2021 * NL_coef) + (LL_2021 * LL_coef) + (WL_2021 * WL_coef), 
         surv = ifelse(size > 0 & is.na(sizeNext), 0,
                       ifelse(size > 0 & sizeNext > 0, 1, NA))) %>% 
  mutate(flo.no = rowSums(dplyr::select(., NB_2020, NFL_2020, NC_2020), na.rm=TRUE),
         flo.if = ifelse(flo.no > 0, 1, 0),
         flo.no = case_when(flo.no == 0 ~ NA_real_,
                            TRUE ~ flo.no),
         fec = Seeds_per_capsule_VA_null * flo.no) %>%
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
      filter(size > (VA_max_seedling_size$max_seedling_size))
    
    
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

Ver_alp_2018_2021 <- Ver_alp_2018_2021 %>% 
  mutate(size = case_when((offspringNext == "clone" & distance_parent < 5) ~ size_parent,
                          (offspringNext == "clone" & distance_parent > 5) ~ size,
                          offspringNext %in% c(NA, "sexual") ~ size)) %>% 
  mutate(distance_parent = case_when(distance_parent > 5 ~ NA_real_,
                                     distance_parent < 5 ~ distance_parent,
                                     distance_parent == 5 ~ distance_parent,
                                     TRUE ~ NA_real_)) %>% 
  mutate(unique_IDS_parent = case_when(is.na(distance_parent) ~ NA_character_,
                                       TRUE ~ unique_IDS_parent)) %>% 
  mutate(X_parent = case_when(is.na(distance_parent) ~ NA_real_,
                              TRUE ~ X_parent)) %>% 
  mutate(Y_parent = case_when(is.na(distance_parent) ~ NA_real_,
                              TRUE ~ Y_parent)) %>% 
  mutate(size_parent = case_when(is.na(distance_parent) ~ NA_real_,
                                 TRUE ~ size_parent))

clone_information_VA <- Ver_alp_2018_2021 %>% 
  select(plotID, transition, unique_IDS_parent) %>% 
  filter(!is.na(unique_IDS_parent)) %>%
  group_by(plotID, transition, unique_IDS_parent) %>%
  summarise(n()) %>%
  rename(clo.no = "n()")

Ver_alp_2018_2021 <- Ver_alp_2018_2021 %>% 
  left_join(clone_information_VA, by = c("plotID", "transition", "unique_IDS" = "unique_IDS_parent")) %>% 
  mutate(clo.if = case_when(clo.no > 0.1 ~ 1,
                            is.na(clo.no) ~ 0))


#Some plots fro visualization/checking
Ver_alp_2018_2021 %>% ggplot(aes(y = sizeNext, x = size, color = flo.if)) + geom_point() + geom_abline()
Ver_alp_2018_2021 %>% ggplot(aes(y = flo.no, x = size, color = transition)) + geom_point() + geom_abline()
Ver_alp_2018_2021 %>% ggplot(aes(y = fec, x = size, color = transition)) + geom_point() + geom_abline()
Ver_alp_2018_2021 %>% filter(seedling == "yes") %>% ggplot(aes(y = sizeNext, x = size)) + geom_point()
Ver_alp_2018_2021 %>% filter(offspringNext == "clone") %>% ggplot(aes(y = sizeNext, x = size_parent, col = distance_parent)) + geom_point() + geom_abline()
Ver_alp_2018_2021 %>% ggplot(aes(y = sizeNext, x = size, col = offspringNext, alpha = 0.5)) + geom_point() + geom_abline()
Ver_alp_2018_2021 %>% ggplot(aes(x = sizeNext, fill = offspringNext, alpha = 0.5)) + geom_density()
Ver_alp_2018_2021 %>% ggplot(aes(x = sizeNext, fill = as.factor(clo.if), alpha = 0.5)) + geom_density()

