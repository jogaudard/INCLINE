################################################################
### Script for cleaning raw data from germination experiment ###
################################################################

#### Libraries ####
library(tidyverse)
library(dataDownloader)
library(osfr)
library(lubridate)
library(germinationmetrics)

#### Downloading data from OSF ####

#osf_auth(token = "get from my document every time you run the code") 

get_file(node = "zhk3m",
         file = "INCLINE_Germination_Seedling_Experiment_Data_Va.csv",
         path = "data/Germination",
         remote_path = "RawData/Germination_lab_experiment")

get_file(node = "zhk3m",
         file = "INCLINE_Germination_Seedling_Experiment_Data_SP.csv",
         path = "data/Germination",
         remote_path = "RawData/Germination_lab_experiment")

get_file(node = "zhk3m",
         file = "comment_dictionary_VA.csv",
         path = "data/Germination",
         remote_path = "RawData/Germination_lab_experiment")

get_file(node = "zhk3m",
         file = "comment_dictionary_SP.csv",
         path = "data/Germination",
         remote_path = "RawData/Germination_lab_experiment")

get_file(node = "zhk3m",
         file = "harvest_comment_dictionary_VA.csv",
         path = "data/Germination",
         remote_path = "RawData/Germination_lab_experiment")

get_file(node = "zhk3m",
         file = "harvest_comment_dictionary_SP.csv",
         path = "data/Germination",
         remote_path = "RawData/Germination_lab_experiment")

get_file(node = "zhk3m",
         file = "dish_comment_dictionary_VA.csv",
         path = "data/Germination",
         remote_path = "RawData/Germination_lab_experiment")

get_file(node = "zhk3m",
         file = "weighing_comment_dictionary_SP.csv",
         path = "data/Germination",
         remote_path = "RawData/Germination_lab_experiment")

get_file(node = "zhk3m",
         file = "INCLINE_metadata_LoggerDates.csv",
         path = "data",
         remote_path = "RawData")

#### Load data ####

Sib_pro_germ <- read.delim("data/Germination/INCLINE_Germination_Seedling_Experiment_Data_SP.csv", sep = ";", dec = ".")
Ver_alp_germ <- read_delim("data/Germination/INCLINE_Germination_Seedling_Experiment_Data_Va.csv")
INCLINE_metadata <- read_delim("data/INCLINE_metadata_LoggerDates.csv", delim = ";")
comment_dict_VA <- read_delim("data/Germination/comment_dictionary_VA.csv", delim = ";")
harvest_comment_dict_VA <- read_delim("data/Germination/harvest_comment_dictionary_VA.csv", delim = ";")
dish_comment_dict_VA <- read_delim("data/Germination/dish_comment_dictionary_VA.csv", delim = ";")
comment_dict_SP <- read_delim("data/Germination/comment_dictionary_SP.csv", delim = ";")
harvest_comment_dict_SP <- read_delim("data/Germination/harvest_comment_dictionary_SP.csv", delim = ";")
weighing_comment_dict_SP <- read_delim("data/Germination/weighing_comment_dictionary_SP.csv", delim = ";")

##### Veronica alpina #####

#### Fixing mistakes in dataset ####

Ver_alp_germ <- Ver_alp_germ %>%
  mutate(Start_date = case_when(unique_ID == "VA_LAV_5_6_18" ~ "18022020",
                                TRUE ~ as.character(Start_date))) %>% #One individual that was missing information in the start date column, manually found the right date and entered it here
  mutate(petri_dish = paste(Species, Site, Water_potential, Replicate, sep = "_")) %>% 
  filter(!petri_dish %in% c("VA_GUD_6_7X", "VA_LAV_9_9X")) #These are extra petri dishes that do not have any data - so I do not think there actually were extra petri dishes, removing them.


# Make new variables

Ver_alp_germ <- Ver_alp_germ %>%
  mutate(Start_date = dmy(Start_date),
         Germination_date = dmy(Germination_date),
         Cotelydon_date = dmy(Cotelydon_date),
         Leaf_date = dmy(Leaf_date),
         Harvest_date = dmy(Harvest_date),
         dead_date = dmy(dead_date)) %>%
  mutate(days_to_germination = Germination_date - Start_date,
         days_to_cotelydon = Cotelydon_date - Start_date,
         days_to_leaf = Leaf_date - Start_date) %>% 
  mutate(site_WP = paste(Site, Water_potential),
         petri_dish = paste(Species, Site, Water_potential, Replicate, sep = "_")) %>% 
  mutate(Water_potential = as.factor(Water_potential)) %>% 
  group_by(Species, Site, Water_potential, Replicate) %>% 
  mutate(seeds_in_dish = n())  %>% 
  mutate(Dry_mass_g_total = case_when(!is.na(Dry_mass_g_total) ~ Dry_mass_g_total,
                                      is.na(Dry_mass_g_total) ~ Dry_mass_g_root + Dry_mass_g_above_ground)) %>% 
  mutate(root_shoot_ratio = Dry_mass_g_root/Dry_mass_g_above_ground) %>% 
  rename(species = Species, siteID = Site, water_potential = Water_potential, replicate = Replicate, seed_nr = Seed, start_date = Start_date, germination_date = Germination_date, comment = Comment, cotelydon_date = Cotelydon_date, leaf_date = Leaf_date, harvest_date = Harvest_date, harvest_comment = Harvest_comment, wet_mass_g_root = Wet_mass_g_root, wet_mass_g = Wet_mass_g_, dry_mass_g_root = Dry_mass_g_root, dry_mass_g_above_ground = Dry_mass_g_above_ground, dry_mass_g_total = Dry_mass_g_total, weighing_comments = Weighing_comments, notes = Notes, removel_whole_petridish = Remove_whole_petridish, seed_viable = Seed_viable, lights_off = "Lights_off (Yes/no)", flag = Flag)

#### Deal with comments. Categorize them ####
## Entering information in flag columns from comment section. I have three columns, flags for the germination (when seeds rotted, or became sick, or when we believe there are mistakes in the dates), seedlings (when the plant has started rotting, or died before seedlings where harvested - to be used for filtering seedlings out of the final data set), and whole petri dish flags - when a shole petridish needs removing because of drying out or mold. Options for flags are: Remove_duplicate, Dead_plant, Sick_plant,  Missing_date, Possible_mistakes_in_ID, Biomass_mistakes, Moldy, Agar_issues and Other. Using dictionaries to translate between comments and flags.

Ver_alp_germ <- Ver_alp_germ %>% 
  mutate(flag_germination = case_when(flag %in% c("Duplicate_remove", "Remove_duplicate", "Remove_Duplicate", "Remove_duplicat", "Remove_unsureID", "	No germination date. 2 seeds.; seed#2 germinated with cotyledons 23.03.2020'", "Might have two versions of this, becasue this one was not crossed out (18.05.2020) Agar dried out, crossed out dish 07.05.2020") ~ "Remove_duplicate")) %>% #Change the few comments in the flag column to match the generalized flags
  mutate(flag_seedling = case_when(flag == "Remove_rotten" ~ "Dead_plant")) %>% #Change the few comments in the flag column to match the generalized flags
  left_join(comment_dict_VA, by = c("comment")) %>% #Add in dictionary to translate from comment column to flags
  left_join(harvest_comment_dict_VA, by = c("harvest_comment")) %>% #Add in dictionary to translate from harvest comment column to flags
  left_join(dish_comment_dict_VA, by = c("comment")) %>% 
  mutate(flag_germination.x = case_when(!is.na(flag_germination.x) ~ flag_germination.x, #gathering the information from different dictionaries for all the flags
                                        is.na(flag_germination.x) ~ flag_germination.y),
         flag_seedling.y = case_when(!is.na(flag_seedling.y) ~ flag_seedling.y,
                                     is.na(flag_seedling.y) ~ flag_seedling),
         flag_whole_petridish = case_when(!is.na(flag_whole_petridish) ~ flag_whole_petridish,
                                          is.na(flag_whole_petridish) ~ flag_whole_petridish.x)) %>% 
  select(-flag_germination, -flag_germination.y, -flag_seedling, -flag_seedling.x, -flag_whole_petridish.x, -flag_whole_petridish.y) %>% 
  rename(flag_germination = flag_germination.x, flag_seedling = flag_seedling.y) %>% 
  select(!flag)  %>% #Remove old flag column
  group_by(petri_dish) %>% 
  fill(flag_whole_petridish, .direction = "downup") #Give whole petridish comment too all seeds in the same petri dish

#Plots for looking at data and looking for mistakes

#Plot to check if total dry mass is larger than the different parts of the plant
Ver_alp_germ %>% 
  ggplot(aes(x = dry_mass_g_total, y = dry_mass_g_above_ground))+
  geom_point() +
  geom_abline()

Ver_alp_germ %>% 
  ggplot(aes(x = dry_mass_g_total, y = dry_mass_g_root))+
  geom_point() +
  geom_abline()

Ver_alp_germ %>% 
  ggplot(aes(x = dry_mass_g_above_ground, y = dry_mass_g_root, color = water_potential))+
  geom_point() +
  geom_abline() +
  scale_color_viridis_d()

#Plots looking at distribution of dayes to germination/cotyledons/true leaves with water potential
Ver_alp_germ %>% 
  ggplot(aes(y = days_to_germination, x = water_potential)) +
  geom_boxplot()

Ver_alp_germ %>% 
  ggplot(aes(y = days_to_cotelydon, x = water_potential)) +
  geom_boxplot()

Ver_alp_germ %>% 
  ggplot(aes(y = days_to_leaf, x = water_potential)) +
  geom_boxplot()


#Plots looking at size of plants with water potential

Ver_alp_germ %>% 
  ggplot(aes(y = dry_mass_g_total, x = water_potential, fill = water_potential)) +
  geom_violin(draw_quantiles = c(0.75, 0.5, 0.25)) +
  scale_fill_viridis_d()

Ver_alp_germ %>% 
  ggplot(aes(y = root_shoot_ratio, x = water_potential, fill = water_potential)) +
  geom_boxplot() +
  scale_fill_viridis_d()

##### Sibbaldia procumbens #####

#### Fixing mistakes in dataset ####

#Making dictionary to fix wrongly entered dates
SP_mistakes <- read.table(header = TRUE, stringsAsFactors = FALSE, text = 
                            "old new
  02.02.2020 02.03.2020
  09.04.2020 09.03.2020
  08.06 08.06.2020")

#Removing wrongly enetered data, fixing wrong dates and making new variables

Sib_pro_germ <- Sib_pro_germ %>%
  mutate(Leaf_date = case_when(Leaf_date == "05.02.2020" ~ "05.03.2020",
                                     TRUE ~ as.character(Leaf_date))) %>% #Fixing one date mistake
  filter(!is.na(Water_potential)) %>%  #Removing a row with only NAs
  mutate(Dry_mass_g_root = str_replace_all(string = Dry_mass_g_root, pattern = ",", replacement = "."),
         Wet_mass_g_root = str_replace_all(string = Wet_mass_g_root, pattern = ",", replacement = ".")) %>% #Fixing a comma mistake
  mutate(Dry_mass_g_root = as.numeric(Dry_mass_g_root),
         Wet_mass_g_root = as.numeric(Wet_mass_g_root))  %>%
  mutate(Total.wet.mass = Wet_mass_g_root + Wet_mass_g_True.leaf + Wet_mass_g_rest) %>% 
  mutate(Germination_date = plyr::mapvalues(Germination_date, from = SP_mistakes$old, to = SP_mistakes$new)) %>% 
  mutate(Cotelydon_date = plyr::mapvalues(Cotelydon_date, from = SP_mistakes$old, to = SP_mistakes$new)) %>%
  mutate(Leaf_date = plyr::mapvalues(Leaf_date, from = SP_mistakes$old, to = SP_mistakes$new)) %>% 
  mutate(Start_date = dmy(Start_date),
         Germination_date = dmy(Germination_date),
         Cotelydon_date = dmy(Cotelydon_date),
         Leaf_date = dmy(Leaf_date),
         Harvest_date = dmy(Harvest_date)) %>% 
  mutate(days_to_germination = Germination_date - Start_date,
         days_to_cotelydon = Cotelydon_date - Start_date,
         days_to_leaf = Leaf_date - Start_date) %>% 
  mutate(site_WP = paste(Site, Water_potential),
         petri_dish = paste(Species, Site, Water_potential, Replicate, sep = "_")) %>% 
  mutate(Water_potential = as.factor(Water_potential),
         Replicate = as.factor(Replicate)) %>% 
  group_by(Species, Site, Water_potential, Replicate) %>% 
  mutate(seeds_in_dish = n()) %>% 
  ungroup() %>% 
  mutate(root_shoot_ratio = Dry_mass_g_root/Dry_mass_g_above_ground) %>% 
  rename(species = Species, siteID = Site, water_potential = Water_potential, replicate = Replicate, seed_nr = Seed, start_date = Start_date, germination_date = Germination_date, cotelydon_date = Cotelydon_date, leaf_date = Leaf_date, harvest_date = Harvest_date, wet_mass_g_root = Wet_mass_g_root, wet_mass_g_above_ground = Wet_mass_g_rest, wet_mass_g_total = Total.wet.mass, wet_mass_g_true_leaf = Wet_mass_g_True.leaf, dry_mass_g_root = Dry_mass_g_root, dry_mass_g_above_ground = Dry_mass_g_above_ground, dry_mass_g_total = Dry_mass_g_total, seed_viable = Viable_seeds, lights_off = Lights_off..Yes.no.) %>%
  mutate(dry_mass = dry_mass_g_root + dry_mass_g_above_ground) %>% 
  mutate(dry_mass_g_total = case_when(dry_mass_g_total == 0 ~ dry_mass_g_total,
                                      dry_mass_g_total > 0 ~ dry_mass_g_total,
                                      is.na(dry_mass_g_total) ~ dry_mass)) %>% 
  mutate(dry_mass_g_total = case_when(ID == "SP_GUD_1_7_18" ~ dry_mass,
                                      ID != "SP_GUD_1_7_18" ~ dry_mass_g_total)) %>% 
  select(-dry_mass)

#### Deal with comments. Categorize them ####
## Entering information in flag columns from comment section. I have three columns, flags for the germination (when seeds rotted, or became sick, or when we believe there are mistakes in the dates), seedlings (when the plant has started rotting, or died before seedlings where harvested - to be used for filtering seedlings out of the final data set), and whole petri dish flags - when a shole petridish needs removing because of drying out or mold. Options for flags are: Remove_duplicate, Dead_plant, Sick_plant,  Missing_date, Possible_mistakes_in_ID, Biomass_mistakes, Moldy, Agar_issues and Other. Using dictionaries to translate between comments and flags.

Sib_pro_germ1 <- Sib_pro_germ %>% 
  left_join(comment_dict_SP, by = c("Comment")) %>% #Translate from the comment column via dictionary
  left_join(harvest_comment_dict_SP, by = c("Harvest_comment")) %>% #translate from the harvest_comment column via dictionary
  left_join(weighing_comment_dict_SP, by = c("Weighing_comments")) %>% #translate from the weighing_comment column via
  rename(flag_germination = flag_germination.x, flag_seedling.z = flag_seedling, flag_seedling = flag_seedling.y) %>% 
  mutate(flag_germination = case_when(is.na(flag_germination) ~ flag_germination.y,
                                      !is.na(flag_germination) ~ flag_germination)) %>% 
  mutate(flag_seedling = case_when(is.na(flag_seedling) ~ flag_seedling.x,
                                   !is.na(flag_seedling) ~ flag_seedling)) %>% 
  select(!flag_seedling.z & !flag_germination.y & !flag_seedling.x) %>% 
  mutate(flag_germination = case_when(is.na(flag_germination) & Remove_whole_petridish == "Remove_duplicate" ~ "Remove_duplicate",
                                      is.na(flag_germination) & Flag %in% c("Remove_duplicate", "Remove_flag") ~ "Remove_duplicate",
                                      !is.na(flag_germination) ~ flag_germination)) %>%
  mutate(flag_seedling = case_when(is.na(flag_seedling) & Remove_whole_petridish == "Remove_parts_missing" ~ "Biomass_mistakes",
                                   is.na(flag_seedling) & Flag == "Remove_biomass_rotten" ~ "Biomass_mistakes",
                                   !is.na(flag_seedling) ~flag_seedling)) %>% 
  mutate(flag_whole_petridish = case_when(is.na(flag_whole_petridish) & Remove_whole_petridish %in% c("Dried out, remove", "dried out, remove") ~ "Agar_issues",
                                          !is.na(flag_whole_petridish) ~ flag_whole_petridish)) %>% #Move the few comments in the flag and remove-whole-petridish column to new flag columns 
  select(!Remove_whole_petridish & !Flag) %>% #Remove old flag columns
  rename(comment = Comment, harvest_comment = Harvest_comment, weighing_comment = Weighing_comments) %>% 
  group_by(petri_dish) %>% 
  fill(flag_whole_petridish, .direction = "downup") %>%  #Give whole petri dish comment too all seeds in the same petri dish
  unique() #the joining makes multiple copies of some rows (have not figured out why yet), using this to fix the problem.


#Plots for looking at data and looking for mistakes

#Plot to check if all dry mass are smaller than wet masses - they are
Sib_pro_germ %>% 
  ggplot(aes(x = wet_mass_g_total, y = dry_mass_g_total))+
  geom_point() +
  geom_abline()

#Plot to check if total dry mass is larger than the different parts of the plant
Sib_pro_germ %>% 
  ggplot(aes(x = dry_mass_g_total, y = dry_mass_g_above_ground))+
  geom_point() +
  geom_abline()

Sib_pro_germ %>% 
  ggplot(aes(x = dry_mass_g_total, y = dry_mass_g_root))+
  geom_point() +
  geom_abline()

Sib_pro_germ %>% 
  ggplot(aes(x = dry_mass_g_above_ground, y = dry_mass_g_root, color = water_potential))+
  geom_point() +
  geom_abline() +
  scale_color_viridis_d()

#Plots looking at distribution of dayes to germination/cotyledons/true leaves with water potential
Sib_pro_germ %>% 
  ggplot(aes(y = days_to_germination, x = water_potential)) +
  geom_boxplot()

Sib_pro_germ %>% 
  ggplot(aes(x = days_to_germination)) +
  geom_density(aes(fill = water_potential, alpha = 0.2)) +
  scale_fill_viridis_d()

Sib_pro_germ %>% 
  ggplot(aes(y = days_to_cotelydon, x = water_potential)) +
  geom_boxplot()

Sib_pro_germ %>% 
  ggplot(aes(x = days_to_cotelydon)) +
  geom_density(aes(fill = water_potential, alpha = 0.2)) +
  scale_fill_viridis_d()

Sib_pro_germ %>% 
  ggplot(aes(y = days_to_leaf, x = water_potential)) +
  geom_boxplot()

Sib_pro_germ %>% 
  ggplot(aes(x = days_to_leaf)) +
  geom_density(aes(fill = water_potential, alpha = 0.2)) +
  scale_fill_viridis_d()

#Plots looking at size of plants with water potential

Sib_pro_germ %>% 
  ggplot(aes(y = dry_mass_g_total, x = water_potential, fill = water_potential)) +
  geom_violin(draw_quantiles = c(0.75, 0.5, 0.25)) +
  scale_fill_viridis_d()

Sib_pro_germ %>% 
  ggplot(aes(y = root_shoot_ratio, x = water_potential, fill = water_potential)) +
  geom_violin(draw_quantiles = c(0.75, 0.5, 0.25)) +
  scale_fill_viridis_d()


#### Make germination metrics ####

Sib_pro_germ %>% 
  mutate(germinated = case_when(is.na(germination_date) ~ 0,
                                !is.na(germination_date) ~ 1)) %>% 
  group_by(petri_dish) %>% 
  mutate(n_germinated = sum(germinated)) %>% view()

