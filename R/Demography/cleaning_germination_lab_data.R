################################################################
### Script for cleaning raw data from germination experiment ###
################################################################

#### Libraries ####
library(tidyverse)
library(dataDownloader)
library(osfr)
library(lubridate)

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
         file = "INCLINE_metadata_LoggerDates.csv",
         path = "data",
         remote_path = "RawData")

#### Load data ####

Sib_pro_germ <- read_csv2("data/Germination/INCLINE_Germination_Seedling_Experiment_Data_SP.csv")
Ver_alp_germ <- read_csv2("data/Germination/INCLINE_Germination_Seedling_Experiment_Data_Va.csv")
INCLINE_metadata <- read_delim("data/INCLINE_metadata_LoggerDates.csv", delim = ";")
comment_dict_VA <- read_delim("data/Germination/comment_dictionary_VA.csv", delim = ";")
harvest_comment_dict_VA <- read_delim("data/Germination/harvest_comment_dictionary_VA.csv", delim = ";")
dish_comment_dict_VA <- read_delim("data/Germination/dish_comment_dictionary_VA.csv", delim = ";")
comment_dict_SP <- read_delim("data/Germination/comment_dictionary_SP.csv", delim = ";")
harvest_comment_dict_SP <- read_delim("data/Germination/harvest_comment_dictionary_SP.csv", delim = ";")
weighing_comment_dict_SP <- read_delim("data/Germination/weighing_comment_dictionary_SP.csv", delim = ";")

##### Veronica alpina #####

#### Fixing mistakes in dataset ####

#Making dictionary to fix wrongly entered dates
VA_mistakes <- read.table(header = TRUE, stringsAsFactors = FALSE, text = 
                            "old new
  02.02.2020 02.03.2020
  20.2.20 20.02.2020
  20.2.2020 20.02.2020
  21.02 21.02.2020
  13.04 13.04.2020 
  21.02.2002 20.02.2020
  04.02.2020 04.03.2020
  03.07.22020 03.07.2020
  6.7.2020 06.07.2020")


#Moving comments to comment section that have been entered in date columns. Removing things that have been entered in the wrong column. Update the dates to the correct ones using the dictionary above.

Ver_alp_germ <- Ver_alp_germ %>%
  mutate(Comment = ifelse(Germination_date %in% c("DEAD", "dead"), "Dead", Comment), 
         Germination_date = ifelse(Germination_date %in% c("DEAD", "dead", " ", ""), NA, Germination_date), 
         Harvest_comment = ifelse(Harvest_date == "Brown and a bit moldy", "Brown and a bit moldy", Harvest_comment), 
         Harvest_date = ifelse(Harvest_date == "Brown and a bit moldy", NA, Harvest_date), 
         Comment = ifelse(dead_date == "yellow", "yellow",
                          ifelse(dead_date %in% c("seed seems dead", "Seed seems dead"), "Seed seems dead", 
                                 ifelse(dead_date == "early", "dead",
                                        ifelse(dead_date == "meant to be harvested 20.04 but no plant?", "meant to be harvested 20.04 but no plant?", Comment)))), 
         dead_date = ifelse(dead_date %in% c("yellow", "Unknown", "seed seems dead", "meant to be harvested 20.04 but no plant?", "early", "Seed seems dead"), NA,
                            ifelse(dead_date == "before 18.05.2020", "18.05.2020",
                                   ifelse(dead_date == "before 17.03.2020", "17.03.2020", dead_date))),
         Dry_mass_g_root = ifelse(Dry_mass_g_root == "X", NA, Dry_mass_g_root),
         Weighing_comments = ifelse(Dry_mass_g_total == "two roots; two seedlings; second seedling: Dry_mass_g_root=0.00036, Dry_mass_g_above_ground=0.00022", "two roots; two seedlings; second seedling: Dry_mass_g_root=0.00036, Dry_mass_g_above_ground=0.00022", 
                                    ifelse(Dry_mass_g_total == "two roots", "two roots", 
                                           ifelse(Dry_mass_g_total == "3 roots", "3 roots", Weighing_comments))),
         Dry_mass_g_total = ifelse(Dry_mass_g_total %in% c("two roots; two seedlings; second seedling: Dry_mass_g_root=0.00036, Dry_mass_g_above_ground=0.00022","two roots","3 roots"), NA, Dry_mass_g_total)) %>% 
  mutate(Germination_date = plyr::mapvalues(Germination_date, from = VA_mistakes$old, to = VA_mistakes$new), #Using dictionary to change dates from wrongly entered to correct
         Cotelydon_date = plyr::mapvalues(Cotelydon_date, from = VA_mistakes$old, to = VA_mistakes$new),
         Leaf_date = plyr::mapvalues(Leaf_date, from = VA_mistakes$old, to = VA_mistakes$new)) %>% 
  mutate(petri_dish = paste(Species, Site, Water_potential, Replicate, sep = "_")) %>% 
  filter(!petri_dish %in% c("VA_GUD_6_7X", "VA_LAV_9_9X")) %>% #These are extra petri dishes that do not have any data - so I do not think there actually were extra petri dishes, removing them.
  mutate(Start_date = case_when(unique_ID == "VA_LAV_5_6_18" ~ "18022020",
                                TRUE ~ as.character(Start_date))) %>%  #One individual that was missing information in the start date column, manually found the right date and entered it here
  mutate(Germination_date = case_when(Germination_date == "21.feb" ~ "21.02.2020",
                                      TRUE ~ as.character(Germination_date))) %>% #Getting date in right order
  mutate(Cotelydon_date = case_when(Cotelydon_date == "13.apr" ~ "13.04.2020",
                                    TRUE ~ as.character(Cotelydon_date)))

# Make new variables

Ver_alp_germ <- Ver_alp_germ %>%
  mutate(Start_date = dmy(Start_date)) %>% 
  mutate(Germination_date = dmy(Germination_date)) %>% 
  mutate(Cotelydon_date = dmy(Cotelydon_date)) %>% 
  mutate(Leaf_date = dmy(Leaf_date)) %>% 
  mutate(petri_dish = paste(Species, Site, Water_potential, Replicate, sep = "_")) %>% 
  mutate(days_to_germination = Germination_date - Start_date,
         days_to_cotelydon = Cotelydon_date - Start_date,
         days_to_leaf = Leaf_date - Start_date) %>% 
  mutate(site_WP = paste(Site, Water_potential)) %>% 
  mutate(Water_potential = as.factor(Water_potential)) %>% 
  group_by(Species, Site, Water_potential, Replicate) %>% 
  mutate(seeds_in_dish = n())  %>% 
  #rename(unique_ID = X) %>% 
  rename(species = Species, siteID = Site, water_potential = Water_potential, replicate = Replicate, seed_nr = Seed, start_date = Start_date, germination_date = Germination_date, comment = Comment, cotelydon_date = Cotelydon_date, leaf_date = Leaf_date, harvest_date = Harvest_date, harvest_comment = Harvest_comment, wet_mass_g_root = Wet_mass_g_root, wet_mass_g = Wet_mass_g_, dry_mass_g_root = Dry_mass_g_root, dry_mass_g_above_ground = Dry_mass_g_above_ground, dry_mass_g_total = Dry_mass_g_total, weighing_comments = Weighing_comments, notes = Notes, removel_whole_petridish = Remove_whole_petridish, seed_viable = Seed_viable, lights_off = "Lights_off (Yes/no)", flag = Flag)

#### Deal with comments. Categorize them ####
## Entering information in flag columns from comment section. I have three columns, flags for the germination (when seeds rotted, or became sick, or when we believe there are mistakes in the dates), seedlings (when the plant has started rotting, or died before seedlings where harvested - to be used for filtering seedlings out of the final data set), and whole petri dish flags - when a shole petridish needs removing because of drying out or mold. Options for flags are: Remove_duplicate, Dead_plant, Sick_plant,  Missing_date, Possible_mistakes_in_ID, Biomass_mistakes, Moldy, Agar_issues and Other. Using dictionaries to translate between comments and flags.

Ver_alp_germ <- Ver_alp_germ %>% 
  mutate(flag_germination = case_when(flag %in% c("Duplicate_remove", "Remove_duplicate", "Remove_Duplicate", "Remove_duplicat", "Remove_unsureID", "	No germination date. 2 seeds.; seed#2 germinated with cotyledons 23.03.2020'", "Might have two versions of this, becasue this one was not crossed out (18.05.2020) Agar dried out, crossed out dish 07.05.2020") ~ "Remove_duplicate")) %>% #Change the few comments in the flag column to match the generalized flags
  mutate(flag_seedling = case_when(flag == "Remove_rotten" ~ "Dead_plant")) %>% #Change the few comments in the flag column to match the generalized flags
  left_join(comment_dict_VA, by = c("comment", "flag_germination", "flag_seedling")) %>% #Translate from the comment column via dictionary
  left_join(harvest_comment_dict_VA, by = c("harvest_comment", "flag_germination", "flag_seedling", "flag_whole_petridish")) %>% #translate from the harvest_comment column via dictionary
  left_join(dish_comment_dict_VA, by = c("comment", "flag_whole_petridish")) %>% #None of these comments apply to Ver_alp, so maybe they are comments for Sib_pro?
  select(!flag)  %>% #Remove old flag column
  group_by(petri_dish) %>% 
  fill(flag_whole_petridish, .direction = "downup") #Give whole petridish comment too all seeds in the same petri dish



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
  mutate(Germination_date = plyr::mapvalues(Germination_date, from = SP_mistakes$old, to = SP_mistakes$new)) %>% 
  mutate(Cotelydon_date = plyr::mapvalues(Cotelydon_date, from = SP_mistakes$old, to = SP_mistakes$new)) %>%
  mutate(Leaf_date = plyr::mapvalues(Leaf_date, from = SP_mistakes$old, to = SP_mistakes$new)) %>% 
  mutate(Replicate = as.factor(Replicate)) %>% 
  mutate(Start_date = dmy(Start_date)) %>% 
  mutate(Germination_date = dmy(Germination_date)) %>% 
  mutate(Cotelydon_date = dmy(Cotelydon_date)) %>% 
  mutate(Leaf_date = dmy(Leaf_date)) %>% 
  mutate(petri_dish = paste(Species, Site, Water_potential, Replicate, sep = "_")) %>% 
  mutate(days_to_germination = Germination_date - Start_date,
         days_to_cotelydon = Cotelydon_date - Start_date,
         days_to_leaf = Leaf_date - Start_date) %>% 
  mutate(site_WP = paste(Site, Water_potential)) %>% 
  mutate(Water_potential = as.factor(Water_potential)) %>% 
  group_by(Species, Site, Water_potential, Replicate) %>% 
  mutate(seeds_in_dish = n()) %>% 
  ungroup() %>% 
  rename(species = Species, siteID = Site, water_potential = Water_potential, replicate = Replicate, seed_nr = Seed, start_date = Start_date, germination_date = Germination_date, cotelydon_date = Cotelydon_date, leaf_date = Leaf_date, harvest_date = Harvest_date, wet_mass_g_root = Wet_mass_g_root, wet_mass_g_above_ground = Wet_mass_g_rest, wet_mass_g_total = "Total wet mass", wet_mass_g_true_leaf = "Wet_mass_g_True leaf", dry_mass_g_root = Dry_mass_g_root, dry_mass_g_above_ground = Dry_mass_g_above_ground, dry_mass_g_total = Dry_mass_g_total, seed_viable = Viable_seeds, lights_off = "Lights_off (Yes/no)")

#### Deal with comments. Categorize them ####
## Entering information in flag columns from comment section. I have three columns, flags for the germination (when seeds rotted, or became sick, or when we believe there are mistakes in the dates), seedlings (when the plant has started rotting, or died before seedlings where harvested - to be used for filtering seedlings out of the final data set), and whole petri dish flags - when a shole petridish needs removing because of drying out or mold. Options for flags are: Remove_duplicate, Dead_plant, Sick_plant,  Missing_date, Possible_mistakes_in_ID, Biomass_mistakes, Moldy, Agar_issues and Other. Using dictionaries to translate between comments and flags.

Sib_pro_germ <- Sib_pro_germ %>% 
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
  fill(flag_whole_petridish, .direction = "downup") #Give whole petri dish comment too all seeds in the same petri dish


