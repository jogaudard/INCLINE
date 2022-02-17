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

Sib_pro_germ <- read_csv("data/Germination/INCLINE_Germination_Seedling_Experiment_Data_SP.csv")
Ver_alp_germ <- read_csv2("data/Germination/INCLINE_Germination_Seedling_Experiment_Data_Va.csv")
INCLINE_metadata <- read_delim("data/INCLINE_metadata_LoggerDates.csv", delim = ";")


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


#Moving comments to comment section that have been entered in date columns.

Ver_alp_germ1 <- Ver_alp_germ %>%
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
  mutate(Start_date = dmy(Start_date)) %>% 
  mutate(Germination_date = dmy(Germination_date)) %>% #One failed, need to find it and fix it
  mutate(Cotelydon_date = dmy(Cotelydon_date)) %>% #One failed, need to find it and fix it
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
  rename(species = Species, siteID = Site, water_potential = Water_potential, replicate = Replicate, seed_nr = Seed, start_date = Start_date, germination_date = Germination_date, comment = Comment, cotelydon_date = Cotelydon_date, deaf_date = Leaf_date, harvest_date = Harvest_date, harvest_comment = Harvest_comment, wet_mass_g_root = Wet_mass_g_root, wet_mass_g = Wet_mass_g_, dry_mass_g_root = Dry_mass_g_root, dry_mass_g_above_ground = Dry_mass_g_above_ground, dry_mass_g_total = Dry_mass_g_total, weighing_comments = Weighing_comments, notes = Notes, removel_whole_petridish = Remove_whole_petridish, seed_viable = Seed_viable, lights_off = "Lights_off (Yes/no)", flag = Flag)

#### Deal with comments. Categorize them ####
## Entering information in the Flag column. Options are: Remove_duplicate, Dead_plant, Sick_plant, Other, Missing_date, Possible_mistakes_in_ID, and Biomass_mistakes

Ver_alp_germ1 <- Ver_alp_germ1 %>% 
  mutate(Flag = ifelse(flag %in% c("Duplicate_remove", "Remove_duplicate", "Remove_Duplicate", "Remove_duplicat", "Remove_unsureID", "	No germination date. 2 seeds.; seed#2 germinated with cotyledons 23.03.2020'", "Might have two versions of this, becasue this one was not crossed out (18.05.2020) Agar dried out, crossed out dish 07.05.2020"),  "Remove_duplicate",
                       ifelse(flag == "No", NA,
                              ifelse(flag == "Remove_rotten", "Dead_plant", flag)))) %>% 
  mutate(Flag = ifelse(comment %in% c("yellowing 09.04.2020", "yellowing 09.04", "yellow", "Very moldy, embryo green with cut-test", "Very moldy, but allive (cut-test)", "seems mouldy at root 17.03.2020", "seems dyin 17.03.2020", "Black fungus on cotelydons", "Black cotelydon?", "09.03.2020, looks pretty bad"), "Sick_plant",
                       ifelse(comment %in% c("Was yellow and dead, but also disturbed during harvest of another seedling", "Was not there on th 09.04.2020 (Seems dead on harvest day 07.04.2020)", "VERY MOLDY PLATE-17.03.2020", "Too mushy to harvest", "This whole plate is dying", "Seems to be dead", "Seems dead,re did the parafilm, 06.04.2020", "Seems dead, re did the parafilm, 06.04.2020", "Seems dead on hravest day 07.04.2020, wasnot harvested", "Seems dead", "Seed blackened 07.05.2020", "rotted seed", "Reposition due to loose agar; Seems to be dead", "moulded away 17.03.2020", "Molded", "Modly seed", "Looks dead, very yellow and lying down, did not harvest on the 07.04.2020", "Looks dead", "Had leaf until the 27th without a root. Got the root on 28th. Seems to disintegrate on the 6.3.. Got cotyledons but falling apart on 17.03.2020", "found germinated and dead 17.03.2020", "Found dead on the 11.03.2020", "	dying 09.04.2020", "dying 09.04", "Don't have any green pgiments left, scored it as dead", "DEAD? -17.03.2020", "Dead?", "dead?", "Dead seed (06.04.2020)", "dead 6.3.", "dead 20.03.2020", "dead 17.03.2020", "dead 06.03.2020. Well, it has cotyledons 17.03.2020", "Dead", "dead", "20.02.2020 Replated seeds on agar. Seems dead."), "Dead_plant",
                              ifelse(comment %in% c("Two seeds?", "Two seeds at same place", "Two seedlings in the same place", "Two seedlings in this spot: there was not really any true leaves here, only two seedling with cotelydons each. That was discovered when pulling them out for harvest, so they were removed. Taking out the true leaf date, but the germintion and cotelydon date can still be used.", "2 seeds on this spot"), "Remove_duplicate",
                                     ifelse(comment %in% c("Was scored as dead on the 30.03.2020, but was found with small pair of true leaves later", "Was pulled out with harvesting the one next to it", "Was pulled halfway out during removing another seedling", "Three cotelydons", "SG Measured in arvo, but some already had values from morning...", "SG checked this dish in arvo, but it already had values.....", "Seed at the bottom of the row", "Rooted on the 14.05.2020", "Reposition due to loose agar", "replaced parafilm 06.04.2020", "reparafilmed 09.04, some crystalization", "reparafilm, 6.4.20", "reparafilm 06.04.2020; this is not ready to harvest, but 13 is", "reparafilm 06.04.2020", "re did the parafilm, 06.04.2020", "needs parafilm 06.04.2020", "Missing seed", "Missing", "Might have two versions of this, becasue this one was not crossed out (18.05.2020) Agar dried out, crossed out dish 07.05.2020", "meant to be harvested 20.04 but no plant?", "lying on agar 09.04", "lying on agar", "Leaves but no root on the 27.02.2020", "Leaves (green but not extanded) but no root. 04.03.2020 extendend leafs but still no root", "It said in the document that three was sampled, and four was to be done on the 16.04.2020. But it was number three that was left. I harvested this on the 16.04.2020. Moved the 13.04.2020 date down to number 4.", "Has very loose agar!!!!", "Had two pairs of true leaves on the 09.04.2020, harvested then", "Green seed with some extendet leaf with no root. 09.03.2020: agar dried out and re-moisturized", "Green seed with extended leaf but no root: on harvest day 08.04.2020 it was so rotten there was no way to sample it.", "Green leaves, but no root", "Green leafs but no root, still no root but have extended green leafs 11.03.2020: rotted on the 14.05.2020", "Green leafs but no root, still just green leafs but no root (11.03.2020)", "Green leaf but no root", "Green but no root, last checked 28th", "Green but no root or fully extended leafs.", "Germination during the weekend", "Germination and cotelydons over the weekend", "general plat agar dried out 30.04.2020, like a tight film", "extended green leaf but no root", "Does not have a true pair of leaves, I think it might be switched out with number 9. Removed the leaf out date here, and moved the date to number 9 (they are right above each other)", "Does not actually have a true leaf, took out the leaf date (02.04.2020)", "Did not have true leaves on the 09.04.2020. Took out the date for leaf out (17.03.2020) and the harvest date (07.04.2020)", "Did not have a date, but had true leaves and a cross, put in the date from number 13 that had a date, but did not have a true pair of leaves, they are right above each other.", "dead, well germinated 20.03.2020", "couldnt find the seed and it was a circle wwith a x on.", "close to dry out 20.03.2020", "close to dry-out, re-moisturized 20.03.2020", "Agar drying 16.04.2020; on agar 09.04", "A pair of true leaves, but the cotelydons are yellow and dissintegrating", "2leaves", "20.02.2020 Replated seeds on agar", "2 leaves", "09.03.2020: agar dried out and re-moisturized"), "Other",
                                            ifelse(comment %in% c("Was scored for true leaf from before, put the last scoring date as the leaf date.", "Was crossed out as leafing out, but with no date, put todays date 16.04.2020", "Was crossed out with true leaves, but no date, put the last scoring date in.", "Was crossed out with cotelydons, but did not have a date on the 29.02.2020, so it happened before that. I just put in the 28.02.2020, because it defenitaly happened before that", "Was originally swtiched around with 18, might not be all correct with the dates and order of things", "Was originally swtiched around with 17, might not be all correct with the dates and order of things", "Was originally swtiched around with 14, might not be all correct with the dates and order of things", "Was originally swtiched around with 10, might not be all correct with the dates and order of things", "Was crossed out as having cotelydons but not marked in on the 29.02.2020. so at the latest it had cotelydons on the 28.02.2020", "the germination date isnt right", "Should have had an earlier date on the leaf out because it was very highly developed by then", "Should have had a leaf date that was earlier, long stem with true leaves on on the 09.04.2020", "Should have had a leaf date that was earlier, long stem with true leaves on on the 09.04.2020", "should have a germination date. harvested on the 07.07 and it had a circle", "ring from before 17.03.2020", "	ring and slash from before 17.03.2020", "Only one cotelydon - have probably been fully expanded before this date, hard to see with only one cotelydon", "On the 4th we found the cotelydons but non of the other seeds had germinated, Noted wrong place?", "not germinated 20.03.2020", "No true leaf here, date was set to 23.03.2020", "no germination record", "No germination date.", "no germination date though?", "No germination date", "No date on germination?", "No cotelydon date", "Must have had cotelydons before this since there were the pair of true leaves as well", "I am assuming that seed 10 was wrongly put there, but should have been put here. Seesm like it leafed out a long time ago, for example the 26.03.2020, so I put that in and harvested it on the 08.04.2020", "Is not dead, and does not have a cross next to it, so why a dead date? (06.04.2020)", "Have a full cross and very developed leaves, but not a date for true leaves on the 09.04.2020", "Hasnt germinatet yet 02.02.2020?", "Hasnt germinated 08.06.2020", "Has a true leaf on the 09.04.2020 (and two crosses), but no date.", "Harvested earlier, but no date logged. Inserted today's date.", "Had germinated and cotelydons on the 06.04.2020, might have germinated before", "Had both cotelydon and leaves on the same date, might have gotten cotelydons before", "Had been crossed out for leaves, but had no date 16.04.2020, so it probably happened before", "Had a leaf out date  of 26.03.2020, but no cross or true leaf pair here on the 08.04.2020", "Had a harvest date on the 13.04.2020, but is harvested 16.04.2020.", "Had a circle around it, but no date for germination, had germinated and folded out cotelydons on the 27.02.2020", "Green seed(no extended leaf) and no root 29.02.2020, extended leaf 02.03.2020 but still no root.", "germinated before this date but not recorded", "Found cotelydons, but no germination date noted from earlier", "Did not have a corelydon date", "Did already hava cross for true leaf, but no date. Put the last scoring date in as leaf date", "Crossed out for leaf date, but didn't have a date. Entered the last scoring date.", "crossed but no date for true leaf", "circled but no germ date", "but cotelydon is on agar 13.04.2020", "Been harvested, but no date logged. Inserted today's date.", "Been harvested at some time, but not logged. Inserted today's date.", "Been harvested at some time, but no date logged. Inserted today's date.", "At least had true leaves on the 20.04 since it was crossed out with true leaves on the 23.04"), "Missing_wrong_date",
                                                   ifelse(comment %in% c("VA_SKJ_5_4 looked a lot like VA_ULV (because it had been labelled wrongly and the SKJ had almost dissaperead) on the 06.04.2020, might have been done wrong before this.", "This is gone. Possibley confused with rep 5", "Root was intertwingled with number 6, impossible to tell the difference between the two - was not harvested", "Could not find any true leaves on this one. I'm guessing it's turned upside down so that it is number 7 is the one that was observed, as nr 7 was ready to harvest?", "Could have been mistaken for 15, because that is dead now, this has true leaves and is harvested on the 07.04.2020", "18 was not ready for harvest-mistaken for 19"), "Possible_mistakes_in_ID", 
                                                          ifelse(comment %in% c("part of root stuck to paper it was harvested on", "no true leaves at harvest"), "Biomass_mistakes", flag )))))))) %>% 
  mutate(seed_viable = ifelse(comment %in% c("Very moldy, embryo green with cut-test", "Very moldy, but allive (cut-test)"), "Yes", seed_viable)) %>% 
  group_by(petri_dish)
