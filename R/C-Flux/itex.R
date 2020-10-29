library(tidyverse)
library("dataDownloader")

#importing flux data from OSF
get_file(node = "zhk3m",
         file = "INCLINE_c-flux_2020.csv",
         path = "data/C-Flux/summer_2020",
         remote_path = "C-Flux")

#only ER, take out p-value>0.05
itex.df <- read_csv("data/C-Flux/summer_2020/INCLINE_c-flux_2020.csv") %>% 
  filter(
    p.value <= 0.05
    # & r.squared >= 0.7
    & Type == "ER"
  )


#average per date and plot

# itex.df <- itex.df %>% 
#   group_by(Plot_ID, Date) %>% 
#   summarise(
#     flux.avg = mean(flux),
#     temp_air.avg = mean(Temp_airavg)
#   )

#adding treatment CTL or OTC

treatment <- read_csv("treatment.csv")

# itex.df <- left_join(itex.df, treatment, by = "Plot_ID")
  # right_join(treatment, itex.df, by = "Plot_ID")

#a bit of cleaning

itex.df <-left_join(itex.df, treatment, by = "Plot_ID") %>%
  mutate(
    Treatment = str_replace_all(Treatment, c("W_C" = "OTC", "C_C" = "CTL")), #replacing the name of the treatments to fit ITEX wishes
    temp_air = Temp_airavg - 273.15, #temp air in celsius
    flux = flux /(60*60), # they want fluxes in mmol/sqm/s instead of mmol/sqm/h
    Replicate = str_replace_all(Replicate, c("1" = "Rep1", "2" = "Rep2", "3" = "Rep3", "4" = "Rep4"))
    # Replicate = replace_all(Replicate, c(1, 2, 3), c("Rep1", "Rep2", "Rep3"))
  ) %>% 
  select(Plot_ID, Replicate, Date, temp_air, r.squared, flux, Treatment)

write_csv(itex.df, "data/C-Flux/summer_2020/ITEX_cflux_2020.csv")


# #fetch closest in time soil temp and moist data from tomst logger for plots which have a tomst logger
# tomst <- #import tomst data
# itex.df <- itex.df %>% 
#   full_join(tomst, by = c("Datetime", "Plot_ID")) %>% 
#   group_by(Plot_ID) %>% 
#   nest() %>% 
#   #fill in soil moist and temp
#   unnest() %>% 
#   ungroup()
#   #filter only the rows with flux data


#assembling environmental data
#organic layer depth
soil <- read_csv("ITEX/soil_samples.csv") %>% 
  group_by(Plot_ID) %>% 
  summarise(
    organic.avg = mean(organic_layer_depth_cm)
  )

#mean plant height
plant <- read_csv("ITEX/vegetation.csv", na = "na") %>% 
  group_by(Plot_ID) %>% 
  summarise(
    height.avg = mean(Vegetation_height_cm, na.rm = TRUE)
  )

#cover data


cover <- read_csv("ITEX/INCLINE_community_2018_2019b.csv", col_types = cols(.default = "c")) %>% 
  filter(
    subPlot == "cover" #I want the plot cover only, not subplot level
    & year == 2019 #2019 only
  ) %>% 
  # # mutate_at("Ach_mil", gsub("NaN", "NA", .))
  # mutate_at("Ach_mil", as.integer)
  pivot_longer(Ach_mil:Vio_tri_cf, names_to = "species") %>% #make a long dataset
  mutate_at("value", as.integer) %>% 
  mutate(
    species = sub("_cf", "", species),
    species = sub("_CF", "", species)
  ) %>% 
  select(Block, plot, Site, species, value)

##getting the average for lichens and mosses (data are only available at subplot level)
lichens.mosses <- read_csv("ITEX/INCLINE_community_2018_2019b.csv", col_types = cols(.default = "c")) %>% 
  filter(
    subPlot != "cover" #we want the actual subplots and not the total plot cover
    & year == 2019
  ) %>% 
  select(subPlot,Block, plot, Site, moss, lichen) %>% 
  mutate_at(c("lichen", "moss"), as.integer) %>% 
  replace_na(list(moss = 0, lichen = 0)) %>% 
  group_by(Block, plot, Site) %>% 
  summarise(
    lichen = mean(lichen, na.rm = TRUE),
    moss = mean(moss, na.rm = TRUE)
  ) %>% 
  pivot_longer(lichen:moss, names_to = "species")

#joining the two dataset to have full info about the plots
cover <- bind_rows(cover, lichens.mosses)



info <- read_csv2("ITEX/species_info.csv") %>% #this is the file with all the info about species
  mutate(species = sub("[.]", "_", species))

info_extra <- read.csv("ITEX/species_extra_info.csv") %>% #an extra file providing by Ragnhild for species that were not in the first one
  select(species, functionalGroup)

info <- bind_rows(info, info_extra) %>% 
  mutate(
    functionalGroup = sub("forb", "Forbs", functionalGroup), 
    functionalGroup = sub("graminoid", "Graminoids", functionalGroup),
    functionalGroup = sub("Gramoinds", "Graminoids", functionalGroup),
    functionalGroup = sub("woody", "Deciduous_Shrubs", functionalGroup), #in the current situation all the woody found are deciduous_shrubs. It might change in the future.
    functionalGroup = sub("pteridophyte", "Forbs", functionalGroup) #pteridophyte are groupe with forbs as there is no such category for ITEX
  ) %>% 
  select(species, functionalGroup) %>% 
  mutate_at("functionalGroup", as_factor)
write_csv(info, "ITEX/ITEX_groups.csv") # next time I can just use this and spare some time and energy
# summary(info)

group_cover <- left_join(cover, info, by = "species") %>% 
  replace_na(list(value = 0)) %>% #na were put when a species was not observed. We assumed that it means the species is not present.
  group_by(Block, plot, Site, functionalGroup) %>% 
  summarise(
    sum.group = sum(value)
  ) %>% 
    mutate( #just building the plot names to have the same as the other dataframes
    Site = substr(Site, 1, 3),
    Site = str_to_upper(Site)
    # Site = str_replace_all(Site, c("Lavisdalen" = "LAV", "Skjellingahaugen"))
  ) %>% 
  unite("Plot_ID", c("Site", "Block", "plot"), sep = "_") %>% 
  pivot_wider(names_from = functionalGroup, values_from = sum.group) #ITEX is using a wide format


#Now it's time to join all the plot meta data together
plot.meta.data <- full_join(plant, soil, by = "Plot_ID") %>% 
  full_join(group_cover, by = "Plot_ID") %>% 
  left_join(treatment, by = "Plot_ID") %>% #adding treatment
  mutate(
    Treatment = str_replace_all(Treatment, c("W_C" = "OTC", "C_C" = "CTL")) #transforming treatment in ITEX format
  ) %>% 
  filter( #we want only the OTC and the CTL
    Treatment == "OTC"
    | Treatment == "CTL"
  )
write_csv(plot.meta.data, "ITEX/plot_meta.cvs")

#below is some stuffs useful to see which species are missing a name and which one is in which group.
#
# 
# temp <- left_join(cover, info, by = "species") %>% 
#   filter(
#     # functionalGroup == "Forbs"
#      !is.na(value)) %>% 
#   select(functionalGroup) %>% 
#   distinct() %>% 
#   write_csv("forbs_to_look_at.csv")
#   select(Block, plot, Site, Veg_cover, species, value, functionalGroup)
#   
#   temp2 <- left_join(cover, info, by = "species") %>% 
#     filter(
#       functionalGroup == "woody"
#       & !is.na(value)) %>% 
#     select(species, functionalGroup) %>% 
#     distinct() %>% 
#     write_csv("woody_to_look_at.csv")
# 
#   temp3 <- left_join(cover, info, by = "species") %>% 
#     filter(
#       is.na(functionalGroup)
#       & !is.na(value)) %>% 
#     select(species) %>% 
#     distinct()
# 
#   temp_lichens <- left_join(cover, info, by = "species") %>% 
#     filter(
#       functionalGroup == "Lichens"
#       & !is.na(value)) %>% 
#     select(Block, plot, Site, value) %>% 
#     distinct()
#   
#   temp_mosses <- left_join(cover, info, by = "species") %>% 
#     filter(
#       functionalGroup == "Mosses"
#       & !is.na(value)) %>% 
#     select(Block, plot, Site, value) %>% 
#     distinct()
  
