###########################################################
### Script for cleaning biomass removal data for INCLINE ###
###########################################################

#### Libraries ####
library("tidyverse")
library("lubridate")
library("ggpubr")
library("dataDownloader")

#### Downloading data from OSF ####

#osf_auth(token = "get from my document every time you run the code") 

get_file(node = "zhk3m",
         file = "Removal_experiment_biomass_data.csv",
         path = "data/Biomass",
         remote_path = "RawData/Biomass_removal")

#### Load data ####

removal <- read.delim("Data/Biomass/Removal_experiment_biomass_data.csv", sep = ",", header = TRUE, dec = ".")

#### Clean data ####

removal <- removal %>% 
  mutate(Site = case_when(Site == "SKJ" ~ "Skj",
                          Site == "GUD" ~ "Gud",
                          Site == "LAV" ~ "Lav",
                          Site == "ULV" ~ "Ulv")) %>% 
  mutate(plotID = paste0(Site, "_", Plot)) %>% 
  pivot_longer(cols = c("Graminoid", "Forb", "Bryophyte", "Woody", "Fern", "Lichen", "Litter"), names_to = "FunctionalGroup", values_to = "value") %>% 
  mutate(Site = factor(Site, levels = c("Ulv", "Lav", "Gud", "Skj")), #Ordering the sites from dry to wet
         FunctionalGroup = factor(FunctionalGroup, levels = c("Graminoid", "Forb", "Fern","Woody", "Bryophyte", "Lichen", "Litter"))) %>%  #Ordering the functional groups like I wan them in the figure
  group_by(Year, plotID, FunctionalGroup) %>% 
  mutate(value = sum(value)) %>% 
    rename(siteID = Site, year = Year, date_removal = Removal_date, date_sorting = Sorting_date, date_weighing = Weighing_date, recorder_removal = Removal_person, recorder_sorting = Sorting_person, recorder_weighing = Weighing_person, treatment = Treatment, comments = Comments, functional_group = FunctionalGroup) %>% 
  select(-Plot)


#### Calculate numbers for biomass information in manuscripts ####
#Mean biomass removed per year
removal %>% group_by(year, siteID, plotID) %>% mutate(total_biomass = sum(value, na.rm = TRUE)) %>% ungroup() %>% group_by(year) %>% mutate(mean_biomass = mean(total_biomass)) %>%  ungroup() %>%  select(year,  mean_biomass) %>% unique() 

#Mean biomass removed per year per treatment
removal %>% group_by(year, siteID, plotID) %>% mutate(total_biomass = sum(value, na.rm = TRUE)) %>% ungroup() %>% group_by(year, treatment) %>% mutate(mean_biomass_treatment = mean(total_biomass)) %>%  ungroup() %>%  select(year, treatment, mean_biomass_treatment) %>% unique() 

#Mean biomass removed per functional group
removal %>% group_by(functional_group, siteID, plotID) %>% mutate(total_biomass = sum(value, na.rm = TRUE)) %>% ungroup() %>% group_by(functional_group) %>% mutate(mean_biomass = mean(total_biomass)) %>%  ungroup() %>%  select(functional_group,  mean_biomass) %>% unique() 


#### Plot data ####

#### Color palette ####
Brown_Green_full_palette <- c("#543005", "#8C510A", "#BF812D","#DFC27D", "#F6E8C3","#F5F5F5", "#C7EAE5","#80CDC1","#35978F","#01665E", "#003C30")
Brown_Green_6_color_palette <- c("#8C510A", "#BF812D", "#F6E8C3","#C7EAE5","#80CDC1","#35978F","#01665E")
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
palette_with_white <- c("#a6611a", "#dfc27d", "#f5f5f5", "#80cdc1", "#018571")
palette <- c("#a6611a", "#dfc27d", "#80cdc1", "#018571")


#### Biomass removal plot ####

plot1 <- removal %>% 
  ggplot(aes(y = value, x = year, fill = functional_group))+
  geom_bar(position = "stack", stat = "identity") +
  facet_wrap(treatment ~ siteID, nrow = 2) +
  scale_fill_manual(values= rev(Brown_Green_6_color_palette)) +
  #ggtitle("Biomass removed") +
  ylab("Biomass (g)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 40),
        text = element_text(size = 35))

ggsave(filename = "biomass_removal_year_treatment_site2.jpg", plot = plot1, width = 20, height = 12)
