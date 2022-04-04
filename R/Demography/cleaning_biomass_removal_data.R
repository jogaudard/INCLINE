################################################
### Script for cleaning biomass removal data ###
################################################

#### Libraries ####
library("tidyverse")
library("lubridate")
library("ggpubr")

#### Downloading data from OSF ####

#osf_auth(token = "get from my document every time you run the code") 

get_file(node = "zhk3m",
         file = "Removal_experiment_biomass_data.csv",
         path = "data/Removal",
         remote_path = "RawData/Biomass_removal")

#### Load data ####

removal <- read_csv("data/Removal/Removal_experiment_biomass_data.csv")
#Need biomass regression datasets as well


#### Clean data ####

removal <- removal %>%
  mutate(plotID = paste0(Site, "_", Plot)) %>% 
  rename(siteID = Site, removal_date = Removal_date, removal_person = Removal_person, sorting_date = Sorting_date, sorting_person = Sorting_person, weighing_date = Weighing_date, treatment = Treatment, comments = Comments, year = Year) %>% 
  select(!Plot) %>% 
  mutate(removal_date = dmy(removal_date),
         sorting_date = dmy(sorting_date),
         weighing_date = dmy(weighing_date)) %>% 
  pivot_longer(cols = c("Graminoid", "Forb", "Bryophyte", "Woody", "Fern", "Lichen", "Litter"), names_to = "functional_group", values_to = "value") %>% 
  mutate(siteID = factor(siteID, levels = c("ULV", "LAV", "GUD", "SKJ")), #Ordering the sites from dry to wet
         functional_group = factor(functional_group, levels = c("Graminoid", "Forb", "Fern","Woody", "Bryophyte", "Lichen", "Litter"))) %>%  #Ordering the functional groups like I want them in the figure
  group_by(year, plotID, functional_group) %>% 
  mutate(value = sum(value))


#### Plot data ####

#### Color palette ####
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")

#### Biomass removal plot ####

biomass_2019 <- removal %>% 
  filter(year == 2019) %>% 
  ggplot(aes(y = value, x = siteID, fill = functional_group))+
  geom_bar(position = "stack", stat = "identity") +
  facet_wrap(year~treatment, nrow = 1) +
  scale_fill_manual(values=rev(cbPalette)) +
  ggtitle("Biomass removed in the first year") +
  ylab("Biomass (g)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 20))

biomass_2020_2021 <- removal %>% 
  filter(!year == 2019) %>% 
  ggplot(aes(y = value, x = siteID, fill = functional_group))+
  geom_bar(position = "stack", stat = "identity") +
  facet_wrap(year~treatment, nrow = 3) +
  scale_fill_manual(values=rev(cbPalette)) +
  ggtitle("Biomass removed in following years") +
  ylab("Biomass (g)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        legend.position = "none")

biomass_figure <- ggarrange(biomass_2019, biomass_2020_2021, nrow = 2, ncol = 1,  heights = c(2,3),  common.legend = TRUE,legend = "bottom")

ggsave(filename = "biomass_removal.jpg", plot = biomass_figure, width = 12, height = 20, units = "cm")
