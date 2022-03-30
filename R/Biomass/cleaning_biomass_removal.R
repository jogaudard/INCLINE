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

removal <-read.delim("Data/Biomass/Removal_experiment_biomass_data.csv", sep = ",", header = TRUE, dec = ".")

#### Clean data ####

removal1 <- removal %>% 
  mutate(plotID = paste0(Site, "_", Plot)) %>% 
  pivot_longer(cols = c("Graminoid", "Forb", "Bryophyte", "Woody", "Fern", "Lichen", "Litter"), names_to = "FunctionalGroup", values_to = "value") %>% 
  mutate(Site = factor(Site, levels = c("ULV", "LAV", "GUD", "SKJ")), #Ordering the sites from dry to wet
         FunctionalGroup = factor(FunctionalGroup, levels = c("Graminoid", "Forb", "Fern","Woody", "Bryophyte", "Lichen", "Litter"))) %>%  #Ordering the functional groups like I wan them in the figure
  group_by(Year, plotID, FunctionalGroup) %>% 
  mutate(value = sum(value))
  

#### Plot data ####

#### Color palette ####
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")

#### Biomass removal plot ####

plot1 <- removal1 %>% 
  ggplot(aes(y = value, x = Year, fill = FunctionalGroup))+
  geom_bar(position = "stack", stat = "identity") +
  facet_wrap(Treatment ~ Site, nrow = 2) +
  scale_fill_manual(values=rev(cbPalette)) +
  ggtitle("Biomass removed in the first year") +
  ylab("Biomass (g)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 40),
        text = element_text(size = 35))

ggsave(filename = "biomass_removal_year_treatment_site.jpg", plot = plot1, width = 20, height = 12)
