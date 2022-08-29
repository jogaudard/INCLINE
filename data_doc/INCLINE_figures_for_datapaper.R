#### Code to make figures for data paper ####

# Use the INCLINE_download_read_cleandata to download from OSF

# read in data
INCLINE_metadata <- read_csv2("data_cleaned/INCLINE_metadata.csv")
demography_Sib_pro <- read_csv("data_cleaned/INCLINE_demography_Sib_pro.csv", col_types = "fffdDfffcffddfffd")
demography_Ver_alp <- read_csv("data_cleaned/INCLINE_demography_Ver_alp.csv", col_types = "fffdDfffcffddfffd")
seedling_data_subalpine <- read_csv("data_cleaned/INCLINE_seedling_data_subalpine.csv", col_types = "ffffffcfdddfddDf")
ndvi <- read_csv("data_cleaned/INCLINE_NDVI_2019_2020_2021.csv", col_types = "fdDffffcc")

# biomass removal and climate figure is in another location

#

datapaper_demo_SP_plot <- demography_Sib_pro %>% 
  filter(demographic_trait == "leaf_stalk_length") %>% 
  mutate(siteID = factor(siteID, levels = c("Ulvehaugen", "Lavisdalen", "Gudmedalen", "Skjellingahaugen"))) %>% 
  ggplot(aes(x = log(demographic_value), fill = OTC)) +
  geom_density(alpha = 0.3) +
  facet_wrap(treatment~siteID) +
  scale_fill_manual(values = c("#AFAFAF", "#7F0E0E")) +
  theme_bw()

datapaper_demo_VA_plot <- demography_Ver_alp %>% 
  filter(demographic_trait == "shoot_height") %>% 
  mutate(siteID = factor(siteID, levels = c("Ulvehaugen", "Lavisdalen", "Gudmedalen", "Skjellingahaugen"))) %>% 
  ggplot(aes(x = log(demographic_value), fill = OTC)) +
  geom_density(alpha = 0.3) +
  facet_wrap(treatment~siteID) +
  scale_fill_manual(values = c("#AFAFAF", "#7F0E0E")) +
  theme_bw()

datapaper_seedling_plot <- seedling_data_subalpine %>% 
  ggplot(aes(x = OTC, y = total_seedlings_survived, fill = OTC)) +
  geom_violin() +
  facet_wrap(species~vegetation) +
  scale_fill_manual(values = c("#AFAFAF", "#7F0E0E")) +
  theme_bw()

datapaper_ndvi_plot <- ndvi %>% 
  left_join(INCLINE_metadata, by = "plotID") %>% 
  filter(!is.na(siteID.y)) %>% 
  mutate(siteID.y = factor(siteID.y, levels = c("Ulvehaugen", "Lavisdalen", "Gudmedalen", "Skjellingahaugen"))) %>% 
  ggplot(aes(x = OTC, y = NDVI, fill = OTC)) +
  geom_violin() +
  facet_wrap(treatment~siteID.y) +
  scale_fill_manual(values = c("#AFAFAF", "#7F0E0E")) +
  theme_bw()

