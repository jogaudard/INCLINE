####################################################################
### Script for testing seed mass differences between populations ###
####################################################################

#### Libraries ####
library(tidyverse)
library(dataDownloader)
library(osfr)
library(lubridate)
library(multcompView)
library(patchwork)

#### Downloading data from OSF ####

#osf_auth(token = "get from my document every time you run the code") 

#### Load data ####

seed_mass <- read.delim("data/Germination/Seed_weight.csv", sep = ",", dec = ".")


#### Clean data ####

#Be aware that one of the batches of seeds contain a different number than the others. So we either need to calculate the weight of individual seeds by dividing by the number_of_seeds column, or filter out the one that has 33 seeds and not 50 seeds.

seed_mass <- seed_mass %>% 
  mutate(weight_per_seed = total_weight/number_of_seeds) %>% 
  mutate(siteID = as.factor(siteID),
         species = as.factor(species)) %>% 
  mutate(siteID = factor(siteID, levels = c("Skjellingahaugen", "Lavisdalen", "Ulvehaugen", "Gudmedalen"))) %>% 
  mutate(species = factor(species, levels = c("Sib_pro", "Ver_alp")))
  

#### Average seed weight in mg ####

seed_mass %>% group_by(species) %>% mutate(mean = mean(weight_per_seed) * 1000) %>% select(species, mean) %>% unique()

#### Testing for difference between species and populations ####

test <- glm(weight_per_seed ~ species*siteID, data = seed_mass)
summary(test)

# Tukey's Honestly Significant Difference test
letters.df <- data.frame(multcompLetters())

seed_mass_VA <- seed_mass %>% filter(species == "Ver_alp")
TukeyHSD(aov(weight_per_seed ~ siteID, data = seed_mass_VA))
letters.df_VA <- data.frame(multcompLetters(TukeyHSD(aov(weight_per_seed ~ siteID, data = seed_mass_VA))$siteID[,4])$Letters)
colnames(letters.df_VA)[1] <- "Letter"
letters.df_VA$siteID <- rownames(letters.df_VA) 

placement_VA <- seed_mass_VA %>% #We want to create a dataframe to assign the letter position.
  group_by(siteID) %>%
  summarise(quantile(weight_per_seed)[4])

colnames(placement_VA)[2] <- "Placement.Value"
letters.df_VA <- left_join(letters.df_VA, placement_VA) #Merge dataframes

seed_mass_SP <- seed_mass %>% filter(species == "Sib_pro")
TukeyHSD(aov(weight_per_seed ~ siteID, data = seed_mass_SP))
letters.df_SP <- data.frame(multcompLetters(TukeyHSD(aov(weight_per_seed ~ siteID, data = seed_mass_SP))$siteID[,4])$Letters)
colnames(letters.df_SP)[1] <- "Letter" 
letters.df_SP$siteID <- rownames(letters.df_SP)

placement_SP <- seed_mass_SP %>% #We want to create a dataframe to assign the letter position.
  group_by(siteID) %>%
  summarise(quantile(weight_per_seed)[4])

colnames(placement_SP)[2] <- "Placement.Value"
letters.df_SP <- left_join(letters.df_SP, placement_SP) #Merge dataframes


#### Plotting differences #####
Precip_palette <- c("#BAD8F7", "#89B7E1", "#2E75B6", "#213964")


seed_mass %>% 
  mutate(siteID = factor(siteID, levels = c("Ulvehaugen","Lavisdalen",  "Gudmedalen", "Skjellingahaugen"))) %>% 
  mutate(species = factor(species, levels = c("Ver_alp", "Sib_pro"))) %>% 
ggplot(aes(x = siteID, y = (weight_per_seed*1000), fill = siteID)) +
  geom_boxplot(alpha = 0.7)+
  geom_jitter(alpha = 0.3, width = 0.05)+
  facet_wrap(~species, nrow = 2, scales = "free_y") +
  theme_bw() +
  labs(y = "Seed mass per seed (mg)", x = "Populations") +
  scale_x_discrete(labels=c("Lavisdalen" = "1561 mm/year", "Ulvehaugen" = "1226 mm/year",
                            "Gudmedalen" = "2130 mm/year", "Skjellingahaugen" = "3402 mm/year")) +
  #scale_fill_manual(values = c("#6D64E4", "#FFC300"))
  scale_fill_manual(values = c("#BAD8F7", "#89B7E1", "#2E75B6", "#213964")) +
  guides(fill = "none") +
  stat_compare_means(method = "anova")

seed_mass_VA_plot <- seed_mass_VA %>% 
  mutate(siteID = factor(siteID, levels = c("Ulvehaugen","Lavisdalen",  "Gudmedalen", "Skjellingahaugen"))) %>% 
  ggplot(aes(x = siteID, y = (weight_per_seed*1000), fill = siteID)) +
  geom_boxplot(alpha = 0.7)+
  geom_jitter(alpha = 0.3, width = 0.1, height = 0)+
  #facet_wrap(~species, nrow = 2, scales = "free_y") +
  theme_bw() +
  labs(y = "Seed mass per seed (mg)", x = "", title = "Veronica alpina") +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 13)) +
  scale_x_discrete(labels=c("Lavisdalen" = "1561 mm/year", "Ulvehaugen" = "1226 mm/year",
                            "Gudmedalen" = "2130 mm/year", "Skjellingahaugen" = "3402 mm/year")) +
  #scale_fill_manual(values = c("#6D64E4", "#FFC300"))
  scale_fill_manual(values = c("#BAD8F7", "#89B7E1", "#2E75B6", "#213964")) +
  guides(fill = "none") +
  geom_text(data = letters.df_VA, aes(x = siteID, y = max(Placement.Value*1000) + 0.0025, label = Letter), size = 4, color = "black", hjust = -1.25, vjust = -0.8, fontface = "bold")

seed_mass_SP_plot <- seed_mass_SP %>% 
  mutate(siteID = factor(siteID, levels = c("Ulvehaugen","Lavisdalen",  "Gudmedalen", "Skjellingahaugen"))) %>% 
  ggplot(aes(x = siteID, y = (weight_per_seed*1000), fill = siteID)) +
  geom_boxplot(alpha = 0.7)+
  geom_jitter(alpha = 0.3, width = 0.1, height = 0)+
  #facet_wrap(~species, nrow = 2, scales = "free_y") +
  theme_bw() +
  labs(y = "Seed mass per seed (mg)", x = "Populations", title = "Sibbaldia procumbens") +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 13)) +
  scale_x_discrete(labels=c("Lavisdalen" = "1561 mm/year", "Ulvehaugen" = "1226 mm/year",
                            "Gudmedalen" = "2130 mm/year", "Skjellingahaugen" = "3402 mm/year")) +
  #scale_fill_manual(values = c("#6D64E4", "#FFC300"))
  scale_fill_manual(values = c("#BAD8F7", "#89B7E1", "#2E75B6", "#213964")) +
  guides(fill = "none") +
  geom_text(data = letters.df_SP, aes(x = siteID, y = max(Placement.Value*1000) + 0.01, label = Letter), size = 4, color = "black", hjust = -1.25, vjust = -0.8, fontface = "bold")

plot <- (seed_mass_VA_plot /
  seed_mass_SP_plot )

ggsave(plot, filename = "Seed_mass_plot.pdf", width = 14, height = 18, units = "cm", dpi = 300)
                                           