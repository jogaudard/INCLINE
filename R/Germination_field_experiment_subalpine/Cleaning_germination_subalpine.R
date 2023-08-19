library(lme4)
library(tidyverse)
library(dplyr)
library(broom)
library(dataDownloader)

# Downloading raw data
get_file(node = "zhk3m",
         file = "INCLINE_germination_subalpine.csv",
         path = "raw_data",
         remote_path = "RawData/Germination_subalpine")

# Reading in raw data
SeedScoring <- read.csv("raw_data/INCLINE_germination_subalpine.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, fileEncoding = 'UTF-8-BOM') %>% mutate(Species = substr(Species, start = 1, stop = 7))

# Fixing blockID to siteID_blockID (e.g. Gud_1), and then fixing siteID to full name of site. Fixing OTC to C and W and Vegetation to Veg and NoVeg.
SeedScoring <- SeedScoring %>%  
  mutate(SiteID = case_when(SiteID == "LAV" ~ "Lav",
                          SiteID == "ULV" ~ "Ulv",
                          SiteID == "GUD" ~ "Gud",
                          SiteID == "SKJ" ~ "Skj"))

SeedScoring$blockID <- paste(SeedScoring$SiteID,SeedScoring$BlockID,sep="_")

SeedScoring <- SeedScoring %>%  
  mutate(siteID = case_when(SiteID == "Lav" ~ "Lavisdalen",
                            SiteID == "Ulv" ~ "Ulvehaugen",
                            SiteID == "Gud" ~ "Gudmedalen",
                            SiteID == "Skj" ~ "Skjellingahaugen")) %>% 
  mutate(OTC = case_when(OTC == "OTC" ~ "W",
                         OTC == "C" ~ "C")) %>%
  mutate(vegetation = case_when(Vegetation == "0" ~ "NoVeg",
                                Vegetation == "1" ~ "Veg"))
                            
# Adding column with total number of seeds sown in each plot of each species.
SeedScoring <- SeedScoring %>%
  group_by(SiteID, BlockID, OTC, Vegetation, Species) %>%
  mutate(TotSpeciesBlock=n()) %>%
  ungroup()

# Adding column with whether seed germinated
SeedScoring$Germination <- ifelse(SeedScoring$Date1==1,1, 
                                  ifelse(SeedScoring$Date2==1,1,
                                  ifelse(SeedScoring$Date3==1,1,
                                  ifelse(SeedScoring$Date4==1,1,
                                  ifelse(SeedScoring$Date5==1,1,
                                  ifelse(SeedScoring$Date6==1,1,
                                  ifelse(SeedScoring$Date7==1,1,0)))))))

# Adding column with total number of seeds emerged of each species in each plot
SeedScoring <- SeedScoring %>%
  group_by(SiteID, BlockID, OTC, Vegetation, Species) %>%
  mutate(TotEmerged = sum(Germination, na.rm = TRUE))%>%
  ungroup()

# Adding column with proportion emerged of each species in each plot
SeedScoring <- SeedScoring %>% 
  mutate(ProportionEmerged = TotEmerged/TotSpeciesBlock)

# Adding column with plotID
SeedScoring <- SeedScoring %>% mutate(plot_seedling_ID = paste(blockID, OTC, vegetation, sep = "_"))

# Adding column with plot_seedling_ID with species
SeedScoring$PlotSpecies <- paste(SeedScoring$plot_seedling_ID,SeedScoring$Species,sep="_")

# Adding column with survival at last date of scoring.
SeedScoring <- SeedScoring %>%
  group_by(plot_seedling_ID, Species) %>%
  mutate(SurvivedPlot = sum(Date7)) %>%
  ungroup()

# Adding column with proportion survived of total emerged.
SeedScoring <- mutate(SeedScoring, ProportionSurvived = SurvivedPlot/TotEmerged)

# Adding uniqueID for each seedling.
SeedScoring$observation <- 1:nrow(SeedScoring)
SeedScoring$uniqueID <- paste(SeedScoring$Species,SeedScoring$blockID, SeedScoring$OTC, SeedScoring$vegetation, SeedScoring$observation, sep="_")

# Adding column of number of not emerged of sown out and not survived of emerged.
SeedScoring$NotSurvived <- (SeedScoring$TotEmerged - SeedScoring$SurvivedPlot)

SeedScoring$NotEmerged <- (SeedScoring$TotSpeciesBlock - SeedScoring$TotEmerged)

# Removing rows of duplicated plots for each species
SeedScoring <- SeedScoring[!duplicated(SeedScoring$PlotSpecies),]

# Making long dataframe
SeedScoring <- gather(SeedScoring, Date, present, "Date1":"Date7", factor_key = TRUE)

# Adding correct dates at each site
SeedScoring <- SeedScoring %>% 
  mutate(date = if_else(SiteID == "Skj" &  Date == "Date1", "2021-06-07",
              if_else(SiteID == "Skj" &  Date == "Date2", "2021-06-22",
              if_else(SiteID == "Skj" &  Date == "Date3", "2021-07-05",
              if_else(SiteID == "Skj" &  Date == "Date4", "2021-07-18",
              if_else(SiteID == "Skj" &  Date == "Date5", "2021-08-02",
              if_else(SiteID == "Skj" &  Date == "Date6", "2021-08-16",
              if_else(SiteID == "Skj" &  Date == "Date7", "2021-08-28",
              if_else(SiteID == "Ulv" &  Date == "Date1", "2021-06-08",
              if_else(SiteID == "Ulv" &  Date == "Date2", "2021-06-23",
              if_else(SiteID == "Ulv" &  Date == "Date3", "2021-07-06",
              if_else(SiteID == "Ulv" &  Date == "Date4", "2021-07-19",
              if_else(SiteID == "Ulv" &  Date == "Date5", "2021-08-03",
              if_else(SiteID == "Ulv" &  Date == "Date6", "2021-08-16",
              if_else(SiteID == "Ulv" &  Date == "Date7", "2021-08-29",
              if_else(SiteID == "Gud" &  Date == "Date1", "2021-06-09",
              if_else(SiteID == "Gud" &  Date == "Date2", "2021-06-22",
              if_else(SiteID == "Gud" &  Date == "Date3", "2021-07-05",
              if_else(SiteID == "Gud" &  Date == "Date4", "2021-07-18",
              if_else(SiteID == "Gud" &  Date == "Date5", "2021-08-02",
              if_else(SiteID == "Gud" &  Date == "Date6", "2021-08-16",
              if_else(SiteID == "Gud" &  Date == "Date7", "2021-08-28",
              if_else(SiteID == "Lav" &  Date == "Date1", "2021-06-10",
              if_else(SiteID == "Lav" &  Date == "Date2", "2021-06-23",
              if_else(SiteID == "Lav" &  Date == "Date3", "2021-07-06",
              if_else(SiteID == "Lav" &  Date == "Date4", "2021-07-19",
              if_else(SiteID == "Lav" &  Date == "Date5", "2021-08-03",
              if_else(SiteID == "Lav" &  Date == "Date6", "2021-08-16",
              if_else(SiteID == "Lav" &  Date == "Date7", "2021-08-29",
                    "error"))))))))))))))))))))))))))))) %>% 
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

# Fixing present from 0 and 1 to yes and no
SeedScoring <- SeedScoring %>% 
  mutate(present = if_else(present == 0, "no",
                            "yes"))

# Renaming and selecting relevant columns
SeedScoring <- SeedScoring %>% 
  rename(species = Species,
         X = x,
         Y = y,
         total_seeds_sown = TotSpeciesBlock,
         germinated = Germination,
         total_seedlings_emerged = TotEmerged,
         total_seedlings_survived = SurvivedPlot,
         strategy = Strategy,
         species = Species,
         plotID = plot_seedling_ID) %>% 
  mutate(registrator = "IJD") %>%
  select(species, siteID, blockID, OTC, vegetation, plot_seedling_ID, uniqueID, strategy, X, Y, total_seeds_sown, germinated, total_seedlings_emerged, total_seedlings_survived, date, present)
         
# Saving the cleaned dataset
write.csv(SeedScoring, "C:/Users/ingri/OneDrive/Skrivebord/Master/Kode/INCLINE/Data/INCLINE_seedling_data_subalpine.csv")
         