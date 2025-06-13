library(dataDownloader)
library(tidyverse)
library(lubridate)

get_file(node = "zhk3m",
         file = "INCLINE_soil-moisture_2022.csv",
         path = "data/INCLINE_microclimate",
         remote_path = "RawData/Climate")

get_file(node = "zhk3m",
         file = "INCLINE_metadata.csv",
         path = "data/C-Flux/summer_2022/raw_data",
         remote_path = "RawData")


soil_moisture <- read_csv("data/INCLINE_microclimate/INCLINE_soil-moisture_2022.csv")

INCLINE_metadata <- read_csv2("data/C-Flux/summer_2022/raw_data/INCLINE_metadata.csv")


# listing all the comments to be sure to not miss anything
soil_moisture %>%
  select(comments) %>% 
  distinct()

# above water table -> it is actually "above table" and has nothing to do with water table. It was written wrong in the dataset. Will be replaced with NA. Problem with calibration as I understand.
# snow covered -> NA
# frozen -> NA
# too stony -> NA



soil_moisture_avg <- soil_moisture %>% 
  mutate(
    comments = str_replace_all(comments, "above water table", "above table")
  ) %>% 
  group_by(turfID, campaign, comments, date) %>% 
  summarise(
    soil_moisture = mean(soil_moisture)
  )

soil_moisture_avg_meta <- soil_moisture_avg %>% 
  left_join(INCLINE_metadata)

write_csv(soil_moisture_avg, "data_cleaned/INCLINE_soil-moisture_2022.csv")


# we can make a graph for fun

soil_moisture_avg_meta %>% 
  ggplot(aes(`precipitation_2009-2019`, soil_moisture, color = OTC)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE)

soil_moisture_avg_meta %>% 
  ggplot(aes(date, soil_moisture, color = OTC)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE) +
  facet_grid(siteID~.)
