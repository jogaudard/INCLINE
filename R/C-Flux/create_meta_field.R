library(tidyverse)
library(dataDownloader)

get_file(node = "zhk3m",
         file = "INCLINE_metadata.csv",
         path = "data",
         remote_path = "RawData")

meta <- read_csv2("data/INCLINE_metadata.csv") %>% 
  # filter(treatment != "R") %>% #remove R (removal) treatment for c-fluxes
  slice(rep(1:n(), each = 4)) %>% #c-flux 2 replicates, NDVI 3, soil moisture 4
  mutate(
    # turfID = paste(turfID, OTC, treatment, sep = "_") #for printing sheet, turfID includes the treatment for easier reading
    treatment = paste(OTC, treatment, sep = "_")
  ) %>% 
  select(turfID, treatment)

write_csv(meta,"meta_field.csv")
