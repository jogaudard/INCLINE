library(tidyverse)
library(dataDownloader)

get_file(node = "zhk3m",
         file = "INCLINE_metadata.csv",
         path = "data",
         remote_path = "RawData")

meta <- read_csv2("data/INCLINE_metadata.csv") %>% 
  filter(treatment != "R") %>% 
  select(turfID)

write_csv(meta,"meta_field.csv")
