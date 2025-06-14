# to clean NVDVI data
library(tidyverse)
library(lubridate)
library(dataDownloader)

get_file(node = "zhk3m",
         file = "INCLINE_NDVI_2020.csv",
         path = "data",
         remote_path = "RawData/NDVI")

get_file(node = "zhk3m",
         file = "NDVI_2019_2021.csv",
         path = "data",
         remote_path = "RawData/NDVI")

get_file(node = "zhk3m",
         file = "INCLINE_NDVI_2022.csv",
         path = "data",
         remote_path = "RawData/NDVI")

NDVI_2020 <- read_csv("data/INCLINE_NDVI_2020.csv") %>% 
  select(plot_ID, NDVI, date, replicate, LOGGER, OTC) %>% 
  rename(
    plotID = "plot_ID",
    tomst = "LOGGER"
  ) %>% 
  mutate(
    date = dmy(date),
    replicate = factor(replicate),
    site = str_replace_all(plotID, c(
        "LAV...." = "Lavisdalen",
        "GUD...." = "Gudmedalen",
        "SKJ...." = "Skjelingahaugen",
        "ULV...." = "Ulvhaugen"
    )),
    plotID = paste(substring(plotID, 1, 1), tolower(substring(plotID, 2, 7)), sep = "")
  )

NDVI_2019_2021 <- read_csv2("data/NDVI_2019_2021.csv") %>% 
  select(plot, site, date, NDVI_1, NDVI_2, NDVI_3, date_comment, comment) %>% 
  pivot_longer(c(NDVI_1, NDVI_2, NDVI_3), names_to = "replicate", values_to = "NDVI") %>% 
  mutate(
    date = dmy(date),
    # plot = toupper(plot),
    replicate = factor(str_extract(replicate, "(?<=.{5}).")),
    tomst = case_when(
      comment == "tomst" ~ TRUE
    ),
    site = str_replace_all(site, c(
      "Skjellinghaugen" = "Skjelingahaugen",
      "Ulvehaugen" = "Ulvhaugen"
    ))
  ) %>% 
  rename(
    plotID = "plot"
  )

NDVI_2022 <- read_csv("data/INCLINE_NDVI_2022.csv") %>% 
  select(turfID, replicate, NDVI, OTC, logger, date) %>% 
  rename(
    plotID = "turfID",
    tomst = "logger"
  ) %>% 
  mutate(
    # date = dmy(date),
    replicate = factor(replicate),
    site = str_replace_all(plotID, c(
      "Lav...." = "Lavisdalen",
      "Gud...." = "Gudmedalen",
      "Skj...." = "Skjelingahaugen",
      "Ulv...." = "Ulvhaugen"
    )),
    plotID = factor(plotID),
    site = factor(site)
  )

NDVI <- full_join(NDVI_2020, NDVI_2019_2021) %>% 
  full_join(NDVI_2022) %>%
  rename(
    comments = "comment",
    siteID = "site",
    OTC_removed = "OTC",
    tomst_presence = "tomst"
  ) %>% 
  mutate(
    OTC_removed = case_when(
      OTC_removed == "OFF" ~ TRUE,
      OTC_removed == "ON" ~ FALSE
    ),
    year = year(date)
  )

#visually checking values
ggplot(NDVI, aes(date, NDVI)) +
  geom_point() +
  facet_wrap(vars(siteID))

write_csv(NDVI, "data_cleaned/INCLINE_NDVI_2019_2020_2021_2022.csv")
