library(dataDownloader)
library(tidyverse)
library(fs)
library(lubridate)
library(broom)

source("https://raw.githubusercontent.com/jogaudard/common/master/fun-fluxes.R")

# download and read data -----------------------------------------------------------
get_file(node = "zhk3m",
         file = "INCLINE_CO2_2022.zip",
         path = "data/C-Flux/summer_2022/raw_data",
         remote_path = "RawData/C-Flux")

get_file(node = "zhk3m",
         file = "INCLINE_field-record_2022.csv",
         path = "data/C-Flux/summer_2022/raw_data",
         remote_path = "RawData/C-Flux")

# Unzip files
zipfile <- "data/C-Flux/summer_2022/raw_data/INCLINE_CO2_2022.zip"
if(file.exists(zipfile)){
  outdir <- "data/C-Flux/summer_2022/raw_data"
  unzip(zipfile, exdir = outdir)
}

#importing fluxes data
location <- "data/C-Flux/summer_2022/raw_data" #location of datafiles

raw_CO2_INCLINE_2022 <-
  dir_ls(location, regexp = "*CO2_campaign*") %>% 
  map_dfr(read_csv,  na = c("#N/A", "Over"))
  # rename( #rename the column to get something more practical without space
  #   CO2 = "CO2 (ppm)",
  #   temp_air = "Temp_air ('C)",
  #   temp_soil = "Temp_soil ('C)",
  #   PAR = "PAR (umolsm2)",
  #   datetime = "Date/Time"
  # ) %>%  
  # mutate(
  #   datetime = dmy_hms(datetime)
  # ) %>%
  # select(datetime, CO2, PAR, temp_air, temp_soil)

record <- read_csv("data/C-Flux/summer_2022/raw_data/INCLINE_field-record_2022.csv", na = c(""), col_types = "fffccfc") %>% 
  drop_na(starting_time) %>%  #delete row without starting time (meaning no measurement was done)
  mutate(
    starting_time = case_when(
      campaign != 1 ~ gsub("(\\d{2})(?=\\d{2})", "\\1:", starting_time, perl = TRUE), # campaing 1 was written as hh:mm:ss and others as hhmmss
      campaign == 1 ~ starting_time
      )
  )


# match fluxes and CO2 concentration --------------------------------------

CO2_INCLINE_2022 <- match.flux4(raw_CO2_INCLINE_2022,
                                record,
                                window_length = 180,
                                startcrop = 0,
                                measurement_length = 180,
                                time_format = "time",
                                date_format = "ymd"
                                )

# detecting transcript mistakes by checking the length of each measurement

mistakes <- CO2_INCLINE_2022 %>% 
  group_by(fluxID, turfID, start, type) %>% 
  # nest() %>% 
  summarise(
    length = length(datetime)
  ) %>% 
  ungroup() %>% 
  arrange(start) %>% 
  mutate(
    flag = case_when(
      lag(length) < 181 ~ "flag",
      length < 181 ~ "flag"
    )
  ) %>% 
  filter(
    flag == "flag"
  )

# get slopes --------------------------------------------------------------

slopes_INCLINE_2022 <- CO2_INCLINE_2022 %>% 
  filter(
    datetime > start_window &
      datetime < end_window
  ) %>% 
  fitting.flux()

slopes_INCLINE_2022_metrics <- slopes_INCLINE_2022%>% 
  select(fluxID, b, b_est, RMSE, r.squared_slope, flag, cor_coef) %>% 
  distinct()

# graph CO2 concentration ------------------------------------------------------------
theme_set(theme_grey(base_size = 5))

slopes_INCLINE_2022 %>% 
  filter(
    campaign == 1
  ) %>%
  ggplot(aes(datetime)) +
  geom_point(aes(y = CO2, color = cut), size = 0.2) +
  geom_line(aes(y = fit), linetype = "longdash") +
  geom_line(aes(y = fit_slope, color = flag), linetype = "dashed") +
  scale_color_manual(values = c(
    "keep" = "green",
    "cut" = "red",
    "ok" = "black",
    "discard" = "red",
    "zero" = "grey",
    "start_error" = "red"
  )) +
  scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
  # ylim(400,800) +
  facet_wrap(~fluxID, scales = "free")

ggsave("campaign1.png", height = 60, width = 100, units = "cm", path = "data/C-Flux/summer_2022/graph_fluxes")

# calculate fluxes --------------------------------------------------------


# graph fluxes over the season --------------------------------------------


