# This script is to clean raw data from various loggers into one datafile with calculated fluxes
library(tidyverse)
# source("https://raw.githubusercontent.com/jogaudard/common/datadoc/fun-fluxes.R")
source("https://raw.githubusercontent.com/jogaudard/common/master/fun-fluxes.R")
library(lubridate, warn.conflicts = FALSE)
library(broom)
library(fs)
library("dataDownloader")


measurement <- 120 #the length of the measurement taken on the field in seconds
startcrop <- 30 #how much to crop at the beginning of the measurement in seconds
endcrop <- 10 #how much to crop at the end of the measurement in seconds

#download and unzip files from OSF
# get_file(node = "zhk3m",
#          file = "INCLINE_cflux_2020.zip",
#          path = "data/C-Flux/summer_2020",
#          remote_path = "RawData/C-Flux")
# raw data will be hosted by Three-D since there are the same (we don't like duplicates)

get_file(node = "pk4bg",
         file = "Three-D_cflux_2020.zip",
         path = "data/C-Flux/summer_2020",
         remote_path = "RawData/C-Flux")

get_file(node = "zhk3m",
         file = "INCLINE_field-record_2020.csv",
         path = "data/C-Flux/summer_2020",
         remote_path = "RawData/C-Flux")

# Unzip files
zipFile <- "data/C-Flux/summer_2020/Three-D_cflux_2020.zip"
if(file.exists(zipFile)){
  outDir <- "data/C-Flux/summer_2020"
  unzip(zipFile, exdir = outDir)
}

location <- "data/C-Flux/summer_2020/rawData" #location of datafiles
#import all squirrel files and select date/time and CO2_calc columns, merge them, name it fluxes
fluxes <-
  dir_ls(location, regexp = "*CO2*") %>% 
  map_dfr(read_csv,  na = c("#N/A", "Over")) %>% 
  rename(CO2 = "CO2 (ppm)") %>%  #rename the column to get something more practical without space
  mutate(
    date = dmy(Date), #convert date in POSIXct
    datetime = as_datetime(paste(date, Time))  #paste date and time in one column
  ) %>%
  select(datetime,CO2)

#import date/time and PAR columns from PAR file
PAR <-
  list.files(path = location, pattern = "*PAR*", full.names = T) %>% 
  map_df(~read_table2(., "", na = c("NA"), col_names = paste0("V",seq_len(12)))) %>% #need that because logger is adding columns with useless information
  rename(date = V2, time = V3, PAR = V4) %>% 
  mutate(
    PAR = as.numeric(as.character(.$PAR)), #removing any text from the PAR column (again, the logger...)
    datetime = paste(date, time),
    datetime = ymd_hms(datetime)
  ) %>% 
  select(datetime, PAR)

#import date/time and value column from iButton file

temp_air <-dir_ls(location, regexp = "*temp*") %>% 
  map_dfr(read_csv,  na = c("#N/A"), skip = 20, col_names = c("datetime", "unit", "temp_value", "temp_dec"), col_types = "ccnn") %>%
  mutate(temp_dec = replace_na(temp_dec,0),
         temp_air = temp_value + temp_dec/1000, #because stupid iButtons use comma as delimiter AND as decimal point
         datetime = dmy_hms(datetime)
  ) %>% 
  select(datetime, temp_air)


#join the df


combined <- fluxes %>% 
  left_join(PAR, by = "datetime") %>% 
  left_join(temp_air, by = "datetime")

#import the record file
# incline <- read_csv("data/C-Flux/summer_2020/INCLINE_field-record_2020.csv", na = c(""), col_types = "cccntcnc") %>%
#   drop_na(starting_time) %>% #delete row without starting time (meaning no measurement was done)
#   mutate(
#     date = dmy(date),
#     start = as_datetime(paste(date, starting_time)), #converting the date as posixct, pasting date and starting time together
#     # Datetime = Start, #useful for left_join
#     end = start + measurement - endcrop, #creating column End and cropping the end of the measurement
#     start = start + startcrop #cropping the start
#   ) %>%  
#   drop_na(starting_time) %>% #need to remove rows with NA in starting time: when running out of battery we took only two measurements per plot, but the replicates stayed in the record file
#   select(plot_ID,type,replicate,starting_time,date,campaign,remarks,start,end)

incline <- read_csv("data/C-Flux/summer_2020/INCLINE_field-record_2020.csv", na = c(""), col_types = "cccntcnc") %>% 
  drop_na(starting_time) %>% #delete row without starting time (meaning no measurement was done)
  mutate(
    date = dmy(date),
    start = ymd_hms(paste(date, starting_time)), #converting the date as posixct, pasting date and starting time together
    end = start + measurement, #creating column End
    start_window = start + startcrop, #cropping the start
    end_window = end - endcrop #cropping the end of the measurement
  )

#matching fluxes


co2_conc_incline <- match.flux(combined,incline)

#adjusting the time window
co2_conc_incline <- co2_conc_incline %>% mutate(
  cut = case_when(
    datetime <= start_window | datetime >= end_window ~ "cut",
    # ID ==  & datetime > ymd_hms("") ~ "cut",
    TRUE ~ "keep"
  ),
  cut = as_factor(cut)
)

# graph CO2 fluxes to visually check the data
# co2_conc_incline %>%
#   # filter(Date == "2020-08-06") %>%
#   ggplot(aes(x=datetime, y=CO2)) +
#   # geom_point(size=0.005) +
#   geom_line(size = 0.1, aes(group = ID)) +
#   # coord_fixed(ratio = 10) +
#   scale_x_datetime(date_breaks = "30 min") +
#   facet_wrap(vars(date), ncol = 1, scales = "free") +
#   # geom_line(size=0.05)
#   ggsave("incline.png", height = 40, width = 100, units = "cm")

#plot each flux to look into details what to cut off
ggplot(co2_conc_incline, aes(x = datetime, y = CO2, color = cut)) +
  geom_line(size = 0.2, aes(group = ID)) +
  scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M:%S") +
  # scale_x_date(date_labels = "%H:%M:%S") +
  facet_wrap(vars(ID), ncol = 36, scales = "free") +
  ggsave("incline_detail.png", height = 60, width = 126, units = "cm")


#graph CO2 fluxes to visually check the data
ggplot(co2_conc_incline, aes(x=datetime, y=CO2, color = cut)) + 
  # geom_point(size=0.005) +
  geom_line(size = 0.2, aes(group = ID)) +
  # coord_fixed(ratio = 10) +
  scale_x_datetime(date_breaks = "10 min", minor_breaks = "30 sec", date_labels = "%e/%m \n %H:%M:%S") +
  facet_wrap(vars(date), ncol = 1, scales = "free") +
  # geom_line(size=0.05)
  ggsave("incline.png", height = 40, width = 100, units = "cm")

#calculation of flux

#new function for that
flux.calc2 <- function(co2conc, # dataset of CO2 concentration versus time (output of match.flux)
                       chamber_volume = 24.5, # volume of the flux chamber in L, default for Three-D chamber (25x24.5x40cm)
                       tube_volume = 0.075, # volume of the tubing in L, default for summer 2020 setup
                       atm_pressure = 1, # atmoshperic pressure, assumed 1 atm
                       plot_area = 0.0625 # area of the plot in m^2, default for Three-D
)
{
  R = 0.082057 #gas constant, in L*atm*K^(-1)*mol^(-1)
  vol = chamber_volume + tube_volume
  # co2conc <- co2_cut
  slopes <- co2conc %>% 
    group_by(ID) %>% 
    mutate(
      time = difftime(datetime[1:length(datetime)],datetime[1] , units = "secs")
    ) %>% 
    select(ID, time, CO2) %>%
    do({model = lm(CO2 ~ time, data=.)    # create your model
    data.frame(tidy(model),              # get coefficient info
               glance(model))}) %>%          # get model info
    filter(term == "time") %>% 
    rename(slope = estimate) %>% 
    select(ID, slope, p.value, r.squared, adj.r.squared, nobs) %>% 
    ungroup()
  
  means <- co2conc %>% 
    group_by(ID) %>% 
    summarise(
      PARavg = mean(PAR, na.rm = TRUE), #mean value of PAR for each flux
      temp_airavg = mean(temp_air, na.rm = TRUE)  #mean value of temp_air for each flux
      + 273.15, #transforming in kelvin for calculation
    ) %>% 
    ungroup()
  
  fluxes_final <- left_join(slopes, means, by = "fluxID") %>% 
    left_join(
      co2conc,
      by = "ID"
    ) %>% 
    select(ID, slope, p.value, r.squared, adj.r.squared, nobs, PARavg, temp_airavg, plot_ID, type, campaign, remarks, start_window) %>% 
    distinct() %>% 
    rename(
      datetime = start_window
    ) %>% 
    mutate(
      flux = (slope * atm_pressure * vol)/(R * temp_airavg * plot_area) #gives flux in micromol/s/m^2
      *3600 #secs to hours
      /1000 #micromol to mmol
    ) %>% #flux is now in mmol/m^2/h, which is more common
    arrange(datetime) %>% 
    select(!slope)
  
  return(fluxes_final)
  
}

co2_flux_incline <- filter(co2_conc_incline, cut == "keep") %>% #cut out the discarded parts
  flux.calc2(co2_conc_incline, chamber_volume = 34.3, plot_area = 0.08575) %>% #need to specify the size of the chamber because it is different than Three-D
  rename(
    turfID = plot_ID
  )


write_csv(co2_flux_incline, "data/C-Flux/summer_2020/INCLINE_c-flux_2020.csv")

#make a freq hist about length of fluxes
ggplot(co2_flux_incline, aes(nobs)) +
  geom_bar() +
  scale_x_binned()

#to remove poor quality data
# co2_flux_incline_clean <- co2_flux_incline %>% 
#   filter(
#     ((p.value <= 0.05 & r.squared >= 0.7) |
#       (p.value >0.05 & r.squared <= 0.2)) &
#       nobs >= 60
#   )
# count(co2_flux_incline_clean)

# co2_flux_incline <- flux.calc(co2_conc_incline, chamber_volume = 34.3, plot_area = 0.08575) %>% #need to specify the size of the chamber because it is different than Three-D
#   rename(
#     plotID = plot_ID
#   ) %>% 
#   write_csv("data/C-Flux/summer_2020/INCLINE_c-flux_2020.csv")
