library(dataDownloader)
library(tidyverse)
library(fs)
library(lubridate)
library(broom)
library(zoo)

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

get_file(node = "zhk3m",
         file = "INCLINE_metadata.csv",
         path = "data/C-Flux/summer_2022/raw_data",
         remote_path = "RawData")

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

# mistakes <- CO2_INCLINE_2022 %>% 
#   group_by(fluxID, turfID, start, type) %>% 
#   # nest() %>% 
#   summarise(
#     length = length(datetime)
#   ) %>% 
#   ungroup() %>% 
#   arrange(start) %>% 
#   mutate(
#     flag = case_when(
#       lag(length) < 181 ~ "flag",
#       length < 181 ~ "flag"
#     )
#   ) %>% 
#   filter(
#     flag == "flag"
#   )


# comments and temperature cleaning ---------------------------------------

CO2_INCLINE_2022 %>%
  select(comments) %>% 
  distinct()

CO2_INCLINE_2022 <- CO2_INCLINE_2022 %>% 
  mutate(
    temp_soil = case_when(
      comments %in% c("soilT logger not plugged in", "no soil T") ~ NA_real_,
      TRUE ~ temp_soil
    )
  )

# get slopes --------------------------------------------------------------

slopes_INCLINE_2022 <- CO2_INCLINE_2022 %>% 
  filter(
    datetime > start_window &
      datetime < end_window
  ) %>% 
  fitting.flux_nocut2()
  # fitting.flux(
  #   weird_fluxesID = c(
  #     355, # second half was detected for window, but it should be the first one
  #     386, # tz is at the wrong end, it does not reflect the reality of the flux (negative ER)
  #     549, # slope_fit is too far from real points and window is too short
  #     640, # wrong direction of slope
  #     696, # dip in the flux
  #     719, # slope in wrong direction
  #     743, # window mismatch
  #     751, # slope in wrong direction
  #     762, # dip in flux
  #     775, # mess at the beginning of the flux
  #     791, # slope in the wrong direction
  #     876 # flux is quite weird
  #     )
  # )


# slopes_INCLINE_2022_metrics <- slopes_INCLINE_2022%>%
#   select(fluxID, Cm, b, b_est, RMSE, r.squared_slope, flag, cor_coef) %>%
#   distinct()

slopes_INCLINE_2022_metrics <- slopes_INCLINE_2022%>%
  select(fluxID, Cm, b, b_est, RMSE, flag, cor_coef) %>%
  distinct()

# graph CO2 concentration ------------------------------------------------------------
theme_set(theme_grey(base_size = 5))

slopes_INCLINE_2022 %>%
  filter(
    campaign == 1
  ) %>%
  ggplot(aes(datetime)) +
  # geom_point(aes(y = CO2, color = cut), size = 0.2) +
  geom_point(aes(y = CO2), size = 0.2) +
  geom_line(aes(y = fit), linetype = "longdash") +
  geom_line(aes(y = fit_slope, color = flag), linetype = "dashed") +
  scale_color_manual(values = c(
    # "keep" = "green",
    # "cut" = "red",
    "ok" = "green",
    "discard" = "red",
    "zero" = "grey",
    "start_error" = "red"
  )) +
  scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
  # ylim(400,800) +
  facet_wrap(~fluxID, scales = "free")

ggsave("campaign1b.png", height = 60, width = 100, units = "cm", path = "data/C-Flux/summer_2022/graph_fluxes")

gc()

slopes_INCLINE_2022 %>%
  filter(
    campaign == 2
  ) %>%
  ggplot(aes(datetime)) +
  # geom_point(aes(y = CO2, color = cut), size = 0.2) +
  geom_point(aes(y = CO2), size = 0.2) +
  geom_line(aes(y = fit), linetype = "longdash") +
  geom_line(aes(y = fit_slope, color = flag), linetype = "dashed") +
  scale_color_manual(values = c(
    # "keep" = "green",
    # "cut" = "red",
    "ok" = "green",
    "discard" = "red",
    "zero" = "grey",
    "start_error" = "red"
  )) +
  scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
  # ylim(400,800) +
  facet_wrap(~fluxID, scales = "free")

ggsave("campaign2b.png", height = 60, width = 100, units = "cm", path = "data/C-Flux/summer_2022/graph_fluxes")

gc()

slopes_INCLINE_2022 %>%
  filter(
    campaign == 3
  ) %>%
  ggplot(aes(datetime)) +
  # geom_point(aes(y = CO2, color = cut), size = 0.2) +
  geom_point(aes(y = CO2), size = 0.2) +
  geom_line(aes(y = fit), linetype = "longdash") +
  geom_line(aes(y = fit_slope, color = flag), linetype = "dashed") +
  scale_color_manual(values = c(
    # "keep" = "green",
    # "cut" = "red",
    "ok" = "green",
    "discard" = "red",
    "zero" = "grey",
    "start_error" = "red"
  )) +
  scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
  # ylim(400,800) +
  facet_wrap(~fluxID, scales = "free")

ggsave("campaign3b.png", height = 60, width = 100, units = "cm", path = "data/C-Flux/summer_2022/graph_fluxes")

gc()

slopes_INCLINE_2022 %>%
  filter(
    campaign == 4
  ) %>%
  ggplot(aes(datetime)) +
  # geom_point(aes(y = CO2, color = cut), size = 0.2) +
  geom_point(aes(y = CO2), size = 0.2) +
  geom_line(aes(y = fit), linetype = "longdash") +
  geom_line(aes(y = fit_slope, color = flag), linetype = "dashed") +
  scale_color_manual(values = c(
    # "keep" = "green",
    # "cut" = "red",
    "ok" = "green",
    "discard" = "red",
    "zero" = "grey",
    "start_error" = "red"
  )) +
  scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
  # ylim(400,800) +
  facet_wrap(~fluxID, scales = "free")

ggsave("campaign4b.png", height = 60, width = 100, units = "cm", path = "data/C-Flux/summer_2022/graph_fluxes")

gc()


# clean cut ---------------------------------------------------------------

slopes_INCLINE_2022_cut <- slopes_INCLINE_2022 %>% 
  filter(
    cut == "keep"
  )


# PAR cleaning ------------------------------------------------------------

slopes_INCLINE_2022_cut <- slopes_INCLINE_2022_cut %>% 
  mutate(
    PAR =
      case_when(
        type == "ER" & PAR < 0 ~ 0,
        type == "LRC" & PAR < 0 ~ 0,
        type == "NEE" & PAR < 60 ~ NA_real_, # PAR sensor had faulty contact
        TRUE ~ PAR
      )
  )

filter(slopes_INCLINE_2022_cut
       # type == "NEE"
       ) %>% #faster than looking at the graph!
  summarise(
    rangePAR = range(PAR, na.rm = TRUE)
  )

# slopes_INCLINE_2022_cut <- slopes_INCLINE_2022_cut %>% 
#   mutate(
#     PAR = case_when(
#       # fluxID %in% c(
#       #   691,
#       #   697,
#       #   695,
#       #   873,
#       #   137,
#       #   51,
#       #   273,
#       #   183,
#       #   585,
#       #   629,
#       #   475,
#       #   257,
#       #   583,
#       #   53,
#       #   275,
#       #   821,
#       #   627,
#       #   469,
#       #   285,
#       #   751,
#       #   749,
#       #   
#       #   ) & 
#         PAR < 60 ~ NA_real_, # PAR sensor had a faulty contact
#       TRUE ~ PAR
#       
#     )
#   )

slopes_INCLINE_2022_cut %>% 
  filter(
    # type == "NEE"
     fluxID == 772
    # & PAR < 10
  ) %>% 
  mutate(
    datetime = ymd_hms(datetime),
    time = hms::as_hms(datetime)
  ) %>% 
  ggplot(aes(x = time, y = PAR)) +
  geom_point() +
  geom_text(aes(label = fluxID), hjust=-1,vjust=1)

slopes_INCLINE_2022_cut %>% 
  filter(
    type == "NEE"
    # & fluxID == 697
    & PAR < 100
  ) %>% 
  mutate(
    datetime = ymd_hms(datetime),
    time = hms::as_hms(datetime)
  ) %>% 
  ggplot(aes(x = time, y = PAR)) +
  geom_point() +
  geom_text(aes(label = fluxID), hjust=-1,vjust=1) +
  facet_wrap(~campaign)



# calculate fluxes --------------------------------------------------------

fluxes_INCLINE_2022 <- slopes_INCLINE_2022_cut %>% 
  mutate(
    slope = case_when(
      flag == "ok" & type == "ER" & slope_tz < 0 ~ NA_real_, # maybe this should be NA or 0, not sure
      flag == "ok" ~ slope_tz,
      flag == "zero" ~ 0,
      flag %in% c("discard", "start_error", "weird_flux") ~ NA_real_
    )
  ) %>%
  flux.calc.zhao18(
    chamber_volume = 35, plot_area = 0.0875
  ) # need to change dimension of chamber
  

# adding metadata

INCLINE_metadata <- read_csv2("data/C-Flux/summer_2022/raw_data/INCLINE_metadata.csv")

fluxes_INCLINE_2022 <- fluxes_INCLINE_2022 %>% 
  select(fluxID, PARavg, temp_soilavg, turfID, type, datetime, campaign, flux, temp_airavg, RMSE) %>% 
  left_join(INCLINE_metadata)

# graph ER and NEE to detect outliers --------------------------------------------

fluxes_INCLINE_2022 %>% 
  mutate(
    datetime = ymd_hms(datetime),
    time = hms::as_hms(datetime)
  ) %>% 
  ggplot(aes(time, flux, color = type)) +
  geom_point() +
  facet_wrap(~campaign)

# light response curves and NEE correction --------------------------------

lrc_INCLINE_2022 <- fluxes_INCLINE_2022 %>% 
  filter(
    type == "LRC"
  )

# lrc_INCLINE_2022 %>% 
#   ggplot(aes(PARavg, flux, color = OTC)) +
#   geom_point() +
#   geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE) +
#   # facet_grid(siteID ~ campaign, scales = "free")
#   facet_grid(~siteID, scales = "free")

fluxes_INCLINE_2022 <- LRC.calc(
  lrc_INCLINE_2022,
  fluxes_INCLINE_2022,
  group = c("siteID", "OTC"),
  PARfix = 300,
  PARnull = 0
)

# calculate GPP -----------------------------------------------------------

fluxes_INCLINE_2022_gep_meta <- GEP.calc2(fluxes_INCLINE_2022)

# fluxes_INCLINE_2022_gep_meta <- fluxes_INCLINE_2022_gep %>% 
#   left_join(INCLINE_metadata) #we loose the metadata when calculating GEP


# graph fluxes ------------------------------------------------------------

fluxes_INCLINE_2022_gep_meta %>% 
  filter(
    type %in% c("ER", "GEP")
  ) %>% 
  ggplot(aes(datetime, PAR_corrected_flux, color = siteID, linetype = OTC)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  geom_hline(yintercept=0, size = 0.3) +
  facet_grid(type ~ ., scales = "free")

fluxes_INCLINE_2022_gep_meta %>% 
  filter(
    type %in% c("ER", "GEP")
  ) %>% 
  ggplot(aes(`precipitation_2009-2019`, PAR_corrected_flux, linetype = OTC)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE) +
  geom_hline(yintercept=0, size = 0.3) +
  facet_grid(type ~ ., scales = "free")

fluxes_INCLINE_2022_gep_meta %>% 
  filter(
    type %in% c("ER", "GEP")
  ) %>% 
  ggplot(aes(datetime, flux, color = siteID, linetype = OTC)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  geom_hline(yintercept=0, size = 0.3) +
  facet_grid(type ~ ., scales = "free")

fluxes_INCLINE_2022_gep_meta %>% 
  filter(
    type %in% c("ER", "GEP")
  ) %>% 
  ggplot(aes(`precipitation_2009-2019`, flux, linetype = OTC)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE) +
  geom_hline(yintercept=0, size = 0.3) +
  facet_grid(type ~ ., scales = "free")



# writing csv -------------------------------------------------------------
# getting rid of meta data

fluxes_INCLINE_2022_gep <- fluxes_INCLINE_2022_gep_meta %>% 
  select(datetime, campaign, plotID, PARavg, type, flux, PAR_corrected_flux, temp_soilavg, temp_airavg, RMSE)

write_csv(fluxes_INCLINE_2022_gep, "data_cleaned/INCLINE_c-flux_2022.csv")











