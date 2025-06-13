library(dataDownloader)
library(fluxible)
library(tidyverse)
library(fs)
library(ggpmisc)

# library(lubridate)
# library(broom)
# library(zoo)

# source("https://raw.githubusercontent.com/jogaudard/common/master/fun-fluxes.R")

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

raw_CO2_INCLINE_2022 <- dir_ls(location, regexp = "*CO2_campaign*")  |>
  map_dfr(read_csv,  na = c("#N/A", "Over")) |>
  rename( #rename the column to get something more practical without space
    f_conc = "CO2 (ppm)",
    temp_air = "Temp_air ('C)",
    temp_soil = "Temp_soil ('C)",
    PAR = "PAR (umolsm2)",
    f_datetime = "Date/Time"
  ) %>%  
  mutate(
    f_datetime = dmy_hms(f_datetime)
  ) %>%
  select(f_datetime, f_conc, PAR, temp_air, temp_soil)

record <- read_csv("data/C-Flux/summer_2022/raw_data/INCLINE_field-record_2022.csv", na = c(""), col_types = "fffccfc") %>% 
  drop_na(starting_time) %>%  #delete row without starting time (meaning no measurement was done)
  mutate(
    starting_time = case_when(
      campaign != 1 ~ gsub("(\\d{2})(?=\\d{2})", "\\1:", starting_time, perl = TRUE), # campaing 1 was written as hh:mm:ss and others as hhmmss
      campaign == 1 ~ starting_time
      ),
    start = paste(date, starting_time),
    start = ymd_hms(start)
  )

str(raw_CO2_INCLINE_2022)
str(record)

# match fluxes and CO2 concentration --------------------------------------

CO2_INCLINE_2022 <- flux_match(
  raw_CO2_INCLINE_2022,
  record,
  f_datetime,
  start,
  measurement_length = 180
) |>
filter(
  !(f_fluxid %in% c(760:772)) # those missing data points are annoying
)

# CO2_INCLINE_2022  |>
#   filter(
#     f_fluxid %in% c(760:772) 
#   ) |>
#     view()
# those measurements where done in the 60 minutes before I fell in the river with the setup. which is probably why the data are missing.




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

slopes_INCLINE_2022 <- CO2_INCLINE_2022 |>
  flux_fitting(
    fit_type = "exp_zhao18",
    start_cut = 10,
    end_cut = 10
    )

str(slopes_INCLINE_2022)

# quality checks

slopes_INCLINE_2022_flags <- slopes_INCLINE_2022 |>
  flux_quality(
    force_zero = c(
      107 # the slope reflects a small bump that is not representing the entire flux
    ),
    force_lm = c(
      132, # lm is fine
      383,
      402,
      435,
      482, # small bump at the start affecting the slope
      662,
      671,
      674,
      873
    ),
    force_ok = c(
      359, # there are just a couple of outliers data points messing up the RMSE but the fit is ok
      378, # same, some outliers
      379, # same, outlisers issue
      384, # outliers issue
      407, # outlier at the start triggering start error
      409, # outliers issue
      422, # noise in the second part is affecting RMSE but not the fit
      673, # outliers issue
      786, # outliers affecting RMSE but slope good
      867 # noisy but slope quite obvious
      )
    )

# plotting to check the data

# plotting is passed as comments because it takes very long to run and we have checked them already

# slopes_INCLINE_2022_flags |>
#   filter(campaign == 1) |>
#   flux_plot(
#     f_ylim_lower = 300,
#     output = "pdfpages",
#     f_plotname = "campaign1"
#   )

# slopes_INCLINE_2022_flags |>
#   filter(campaign == 2) |>
#   flux_plot(
#     f_ylim_lower = 300,
#     output = "pdfpages",
#     f_plotname = "campaign2"
#   )

# slopes_INCLINE_2022_flags |>
#   filter(campaign == 3) |>
#   flux_plot(
#     f_ylim_lower = 300,
#     output = "pdfpages",
#     f_plotname = "campaign3"
#   )

# slopes_INCLINE_2022_flags |>
#   filter(campaign == 4) |>
#   flux_plot(
#     f_ylim_lower = 300,
#     output = "pdfpages",
#     f_plotname = "campaign4"
#   )

# flux_plot(
#   slopes_INCLINE_2022_flags,
#   # fit_type = "exp",
#   f_ylim_lower = 300,
#   output = "pdfpages",
#   print_plot = FALSE
#   )




# clean cut ---------------------------------------------------------------

# this is now inlcuded in flux_calc
# slopes_INCLINE_2022_cut <- slopes_INCLINE_2022 %>% 
#   filter(
#     cut == "keep"
#   )


# PAR cleaning ------------------------------------------------------------

slopes_INCLINE_2022_flags <- slopes_INCLINE_2022_flags %>% 
  mutate(
    PAR =
      case_when(
        type == "ER" & PAR < 0 ~ 0,
        type == "LRC" & PAR < 0 ~ 0,
        type == "NEE" & PAR < 60 ~ NA_real_, # PAR sensor had faulty contact
        TRUE ~ PAR
      )
  )

filter(slopes_INCLINE_2022_flags,
       type == "NEE"
       ) %>% #faster than looking at the graph!
  summarise(
    rangePAR = range(PAR, na.rm = TRUE)
  )



slopes_INCLINE_2022_flags %>% 
  filter(
    # type == "ER"
     f_fluxid == 792
    # & PAR < 10
  ) %>% 
  mutate(
    datetime = ymd_hms(f_datetime),
    time = hms::as_hms(f_datetime)
  ) %>% 
  ggplot(aes(x = time, y = PAR)) +
  geom_point() +
  geom_text(aes(label = f_fluxid), hjust=-1,vjust=1)

slopes_INCLINE_2022_flags %>% 
  filter(
    type == "NEE"
    # & fluxID == 697
    & PAR < 100
  ) %>% 
  mutate(
    datetime = ymd_hms(f_datetime),
    time = hms::as_hms(f_datetime)
  ) %>% 
  ggplot(aes(x = time, y = PAR)) +
  geom_point() +
  geom_text(aes(label = f_fluxid), hjust=-1,vjust=1) +
  facet_wrap(~campaign)



# calculate fluxes --------------------------------------------------------

fluxes_INCLINE_2022 <- slopes_INCLINE_2022_flags |>
    flux_calc(
      slope_col = f_slope_corr,
      temp_air_col = temp_air,
      setup_volume = 35,
      plot_area = 0.0875,
      atm_pressure = 1,
      conc_unit = "ppm",
      flux_unit = "mmol",
      cols_keep = c("turfID", "treatment", "type", "campaign", "comments", "f_quality_flag", "f_RMSE"),
      cols_ave = c("PAR", "temp_soil")
    )


str(fluxes_INCLINE_2022)

# fluxes_INCLINE_2022 <- slopes_INCLINE_2022_cut %>% 
#   mutate(
#     slope = case_when(
#       flag == "ok" & type == "ER" & slope_tz < 0 ~ NA_real_, # maybe this should be NA or 0, not sure
#       flag == "ok" ~ slope_tz,
#       flag == "zero" ~ 0,
#       flag %in% c("discard", "start_error", "weird_flux") ~ NA_real_
#     )
#   ) %>%
#   flux.calc.zhao18(
#     chamber_volume = 35, plot_area = 0.0875
#   ) # need to change dimension of chamber
  

# adding metadata

INCLINE_metadata <- read_csv2("data/C-Flux/summer_2022/raw_data/INCLINE_metadata.csv")

fluxes_INCLINE_2022 <- fluxes_INCLINE_2022 %>% 
  select(f_fluxid, PAR_ave, temp_soil_ave, turfID, type, f_datetime, campaign, f_flux, f_temp_air_ave, f_quality_flag) %>% 
  left_join(INCLINE_metadata)

# graph ER and NEE to detect outliers --------------------------------------------

fluxes_INCLINE_2022 %>% 
  mutate(
    datetime = ymd_hms(f_datetime),
    time = hms::as_hms(f_datetime)
  ) %>% 
  ggplot(aes(time, f_flux, color = type)) +
  geom_point() +
  facet_wrap(~campaign)

# light response curves and NEE correction --------------------------------

# lrc_INCLINE_2022 <- fluxes_INCLINE_2022 %>% 
#   filter(
#     type == "LRC"
#   )

# lrc_INCLINE_2022 %>% 
#   ggplot(aes(PARavg, flux, color = OTC)) +
#   geom_point() +
#   geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE) +
#   # facet_grid(siteID ~ campaign, scales = "free")
#   facet_grid(~siteID, scales = "free")

# fluxes_INCLINE_2022 <- LRC.calc(
#   lrc_INCLINE_2022,
#   fluxes_INCLINE_2022,
#   group = c("siteID", "OTC"),
#   PARfix = 300,
#   PARnull = 0
# )

fluxes_INCLINE_2022 <- fluxes_INCLINE_2022 |>
  mutate(
    type = case_when(
      campaign == "LRC" ~ "LRC",
      .default = type
    )
  )

fluxes_INCLINE_2022_par <- fluxes_INCLINE_2022 |>
  flux_lrc(
    type_col = type,
    par_ave = PAR_ave,
    lrc_group = "OTC"
  )

# calculate GPP -----------------------------------------------------------

# fluxes_INCLINE_2022_gep_meta <- GEP.calc2(fluxes_INCLINE_2022)

# fluxes_INCLINE_2022_gep_meta <- fluxes_INCLINE_2022_gep %>% 
#   left_join(INCLINE_metadata) #we loose the metadata when calculating GEP

fluxes_INCLINE_2022_gpp <- flux_gpp(
  fluxes_INCLINE_2022_par,
  type,
  f_datetime,
  id_cols = c("turfID", "campaign"),
  cols_keep = "all"
)


# graph fluxes ------------------------------------------------------------

fluxes_INCLINE_2022_gpp %>% 
  filter(
    type %in% c("ER", "GPP")
  ) %>% 
  ggplot(aes(f_datetime, PAR_corrected_flux, color = siteID, linetype = OTC)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE) +
  geom_hline(yintercept=0, linewidth = 0.3) +
  facet_grid(type ~ ., scales = "free")

fluxes_INCLINE_2022_gpp %>% 
  filter(
    type %in% c("ER", "GPP")
  ) %>% 
  ggplot(aes(`precipitation_2009-2019`, PAR_corrected_flux, linetype = OTC)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE) +
  geom_hline(yintercept=0, linewidth = 0.3) +
  facet_grid(type ~ ., scales = "free")

fluxes_INCLINE_2022_gpp %>% 
  filter(
    type %in% c("ER", "GPP")
  ) %>% 
  ggplot(aes(f_datetime, f_flux, color = siteID, linetype = OTC)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  geom_hline(yintercept=0, linewidth = 0.3) +
  facet_grid(type ~ ., scales = "free")

fluxes_INCLINE_2022_gpp %>% 
  filter(
    type %in% c("ER", "GPP")
  ) %>% 
  ggplot(aes(`precipitation_2009-2019`, f_flux, linetype = OTC)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE) +
  geom_hline(yintercept=0, linewidth = 0.3) +
  facet_grid(type ~ ., scales = "free")



# writing csv -------------------------------------------------------------
# getting rid of meta data

fluxes_INCLINE_2022_gpp <- fluxes_INCLINE_2022_gpp %>% 
  select(f_datetime, campaign, plotID, PAR_ave, type, f_flux, PAR_corrected_flux, temp_soil_ave, f_temp_air_ave, f_RMSE)

write_csv(fluxes_INCLINE_2022_gpp, "data_cleaned/INCLINE_c-flux_2022.csv")



# comparison

get_file(node = "zhk3m",
         file = "INCLINE_c-flux_2022_old.csv",
         path = "data_cleaned",
         remote_path = "C-Flux")

flux_incline_old <- read_csv("data_cleaned/INCLINE_c-flux_2022_old.csv")

flux_incline_old <- flux_incline_old |>
  filter(type %in% c("NEE", "ER")) |>
  select(plotID, type, campaign, flux) |>
  rename(flux_old = "flux") |>
  mutate(
    campaign = as.factor(campaign)
  )

fluxes_incline_new <- fluxes_INCLINE_2022_gpp |>
  filter(type %in% c("NEE", "ER")) |>
  select(plotID, type, campaign, f_flux, PAR_corrected_flux)

fluxes_comparison <- full_join(
  flux_incline_old,
  fluxes_incline_new,
  by = join_by(plotID, type, campaign)
) |>
pivot_longer(
  cols = c(flux_old, PAR_corrected_flux),
  names_to = "method",
  values_to = "flux"
)

fluxes_comparison |>
  ggplot(aes(f_flux, flux, color = method)) +
  geom_point(size = 0.5) +
  # geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE) +
  geom_abline(slope = 1, intercept = 0) +
  stat_poly_line() +
  stat_correlation(use_label("cor.label", "R2", "n")) +
  facet_grid(type ~ .)

fluxes_comparison |>
  filter(method == "flux_old") |>
  drop_na(f_flux) |>
  mutate(
    diff = case_when(
      f_flux != 0 ~ (flux - f_flux) / (f_flux)
    )
  ) |>
  summarise(
    .by = type,
    mean = mean(diff, na.rm = TRUE)
  )


fluxes_comparison |>
  filter(method == "flux_old") |>
  summarise(
    .by = type,
    mean_oldflux = mean(flux, na.rm = TRUE),
    mean_fflux = mean(f_flux, na.rm = TRUE)
  ) |>
  mutate(
    diff = (mean_oldflux - mean_fflux) / abs(mean_fflux)
  )
