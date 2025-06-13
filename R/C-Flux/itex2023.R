library(dataDownloader)
library(tidyverse)
library(lubridate)
library(measurements)

get_file(node = "zhk3m",
         file = "INCLINE_c-flux_2022.csv",
         path = "data_cleaned",
         remote_path = "C-Flux")

get_file(node = "zhk3m",
         file = "INCLINE_soil-moisture_2022.csv",
         path = "data_cleaned",
         remote_path = "Climate")

get_file(node = "zhk3m",
         file = "INCLINE_metadata.csv",
         path = "data/C-Flux/summer_2022/raw_data",
         remote_path = "RawData")

INCLINE_metadata <- read_csv2("data/C-Flux/summer_2022/raw_data/INCLINE_metadata.csv")

fluxes <- read_csv("data_cleaned/INCLINE_c-flux_2022.csv") %>% 
  left_join(INCLINE_metadata)

soil_moisture_avg <- read_csv("data_cleaned/INCLINE_soil-moisture_2022.csv") #%>% 
  # mutate(
  #   year = year(date)
  # ) %>% 
  # select(turfID, year, soil_moisture)


# fluxes df compatible with ITEX ------------------------------------------


fluxes_itex <- fluxes %>% 
  filter(
    type == "ER"
    & treatment == "C"
  ) %>% 
  left_join(soil_moisture_avg, by = c("turfID", "campaign")) %>%
  mutate(
    date = date(datetime),
    OTC = str_replace_all(OTC, c("C" = "CTL", "W" = "OTC")),
    ITEX_ID = case_when(
      siteID == "Lavisdalen" ~ "NOR_9",
      siteID == "Gudmedalen" ~ "NOR_11",
      siteID == "Ulvehaugen" ~ "NOR_10",
      siteID == "Skjellingahaugen" ~ "NOR_12"
    ),
    flux = flux * 0.04401 # they want g CO2 / m2 / h
  ) %>% 
  select(plotID, siteID, date, OTC, temp_soilavg, flux, temp_airavg, ITEX_ID, soil_moisture, comments, RMSE) %>% 
  rename(
    treatment = "OTC"
  ) %>% 
  drop_na(date, flux) %>% 
  arrange(ITEX_ID) %>% 
  relocate(ITEX_ID, date, treatment, plotID, flux, RMSE, temp_airavg, temp_soilavg, soil_moisture, siteID, comments)

write_csv(fluxes_itex, "ITEX/ITEX_fluxes_2022.csv")


# cover dataset -----------------------------------------------------------

# we need
# mean plant height cm
# biomass g/cm2
# graminoids %
# forbs %
# deciduous shrubs %
# evergreen shrubs %
# mosses %
# lichens %



# get_file(node = "zhk3m",
#          file = "community_clean_med_NA.csv",
#          path = "data_cleaned",
#          remote_path = "Community")
# 
# get_file(node = "zhk3m",
#          file = "INCLINE_community_2018_2019_2021_2022.csv",
#          path = "data_cleaned",
#          remote_path = "RawData/Community")

# get_file(node = "zhk3m",
#          file = "community_clean_done_without_NA.csv",
#          path = "data_cleaned",
#          remote_path = "Community")

get_file(node = "zhk3m",
         file = "INCLINE_community_plotlevel_info.csv",
         path = "data_cleaned",
         remote_path = "Community")

get_file(node = "zhk3m",
         file = "INCLINE_community_species_cover.csv",
         path = "data_cleaned",
         remote_path = "Community")

# get_file(node = "zhk3m",
#          file = "INCLINE_biomass_removal.csv",
#          path = "data_cleaned",
#          remote_path = "Biomass_removal")

# community <- read_csv2("data_cleaned/community_clean_med_NA.csv")

# community <- read_csv2("data_cleaned/community_clean_done_without_NA.csv")

height <- read_csv("data_cleaned/INCLINE_community_plotlevel_info.csv")

height <- height %>% 
  filter(
    year == 2022
    # & treatment == "C"
  ) %>% 
  select(plotID, total_bryophyte_cover, total_lichen_cover, vegetation_height_mean)

community <- read_csv("data_cleaned/INCLINE_community_species_cover.csv")

# biomass <- read_csv("data_cleaned/INCLINE_biomass_removal.csv") # biomass cannot be used because it is different plots than the ones with c-fluxes measurements



plot_data <- community %>% 
  # left_join(INCLINE_metadata) %>% 
  filter(
    year == 2022,
    # subPlot != "whole_plot",
    treatment == "C"
  ) %>% 
  mutate(
    functional_group = case_when(
      species == "Hie_pil" ~ "Forbs",
      TRUE ~ functional_group
    )
  #   # lichen = replace_na(lichen, 0),
  #   # moss = replace_na(moss, 0),
  #   # forbs = Lot_cor + Aco_sep + Eri_sp + Eri_uni + Equ_sci + Equ_arv + Pin_vul + Tof_pus + Sau_alp + Leu_vul + Ant_odo + Eup_wet + Sib_pro + Alc_alp + Alc_sp + Oma_sup + Ver_alp + Vio_pal + Cam_rot + Sag_sag + Leo_aut + Sel_sel + Pyr_sp + Luz_mul + Tar_sp + Pot_cra + Dip_alp + Tha_alp + Lys_eur + Hie_alp + Rum_ace + Cer_cer + Epi_ana + Equ_arv + Epi_sp + Tof_pus + Nid_seedling + Bar_alp + Sil_aca + Par_pal + Hie_alp + Cer_fon + Pot_ere + Vio_bif + Coel_vir + Ran_acr + Gen_niv + Pin_vul + Eri_sp + Ach_mil + Pyr_min + Bis_viv + Ast_alp + Rum_acl + Bot_lun + Gen_ama + Ran_sp + Oxy_dig + Fern + Ger_syl + Geu_riv + Rhi_min + Hie_sp + Tri_ces + Hyp_sel + Sol_vir + Vio_can + Ort_sec + Pru_vul + Ver_off + Suc_pra + Hyp_mac + Ran_pyg + Dry_oct + Luz_spi + Tri_rep + Hyp_sp + Ste_gra + Sel_sp + Vio_tri + Ver_cha + Nid_juvenile + Gen_sp + Tri_sp + Oma_sp + Cer_alp + Tri_pra + Sil_vul + Sag_sp + Phe_con + Gym_dry + Oma_nor + Gal_sp + Gen_cam + Oxa_ace + Lot_cor + Aco_sep + Eri_uni + Equ_sci + Sau_alp + Leu_vul,
  #   # graminoids = Nar_str + Agr_mer + Agr_cap + Car_big + Car_nor + Car_cap + Car_pal + Car_pil + Poa_pra + Car_vag + Ave_fle + Des_ces + Poa_alp + Jun_tri + Phl_alp + Fes_ovi + Fes_rub + Sau_alp + Fes_sp + Car_sp + Ant_dio + Fes_viv + Des_alp + Car_fla + Car_sax + Ant_sp + Car_atr,
  #   # evergreens = Emp_nig + Vac_vit + Cal_vul,
  #   # deciduous = Sal_her + Vac_myr + Vac_uli + Sal_sp + Bet_nan + Bet_pub + Sal_lan,
  #   # OTC = str_replace_all(warming, c("C" = "CTL", "W" = "OTC")),
  #   ITEX_ID = case_when(
  #     site == "Lavisdalen" ~ "NOR_9",
  #     site == "Gudmedalen" ~ "NOR_11",
  #     site == "Ulvehaugen" ~ "NOR_10",
  #     site == "Skjellingahaugen" ~ "NOR_12"
  #   )
  ) %>%
  select(plotID, functional_group, cover) %>% 
  group_by(plotID, functional_group) %>% 
  summarise(
    group_cover = sum(cover)
  ) %>% 
  ungroup() %>%
  pivot_wider(names_from = functional_group, values_from = group_cover) %>% 
  left_join(INCLINE_metadata) %>%
  left_join(height) %>% 
  # select(year, moss, lichen, Veg_height_mm, plotID, site, forbs, graminoids, evergreens, deciduous, OTC, ITEX_ID, coordinate_N, coordinate_E) %>% 
  # distinct() %>% 
  # group_by(year, plotID, site, OTC, ITEX_ID, coordinate_N, coordinate_E) %>% 
  #   summarise(
  #     moss_mean = mean(moss, na.rm = FALSE),
  #     lichen_mean = mean(lichen, na.rm = FALSE),
  #     veg_height_mean_cm = mean(Veg_height_mm, na.rm = TRUE) / 10,
  #     forbs_mean = mean(forbs, na.rm = FALSE),
  #     graminoids_mean = mean(graminoids, na.rm = FALSE),
  #     evergreens_mean = mean(evergreens, na.rm = FALSE),
  #     deciduous_mean = mean(deciduous, na.rm = FALSE)
  #   ) %>% 
  # ungroup() %>% 
  # select(year, plotID, site, OTC, moss_mean, lichen_mean, veg_height_mean_cm, forbs_mean, graminoids_mean, evergreens_mean, deciduous_mean, coordinate_N, coordinate_E) %>% 
  mutate(
    N_Coord = gsub("(\\d{2})(?=\\d{2})", "\\1 ", (coordinate_N/10), perl = TRUE),
    E_Coord = gsub("(^\\d)(?=\\d{2})(\\d{2})", "\\1 \\2 ", (coordinate_E/10), perl = TRUE),
    N_Coord = conv_unit(N_Coord, from = "deg_min_sec", to = "dec_deg"),
    E_Coord = conv_unit(E_Coord, from = "deg_min_sec", to = "dec_deg"),
    OTC = str_replace_all(OTC, c("C" = "CTL", "W" = "OTC")),
    community_mean_plant_height = vegetation_height_mean / 10, # they want cm
    # ITEX_ID = case_when(
    #   siteID == "Lavisdalen" ~ "NOR_9",
    #   siteID == "Gudmedalen" ~ "NOR_11",
    #   siteID == "Ulvehaugen" ~ "NOR_10",
    #   siteID == "Skjellingahaugen" ~ "NOR_12"
    #   )
    ITEX_ID = str_replace_all(
      siteID,
      c(
        "Lavisdalen" = "NOR_9",
        "Gudmedalen" = "NOR_11",
        "Ulvehaugen" = "NOR_10",
        "Skjellingahaugen" = "NOR_12"
      )
    )
  ) %>% 
  select(plotID, Deciduous_shrubs, Forbs, Graminoids, Evergreen_shrubs, ITEX_ID, OTC, total_bryophyte_cover, total_lichen_cover, community_mean_plant_height, N_Coord, E_Coord) %>%
  rename(
    treatment = "OTC",
    # community_mean_plant_height = "vegetation_height_mean",
    Mosses = "total_bryophyte_cover",
    Lichens = "total_lichen_cover"
  ) %>%
  relocate(ITEX_ID, treatment, plotID, N_Coord, E_Coord, community_mean_plant_height, Graminoids, Forbs, Deciduous_shrubs, Evergreen_shrubs, Mosses, Lichens) %>% 
  # left_join(soil_moisture_avg, by = c("plotID" = "turfID", "year")) %>% 

  arrange(ITEX_ID)
  
write_csv(plot_data, "ITEX/ITEX_plot-data_2022.csv")


# missing info ------------------------------------------------------------

# OTC height
# Vegetation Type (Walker et al., 2005)
# Soil Type
# Parent Material
# Soil Moisture Category
# Organic Layer depth
# error in legend (corrected)



































