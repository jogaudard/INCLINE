###########################################################
### Script for cleaning raw community data from INCLINE ###
###########################################################

#### Libraries ####
library(tidyverse)
library(dataDownloader)
library(osfr)
library(lubridate)

#### Downloading data from OSF ####



#### Load data ####

comm <- read_csv2("data/Community/INCLINE_community_2018_2019_2021.csv")


#### Cleaning data ####

#Renaming columns

comm1 <- comm %>% 
  rename(block = Block, treatment = Treatment, plot = plot, site = Site, measure = Measure, weather = Weather, unknown = Unknown, veg_cover = Veg_cover, veg_height_mm = Veg_height_mm, moss_depth_mm = Moss_depth_mm, Sel_sp = Selaginella_sp) %>% 
  select(-"...197") %>% 
  mutate(siteID = substr(site, 0, 3)) %>% 
  mutate(plotID = paste0(siteID, "_", block, "_", plot, "_"))



comm2 <- comm1 %>% 
  select(-Leu_aut_cf) %>% 
  filter(measure == "cover") %>% 
  mutate(Agr_cap = as.numeric(Agr_cap),
         Agr_mer = as.numeric(Agr_mer),
         Alc_alp = as.numeric(Alc_alp),
         Alc_sp = as.numeric(Alc_sp),
         Ant_odo = as.numeric(Ant_odo),
         Ast_alp = as.numeric(Ast_alp),
         Ave_fle = as.numeric(Ave_fle),
         Bis_viv = as.numeric(Bis_viv),
         Cal_vul = as.numeric(Cal_vul),
         Cam_rot = as.numeric(Cam_rot),
         Car_big = as.numeric(Car_big),
         Car_cap = as.numeric(Car_cap),
         Car_fla = as.numeric(Car_fla),
         Car_nig = as.numeric(Car_nig),
         Car_nor_cf = as.numeric(Car_nor_cf),
         Car_pal = as.numeric(Car_pal),
         Car_pil = as.numeric(Car_pil),
         Cer_cer = as.numeric(Cer_cer),
         Cer_fon = as.numeric(Cer_fon),
         Des_ces = as.numeric(Des_ces),
         Emp_nig = as.numeric(Emp_nig),
         Epi_ana = as.numeric(Epi_ana),
         Epi_ana_cf = as.numeric(Epi_ana_cf),
         Eup_sp = as.numeric(Eup_sp),
         Eup_wet = as.numeric(Eup_wet),
         Fes_ovi = as.numeric(Fes_ovi),
         Fes_viv = as.numeric(Fes_viv),
         Gen_niv = as.numeric(Gen_niv),
         Ger_syl = as.numeric(Ger_syl),
         Leo_aut = as.numeric(Leo_aut),
         Luz_mul = as.numeric(Luz_mul),
         Luz_spi = as.numeric(Luz_spi),
         Nar_str = as.numeric(Nar_str),
         Oma_sup = as.numeric(Oma_sup),
         Par_pal = as.numeric(Par_pal),
         Phl_alp = as.numeric(Phl_alp),
         Pin_vul = as.numeric(Pin_vul),
         Poa_alp = as.numeric(Poa_alp),
         Poa_pra = as.numeric(Poa_pra),
         Pot_cra = as.numeric(Pot_cra),
         Pot_ere = as.numeric(Pot_ere),
         Pyr_sp = as.numeric(Pyr_sp),
         Ran_acr = as.numeric(Ran_acr),
         Ranunculus = as.numeric(Ranunculus),
         Rhi_min = as.numeric(Rhi_min),
         Sag_sag = as.numeric(Sag_sag),
         Sal_her = as.numeric(Sal_her),
         Sau_alp = as.numeric(Sau_alp),
         Sel_sel = as.numeric(Sel_sel),
         Sib_pro = as.numeric(Sib_pro),
         Sil_aca = as.numeric(Sil_aca),
         Suc_pra = as.numeric(Suc_pra),
         Tar_sp = as.numeric(Tar_sp),
         Tha_alp = as.numeric(Tha_alp),
         Tri_eur = as.numeric(Tri_eur),
         Vac_myr = as.numeric(Vac_myr),
         Vac_uli = as.numeric(Vac_uli),
         Ver_alp = as.numeric(Ver_alp),
         Ver_off = as.numeric(Ver_off),
         Vio_can = as.numeric(Vio_can),
         Vio_bif = as.numeric(Vio_bif),
         Vio_pal = as.numeric(Vio_pal)) %>% 
  pivot_longer(cols = 12:178, names_to = "species", values_to = "cover")
  