library(tidyverse)
library("dataDownloader")

#importing flux data from OSF
get_file(node = "7a4y9",
         file = "INCLINE_c-flux_2020.csv",
         path = "data/C-Flux/summer_2020",
         remote_path = "C-Flux")

#only ER, take out p-value>0.05
itex.df <- read_csv("data/C-Flux/summer_2020/INCLINE_c-flux_2020.csv") %>% 
  filter(
    p.value <= 0.05
    & r.squared >= 0.7
    & Type == "ER"
  )


#average per date and plot

itex.df <- itex.df %>% 
  group_by(Plot_ID, Date) %>% 
  summarise(
    flux.avg = mean(flux),
    temp_air.avg = mean(Temp_airavg)
  )

#adding treatment CTL or OTC

treatment <- read_csv("treatment.csv")

itex.df <- left_join(itex.df, treatment, by = "Plot_ID")
  # right_join(treatment, itex.df, by = "Plot_ID")

#a bit of cleaning

itex.df <-itex.df %>% 
  mutate(
    Treatment = str_replace_all(Treatment, c("W_C" = "OTC", "C_C" = "CTL")), #replacing the name of the treatments to fit ITEX wishes
    temp_air = temp_air.avg - 273.15 #temp air in celsius
  )
  
write_csv(itex.df, "data/C-Flux/summer_2020/ITEX_cflux_2020.csv")
