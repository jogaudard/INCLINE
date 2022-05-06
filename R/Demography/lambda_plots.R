########################################################
### Script for making population growth rate figures ###
########################################################

##### Libraries #####
library(tidyverse)
library(patchwork)

##### Color palette #####
palette <- c("#a6611a", "#dfc27d", "#80cdc1", "#018571")
precip_3_palette <- c("#BAD8F7", "#89B7E1", "#2E75B6", "#000000")

##### Make dataset with lambdas #####

lambdas <- c(as.numeric(eigen(IPM_SP_CC)$value[1]), as.numeric(eigen(IPM_SP_CE_precip1)$value[1]), as.numeric(eigen(IPM_SP_CE_precip2)$value[1]), as.numeric(eigen(IPM_SP_CE_precip3)$value[1]), as.numeric(eigen(IPM_SP_CN_precip1)$value[1]), as.numeric(eigen(IPM_SP_CN_precip2)$value[1]), as.numeric(eigen(IPM_SP_CN_precip3)$value[1]), as.numeric(eigen(IPM_SP_CR)$value[1]),
             as.numeric(eigen(IPM_SP_WC_precip1)$value[1]), as.numeric(eigen(IPM_SP_WC_precip2)$value[1]), as.numeric(eigen(IPM_SP_WC_precip3)$value[1]), as.numeric(eigen(IPM_SP_WE_precip1)$value[1]), as.numeric(eigen(IPM_SP_WE_precip2)$value[1]), as.numeric(eigen(IPM_SP_WE_precip3)$value[1]), as.numeric(eigen(IPM_SP_WN)$value[1]), as.numeric(eigen(IPM_SP_WR_precip1)$value[1]), as.numeric(eigen(IPM_SP_WR_precip2)$value[1]), as.numeric(eigen(IPM_SP_WR_precip3)$value[1]), 
             as.numeric(eigen(IPM_VA_CC_precip1)$value[1]), as.numeric(eigen(IPM_VA_CC_precip2)$value[1]), as.numeric(eigen(IPM_VA_CC_precip3)$value[1]), as.numeric(eigen(IPM_VA_CE_precip1)$value[1]), as.numeric(eigen(IPM_VA_CE_precip2)$value[1]), as.numeric(eigen(IPM_VA_CE_precip3)$value[1]), as.numeric(eigen(IPM_VA_CN_precip1)$value[1]), as.numeric(eigen(IPM_VA_CN_precip2)$value[1]), as.numeric(eigen(IPM_VA_CN_precip3)$value[1]), as.numeric(eigen(IPM_VA_CR_precip1)$value[1]), as.numeric(eigen(IPM_VA_CR_precip2)$value[1]), as.numeric(eigen(IPM_VA_CR_precip3)$value[1]),
             as.numeric(eigen(IPM_VA_WC_precip1)$value[1]), as.numeric(eigen(IPM_VA_WC_precip2)$value[1]), as.numeric(eigen(IPM_VA_WC_precip3)$value[1]), as.numeric(eigen(IPM_VA_WE_precip1)$value[1]), as.numeric(eigen(IPM_VA_WE_precip2)$value[1]), as.numeric(eigen(IPM_VA_WE_precip3)$value[1]), as.numeric(eigen(IPM_VA_WN_precip1)$value[1]), as.numeric(eigen(IPM_VA_WN_precip2)$value[1]), as.numeric(eigen(IPM_VA_WN_precip3)$value[1]), as.numeric(eigen(IPM_VA_WR_precip1)$value[1]), as.numeric(eigen(IPM_VA_WR_precip2)$value[1]), as.numeric(eigen(IPM_VA_WR_precip3)$value[1]))

species <- c("Sibbaldia procumbens", "Sibbaldia procumbens", "Sibbaldia procumbens", "Sibbaldia procumbens", "Sibbaldia procumbens", "Sibbaldia procumbens", "Sibbaldia procumbens", "Sibbaldia procumbens",
             "Sibbaldia procumbens", "Sibbaldia procumbens", "Sibbaldia procumbens", "Sibbaldia procumbens", "Sibbaldia procumbens", "Sibbaldia procumbens", "Sibbaldia procumbens", "Sibbaldia procumbens", "Sibbaldia procumbens", "Sibbaldia procumbens",
             "Veronica alpina", "Veronica alpina", "Veronica alpina", "Veronica alpina", "Veronica alpina", "Veronica alpina", "Veronica alpina", "Veronica alpina", "Veronica alpina", "Veronica alpina", "Veronica alpina", "Veronica alpina",
             "Veronica alpina", "Veronica alpina", "Veronica alpina", "Veronica alpina", "Veronica alpina", "Veronica alpina", "Veronica alpina", "Veronica alpina", "Veronica alpina", "Veronica alpina", "Veronica alpina", "Veronica alpina")

treatments <- c("CC", "CE", "CE", "CE", "CN", "CN", "CN", "CR",
                "WC", "WC", "WC", "WE", "WE", "WE", "WN", "WR", "WR", "WR",
                "CC", "CC", "CC", "CE", "CE", "CE", "CN", "CN", "CN", "CR", "CR", "CR",
                "WC", "WC", "WC", "WE", "WE", "WE", "WN", "WN", "WN", "WR", "WR", "WR")

precipitation <- c("Overall", "1.2", "2.3", "3.4", "1.2", "2.3", "3.4", "Overall",
                   "1.2", "2.3", "3.4", "1.2", "2.3", "3.4", "Overall", "1.2", "2.3", "3.4",
                   "1.2", "2.3", "3.4", "1.2", "2.3", "3.4", "1.2", "2.3", "3.4", "1.2", "2.3", "3.4",
                   "1.2", "2.3", "3.4", "1.2", "2.3", "3.4", "1.2", "2.3", "3.4", "1.2", "2.3", "3.4")

lambda_df <- enframe(lambdas)

lambda_df <- lambda_df %>% 
  bind_cols(species, treatments, precipitation) %>%
  rename(lambda = value, Species = ...3, Treatments = ...4, precipitation = ...5) %>%
  select(!name) %>% 
  mutate(Treatments = factor(Treatments, levels = c("CC", "CR", "CE", "CN", "WC", "WR", "WE", "WN"))) %>% 
  mutate(precipitation = case_when(precipitation == "Overall" ~ "Across precipitation",
                                   precipitation == "1.2" ~ "1.2 m/year",
                                   precipitation == "2.3" ~ "2.3 m/year",
                                   precipitation == "3.4" ~ "3.4 m/year")) %>% 
  mutate(precipitation = factor(precipitation, levels = c("Across precipitation", "1.2 m/year", "2.3 m/year", "3.4 m/year")))


#lambda_plot <- 
  
  lambda_lolliplot <- lambda_df %>% 
  mutate(precipitation = factor(precipitation, levels = c( "1.2 m/year", "2.3 m/year", "3.4 m/year", "Across precipitation"))) %>% 
ggplot(aes(x = lambda, y = Treatments, color = precipitation)) +
  #geom_linerange(aes(xmin=1, xmax=lambda, y=Treatments), position = position_dodge(width = 0.40))+
  #geom_segment(aes(x=1, xend=lambda, y=Treatments, yend=Treatments), position = position_dodge(width = 0.90)) +
  #geom_rect(aes(xmin = 0, xmax = 2, ymin = Treatments - 0.5, ymax = Treatments + 0.5), alpha = 0.2) +
  #geom_rect(aes(xmin = 0, xmax = 2, ymin = Treatments - 0.5, ymax = Treatments + 0.5), alpha = 0.2) + 
    geom_point(size = 5, position = position_dodge(width = 0.40)) +
  facet_wrap(~Species, nrow = 1, scales = "free") +
  geom_vline(xintercept = 1) +
  scale_y_discrete(limits = rev) +
  scale_color_manual(values = precip_3_palette) +
  theme_bw() +
  xlab("λ") +
  ggtitle("A) Population growth rate") +
  theme(plot.title = element_text(hjust = 0.5)) 
    #scale_fill_manual(values = c("#a6611a", "#dfc27d"))

lambda_CC_precip <- lambda_df %>% 
  filter(Treatments == "CC") %>% 
  mutate(lambda_CC = lambda) %>% 
  select(-lambda, -Treatments) %>% 
  filter(!precipitation == "Across precipitation")

lambda_WC_precip <- lambda_df %>% 
  filter(Treatments == "WC") %>% 
  mutate(lambda_WC = lambda) %>% 
  select(-lambda, -Treatments) %>% 
  filter(!precipitation == "Across precipitation")

lambda_CC_no_precip <- lambda_df %>% 
  filter(Treatments == "CC") %>% 
  mutate(lambda_CC = lambda) %>% 
  filter(precipitation == "Across precipitation") %>% 
  select(-lambda, -Treatments, -precipitation) 

# Adding the precipitation levels to treatments that does not have any precip difference so that we can compare with the WC that has values for different precipitationations.
precipitation <- c("1.2 m/year", "2.3 m/year", "3.4 m/year")
precipitation <- data.frame(precipitation)

lambda_WN <- lambda_df %>% 
  filter(Treatments == "WN",
         Species == "Sibbaldia procumbens") %>% 
  mutate(lambda = lambda) %>% 
  select(-precipitation) %>% 
  slice(rep(1:n(), each = 3)) %>% 
  add_column(precipitation)

lambda_CR <- lambda_df %>% 
  filter(Treatments == "CR",
         Species == "Sibbaldia procumbens") %>% 
  mutate(lambda = lambda) %>% 
  select(-precipitation) %>% 
  slice(rep(1:n(), each = 3)) %>% 
  add_column(precipitation)

## Calculate differences in lambda

lambda_df_differences <- lambda_df %>% 
  bind_rows(lambda_WN) %>% 
  bind_rows(lambda_CR) %>% 
  left_join(lambda_CC_precip, by = c("Species", "precipitation")) %>% 
  left_join(lambda_WC_precip, by = c("Species", "precipitation")) %>% 
  left_join(lambda_CC_no_precip, by = c("Species")) %>% 
  mutate(lambda_CC = case_when(is.na(lambda_CC.x) ~ lambda_CC.y,
                               !is.na(lambda_CC.x) ~ lambda_CC.x)) %>% 
  select(-lambda_CC.y, -lambda_CC.x) %>% 
  mutate(CC_comparison = lambda - lambda_CC,
         WC_comparison = lambda - lambda_WC)


lambda_diff_CC_plot <- lambda_df_differences %>% 
  filter(!Treatments == "CC") %>% 
  filter(!precipitation == "Across precipitation") %>% 
ggplot(aes(x = CC_comparison, y = Treatments, shape = Species, color = Species)) +
  geom_point(size = 5) +
  #geom_segment(aes(x=0, xend=CC_comparison, y=Treatments, yend=Treatments)) +
  facet_wrap(~precipitation, nrow = 1) +
  geom_vline(xintercept = 0) +
  scale_y_discrete(limits = rev) +
  scale_color_manual(values = c("#a6611a","#018571")) +
  theme_bw()+
  xlab("Δ λ") +
  ggtitle("b) Comparing λ with extant climate control (CC)") +
  theme(plot.title = element_text(hjust = 0.5))

lambda_diff_CC_lolliplot <- lambda_df_differences %>% 
  filter(!Treatments == "CC") %>% 
  mutate(precipitation = factor(precipitation, levels = c( "1.2 m/year", "2.3 m/year", "3.4 m/year", "Across precipitation"))) %>% 
  ggplot(aes(x = CC_comparison, y = Treatments, color = precipitation)) +
  geom_linerange(aes(xmin=0, xmax=CC_comparison, y=Treatments), position = position_dodge(width = 0.40))+
  #geom_segment(aes(x=1, xend=lambda, y=Treatments, yend=Treatments), position = position_dodge(width = 0.90)) +
  #geom_rect(aes(xmin = 0, xmax = 2, ymin = Treatments - 0.5, ymax = Treatments + 0.5), alpha = 0.2) +
  #geom_rect(aes(xmin = 0, xmax = 2, ymin = Treatments - 0.5, ymax = Treatments + 0.5), alpha = 0.2) + 
  geom_point(size = 5, position = position_dodge(width = 0.40)) +
  facet_wrap(~Species, nrow = 1, scales = "free") +
  geom_vline(xintercept = 0) +
  scale_y_discrete(limits = rev) +
  scale_color_manual(values = precip_3_palette) +
  theme_bw() +
  xlab("Δ λ") +
  ggtitle("b) Comparing λ with extant climate control (CC)") +
  theme(plot.title = element_text(hjust = 0.5)) 
#scale_fill_manual(values = c("#a6611a", "#dfc27d"))


lambda_diff_WC_plot <- lambda_df_differences %>% 
  filter(Treatments %in% c("WR", "WE", "WN")) %>% 
  filter(!precipitation == "Across precipitation") %>% 
ggplot(aes(x = WC_comparison, y = Treatments, shape = Species, color = Species)) +
  geom_point(size = 5) +
  #geom_segment(aes(x=0, xend=CC_comparison, y=Treatments, yend=Treatments)) +
  facet_wrap(~precipitation, nrow = 1) +
  geom_vline(xintercept = 0) +
  scale_y_discrete(limits = rev) +
  scale_color_manual(values = c("#a6611a","#018571")) +
  theme_bw() +
  xlab("Δ λ") +
  ggtitle("c) Comparing λ with warmed control (WC)") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 30))

lambda_diff_WC_lolliplot <- lambda_df_differences %>% 
  filter(Treatments %in% c("WR", "WE", "WN")) %>% 
  mutate(precipitation = factor(precipitation, levels = c( "1.2 m/year", "2.3 m/year", "3.4 m/year", "Across precipitation"))) %>% 
  ggplot(aes(x = CC_comparison, y = Treatments, color = precipitation)) +
  geom_linerange(aes(xmin=0, xmax=CC_comparison, y=Treatments), position = position_dodge(width = 0.40))+
  #geom_segment(aes(x=1, xend=lambda, y=Treatments, yend=Treatments), position = position_dodge(width = 0.90)) +
  #geom_rect(aes(xmin = 0, xmax = 2, ymin = Treatments - 0.5, ymax = Treatments + 0.5), alpha = 0.2) +
  #geom_rect(aes(xmin = 0, xmax = 2, ymin = Treatments - 0.5, ymax = Treatments + 0.5), alpha = 0.2) + 
  geom_point(size = 5, position = position_dodge(width = 0.40)) +
  facet_wrap(~Species, nrow = 1, scales = "free") +
  geom_vline(xintercept = 0) +
  scale_y_discrete(limits = rev) +
  scale_color_manual(values = precip_3_palette) +
  theme_bw() +
  xlab("Δ λ") +
  ggtitle("b) Comparing λ with extant climate control (CC)") +
  theme(plot.title = element_text(hjust = 0.5)) 
#scale_fill_manual(values = c("#a6611a", "#dfc27d"))


(lambda_plot /  lambda_diff_CC_plot / lambda_diff_WC_plot) + 
  plot_layout(heights = c(2,2, 1), guides = 'collect') &
  theme(legend.position = "bottom", text = element_text(size = 14))


(lambda_lolliplot /  lambda_diff_CC_lolliplot / lambda_diff_WC_lolliplot) + 
  plot_layout(heights = c(2,1.9, 1), guides = 'collect') &
  theme(legend.position = "bottom", text = element_text(size = 14))
