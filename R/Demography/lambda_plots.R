########################################################
### Script for making population growth rate figures ###
########################################################

##### Libraries #####
library(tidyverse)
library(patchwork)

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


palette <- c("#a6611a", "#dfc27d", "#80cdc1", "#018571")

lambda_plot <- ggplot(aes(x = lambda, y = Treatments, shape = Species, color = Species), data = lambda_df) +
  geom_point(size = 5) +
  #geom_segment(aes(x=1, xend=lambda, y=Treatments, yend=Treatments)) +
  facet_wrap(~precipitation, nrow = 1) +
  geom_vline(xintercept = 1) +
  scale_y_discrete(limits = rev) +
  scale_color_manual(values = c("#a6611a","#018571")) +
  theme_bw() +
  xlab("λ") +
  ggtitle("A) Population growth rate") +
  theme(plot.title = element_text(hjust = 0.5))

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

lambda_WC_no_precip <- lambda_df %>% 
  filter(Treatments == "WC",
         Species == "Sibbaldia procumbens") %>% 
  mutate(lambda_WC = lambda) %>% 
  select(-lambda, -Treatments) %>% 
  filter(!precipitation == "Across precipitation")


lambda_df_differences <- lambda_df %>% 
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
ggplot(aes(x = CC_comparison, y = Treatments, shape = Species, color = Species)) +
  geom_point(size = 5) +
  #geom_segment(aes(x=0, xend=CC_comparison, y=Treatments, yend=Treatments)) +
  facet_wrap(~precipitation, nrow = 1) +
  geom_vline(xintercept = 0) +
  scale_y_discrete(limits = rev) +
  scale_color_manual(values = c("#a6611a","#018571")) +
  theme_bw()+
  xlab("Δ λ") +
  ggtitle("b) Comparing λ with extant climate control") +
  theme(plot.title = element_text(hjust = 0.5))

lambda_diff_WC_plot <- lambda_df_differences %>% 
  filter(Treatments %in% c("WR", "WE", "WN")) %>% 
ggplot(aes(x = WC_comparison, y = Treatments, shape = Species, color = Species)) +
  geom_point(size = 5) +
  #geom_segment(aes(x=0, xend=CC_comparison, y=Treatments, yend=Treatments)) +
  facet_wrap(~precipitation, nrow = 1) +
  geom_vline(xintercept = 0) +
  scale_y_discrete(limits = rev) +
  scale_color_manual(values = c("#a6611a","#018571")) +
  theme_bw() +
  xlab("Δ λ") +
  ggtitle("c) Comparing λ with warmed control") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 30))


(lambda_plot /  lambda_diff_CC_plot / lambda_diff_WC_plot) + 
  plot_layout(heights = c(2,2, 1), guides = 'collect') &
  theme(legend.position = "bottom", text = element_text(size = 14))
