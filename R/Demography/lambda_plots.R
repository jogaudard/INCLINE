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
  
  lambda_lolliplot2 <- lambda_df %>% 
    mutate(precipitation = factor(precipitation, levels = c( "1.2 m/year", "2.3 m/year", "3.4 m/year", "Across precipitation"))) %>% 
    mutate(Treatments = factor(Treatments, levels = c("CR", "CC", "CE", "CN", "WR", "WC", "WE", "WN"))) %>% 
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

lambda_diff_CC_lolliplot2 <- lambda_df_differences %>% 
  filter(!Treatments == "CC") %>% 
  mutate(precipitation = factor(precipitation, levels = c( "1.2 m/year", "2.3 m/year", "3.4 m/year", "Across precipitation"))) %>% 
  mutate(Treatments = factor(Treatments, levels = c("CR", "CE", "CN", "WR", "WC", "WE", "WN"))) %>% 
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
  ggplot(aes(x = WC_comparison, y = Treatments, color = precipitation)) +
  geom_linerange(aes(xmin=0, xmax=WC_comparison, y=Treatments), position = position_dodge(width = 0.40))+
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
  ggtitle("b) Comparing λ with warming control (WC)") +
  theme(plot.title = element_text(hjust = 0.5)) 
#scale_fill_manual(values = c("#a6611a", "#dfc27d"))


(lambda_plot /  lambda_diff_CC_plot / lambda_diff_WC_plot) + 
  plot_layout(heights = c(2,2, 1), guides = 'collect') &
  theme(legend.position = "bottom", text = element_text(size = 14))


(lambda_lolliplot /  lambda_diff_CC_lolliplot / lambda_diff_WC_lolliplot) + 
  plot_layout(heights = c(2,1.9, 1), guides = 'collect') &
  theme(legend.position = "bottom", text = element_text(size = 14))


(lambda_lolliplot2 /  lambda_diff_CC_lolliplot2 / lambda_diff_WC_lolliplot) + 
  plot_layout(heights = c(2,1.9, 1), guides = 'collect') &
  theme(legend.position = "bottom", text = element_text(size = 14))


(lambda_lolliplot /  lambda_diff_CC_lolliplot) + 
  plot_layout(heights = c(2,1.9), guides = 'collect') &
  theme(legend.position = "bottom", text = element_text(size = 14))

#### trying some new plots ####

lambda_horizontal <- lambda_df %>% 
  mutate(precipitation = factor(precipitation, levels = c( "1.2 m/year", "2.3 m/year", "3.4 m/year", "Across precipitation"))) %>% 
  ggplot(aes(y = lambda, x = Treatments, color = precipitation)) +
  #geom_linerange(aes(xmin=1, xmax=lambda, y=Treatments), position = position_dodge(width = 0.40))+
  #geom_segment(aes(x=1, xend=lambda, y=Treatments, yend=Treatments), position = position_dodge(width = 0.90)) +
  #geom_rect(aes(xmin = 0, xmax = 2, ymin = Treatments - 0.5, ymax = Treatments + 0.5), alpha = 0.2) +
  #geom_rect(aes(xmin = 0, xmax = 2, ymin = Treatments - 0.5, ymax = Treatments + 0.5), alpha = 0.2) + 
  geom_point(size = 5, position = position_dodge(width = 0.40)) +
  facet_wrap(~Species, nrow = 1, scales = "free") +
  geom_hline(yintercept = 1) +
  #scale_y_discrete(limits = rev) +
  scale_color_manual(values = precip_3_palette) +
  theme_bw() +
  ylab("λ") +
  ggtitle("A) Population growth rate") +
  theme(plot.title = element_text(hjust = 0.5)) 
#scale_fill_manual(values = c("#a6611a", "#dfc27d"))

### Calculate differences in lambda to other things than the controls

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

lambda_df_differences2 <- lambda_df_differences %>% 
  mutate(lambda_CR = case_when(Treatments == "CR" ~ lambda),
         lambda_CN = case_when(Treatments == "CN" ~ lambda),
         lambda_CE = case_when(Treatments == "CE" ~ lambda),
         lambda_WE = case_when(Treatments == "WE" ~ lambda)) %>% 
  group_by(Species, precipitation) %>% 
  fill(lambda_CR, .direction = "downup") %>% 
  fill(lambda_CN, .direction = "downup") %>% 
  fill(lambda_CE, .direction = "downup") %>% 
  fill(lambda_WE, .direction = "downup") %>% 
  mutate(CR_comparison = lambda - lambda_CR,
         CE_comparison = lambda - lambda_CE,
         CN_comparison = lambda - lambda_CN,
         WE_comparison = lambda - lambda_WE)

lambda_df_differences3 <- lambda_df_differences2 %>% 
  pivot_longer(cols = c(CC_comparison, WC_comparison, CR_comparison), values_to = "value")

### Plots for prediction 0: The direct effects of warming have a positive effect on population dynamics, while the indirect effects counteract these positive effects.

lambda_diff_VA_WR_CR_lolliplot <- lambda_df_differences2 %>% 
  filter(Species == "Veronica alpina") %>% 
  filter(Treatments %in% c("WR")) %>% 
  mutate(precipitation = factor(precipitation, levels = c( "1.2 m/year", "2.3 m/year", "3.4 m/year", "Across precipitation"))) %>% 
  ggplot(aes(y = CR_comparison, x = Treatments, color = precipitation)) +
  geom_linerange(aes(ymin=0, ymax=CR_comparison, x= Treatments), position = position_dodge(width = 0.40))+
  geom_point(size = 5, position = position_dodge(width = 0.40)) +
  #facet_wrap(~Species, nrow = 2, scales = "free") +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = precip_3_palette) +
  theme_bw() +
  ylab("Δ λ") +
  xlab("") +
  ggtitle("CR") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(-0.25, 0.8)

lambda_diff_VA_WC_CC_lolliplot <- lambda_df_differences2 %>% 
  filter(Species == "Veronica alpina") %>% 
  filter(Treatments %in% c("WC")) %>% 
  mutate(precipitation = factor(precipitation, levels = c( "1.2 m/year", "2.3 m/year", "3.4 m/year", "Across precipitation"))) %>% 
  ggplot(aes(y = CC_comparison, x = Treatments, color = precipitation)) +
  geom_linerange(aes(ymin=0, ymax=CC_comparison, x= Treatments), position = position_dodge(width = 0.40))+
  geom_point(size = 5, position = position_dodge(width = 0.40)) +
  #facet_wrap(~Species, nrow = 2, scales = "free") +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = precip_3_palette) +
  theme_bw() +
  ylab("Δ λ") +
  xlab("") +
  ggtitle("CC") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(-0.25, 0.8)


lambda_diff_SP_WR_CR_lolliplot <- lambda_df_differences2 %>% 
  filter(Species == "Sibbaldia procumbens") %>% 
  filter(Treatments %in% c("WR")) %>% 
  mutate(precipitation = factor(precipitation, levels = c( "1.2 m/year", "2.3 m/year", "3.4 m/year", "Across precipitation"))) %>% 
  ggplot(aes(y = CR_comparison, x = Treatments, color = precipitation)) +
  geom_linerange(aes(ymin=0, ymax=CR_comparison, x= Treatments), position = position_dodge(width = 0.40))+
  geom_point(size = 5, position = position_dodge(width = 0.40)) +
  #facet_wrap(~Species, nrow = 2, scales = "free") +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = precip_3_palette) +
  theme_bw() +
  ylab("Δ λ") +
  xlab("") +
  ggtitle("CR") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(-0.1, 0.1)

lambda_diff_SP_WC_CC_lolliplot <- lambda_df_differences2 %>% 
  filter(Species == "Sibbaldia procumbens") %>% 
  filter(Treatments %in% c("WC")) %>% 
  mutate(precipitation = factor(precipitation, levels = c( "1.2 m/year", "2.3 m/year", "3.4 m/year", "Across precipitation"))) %>% 
  ggplot(aes(y = CC_comparison, x = Treatments, color = precipitation)) +
  geom_linerange(aes(ymin=0, ymax=CC_comparison, x= Treatments), position = position_dodge(width = 0.40))+
  geom_point(size = 5, position = position_dodge(width = 0.40)) +
  #facet_wrap(~Species, nrow = 2, scales = "free") +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = precip_3_palette) +
  theme_bw() +
  ylab("Δ λ") +
  xlab("") +
  ggtitle("CC") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(-0.1, 0.1)

P0_VA_plot <- ((lambda_diff_VA_WR_CR_lolliplot + lambda_diff_VA_WC_CC_lolliplot)) + 
  plot_annotation( title = "Direct and indirect effects of warming in the current alpine community",
                   subtitle = 'Veronica alpina') +
  plot_layout(widths = c(1,1), guides = 'collect') &
  theme(legend.position = "bottom", text = element_text(size = 14))

P0_SP_plot <- ((lambda_diff_SP_WR_CR_lolliplot + lambda_diff_SP_WC_CC_lolliplot)) + 
  plot_annotation( title = "Direct and indirect effects of warming in the current alpine community",
                   subtitle = 'Sibbaldia procumbens') +
  plot_layout(widths = c(1,1), guides = 'collect') &
  theme(legend.position = "bottom", text = element_text(size = 14))

### Plots for prediction 1: There are facilitative interactions between species in alpine plant communities that change to competitive when warmed.

lambda_diff_VA_WR_WC_lolliplot <- lambda_df_differences2 %>% 
  filter(Species == "Veronica alpina") %>% 
  filter(Treatments %in% c("WR")) %>% 
  mutate(precipitation = factor(precipitation, levels = c( "1.2 m/year", "2.3 m/year", "3.4 m/year", "Across precipitation"))) %>% 
  ggplot(aes(y = WC_comparison, x = Treatments, color = precipitation)) +
  geom_linerange(aes(ymin=0, ymax=WC_comparison, x= Treatments), position = position_dodge(width = 0.40))+
  geom_point(size = 5, position = position_dodge(width = 0.40)) +
  #facet_wrap(~Species, nrow = 2, scales = "free") +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = precip_3_palette) +
  theme_bw() +
  ylab("Δ λ") +
  xlab("") +
  ggtitle("WC") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(-0.25, 0.8)

lambda_diff_VA_CR_CC_lolliplot <- lambda_df_differences2 %>% 
  filter(Species == "Veronica alpina") %>% 
  filter(Treatments %in% c("CR")) %>% 
  mutate(precipitation = factor(precipitation, levels = c( "1.2 m/year", "2.3 m/year", "3.4 m/year", "Across precipitation"))) %>% 
  ggplot(aes(y = CC_comparison, x = Treatments, color = precipitation)) +
  geom_linerange(aes(ymin=0, ymax=CC_comparison, x= Treatments), position = position_dodge(width = 0.40))+
  geom_point(size = 5, position = position_dodge(width = 0.40)) +
  #facet_wrap(~Species, nrow = 2, scales = "free") +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = precip_3_palette) +
  theme_bw() +
  ylab("Δ λ") +
  xlab("") +
  ggtitle("CC") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(-0.25, 0.8)

lambda_diff_SP_WR_WC_lolliplot <- lambda_df_differences2 %>% 
  filter(Species == "Sibbaldia procumbens") %>% 
  filter(Treatments %in% c("WR")) %>% 
  mutate(precipitation = factor(precipitation, levels = c( "1.2 m/year", "2.3 m/year", "3.4 m/year", "Across precipitation"))) %>% 
  ggplot(aes(y = WC_comparison, x = Treatments, color = precipitation)) +
  geom_linerange(aes(ymin=0, ymax=WC_comparison, x= Treatments), position = position_dodge(width = 0.40))+
  geom_point(size = 5, position = position_dodge(width = 0.40)) +
  #facet_wrap(~Species, nrow = 2, scales = "free") +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = precip_3_palette) +
  theme_bw() +
  ylab("Δ λ") +
  xlab("") +
  ggtitle("WC") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(-0.1, 0.1)

lambda_diff_SP_CR_CC_lolliplot <- lambda_df_differences2 %>% 
  filter(Species == "Sibbaldia procumbens") %>% 
  filter(Treatments %in% c("CR")) %>% 
  mutate(precipitation = factor(precipitation, levels = c( "1.2 m/year", "2.3 m/year", "3.4 m/year", "Across precipitation"))) %>% 
  ggplot(aes(y = CC_comparison, x = Treatments, color = precipitation)) +
  geom_linerange(aes(ymin=0, ymax=CC_comparison, x= Treatments), position = position_dodge(width = 0.40))+
  geom_point(size = 5, position = position_dodge(width = 0.40)) +
  #facet_wrap(~Species, nrow = 2, scales = "free") +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = precip_3_palette) +
  theme_bw() +
  ylab("Δ λ") +
  xlab("") +
  ggtitle("CC") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(-0.1, 0.1)


P1_VA_plot <- ((lambda_diff_VA_CR_CC_lolliplot + lambda_diff_VA_WR_WC_lolliplot)) + 
  plot_annotation( title = "Changes in interactions within current plant communities",
                   subtitle = 'Veronica alpina') +
  plot_layout(widths = c(1,1), guides = 'collect') &
  theme(legend.position = "bottom", text = element_text(size = 14))

P1_SP_plot <- ((lambda_diff_SP_CR_CC_lolliplot + lambda_diff_SP_WR_WC_lolliplot)) + 
  plot_annotation( title = "Changes in interactions within current plant communities",
                   subtitle = 'Sibbaldia procumbens') +
  plot_layout(widths = c(1,1), guides = 'collect') &
  theme(legend.position = "bottom", text = element_text(size = 14))


### Plots for prediction 2: The immigration of functionally novel species is more negative to the population dynamics of alpine species than the immigration of functionally similar species, and more so under warming. 

lambda_diff_VA_CEN_CC_lolliplot <- lambda_df_differences2 %>% 
  filter(Species == "Veronica alpina") %>% 
  filter(Treatments %in% c("CE", "CN")) %>% 
  mutate(precipitation = factor(precipitation, levels = c( "1.2 m/year", "2.3 m/year", "3.4 m/year", "Across precipitation"))) %>% 
  ggplot(aes(y = CC_comparison, x = Treatments, color = precipitation)) +
  geom_linerange(aes(ymin=0, ymax=CC_comparison, x= Treatments), position = position_dodge(width = 0.40))+
  geom_point(size = 5, position = position_dodge(width = 0.40)) +
  #facet_wrap(~Species, nrow = 2, scales = "free") +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = precip_3_palette) +
  theme_bw() +
  ylab("Δ λ") +
  xlab("") +
  ggtitle("CC") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(-0.25, 0.8)

lambda_diff_VA_WEN_WC_lolliplot <- lambda_df_differences2 %>% 
  filter(Species == "Veronica alpina") %>% 
  filter(Treatments %in% c("WE", "WN")) %>% 
  mutate(precipitation = factor(precipitation, levels = c( "1.2 m/year", "2.3 m/year", "3.4 m/year", "Across precipitation"))) %>% 
  ggplot(aes(y = WC_comparison, x = Treatments, color = precipitation)) +
  geom_linerange(aes(ymin=0, ymax=WC_comparison, x= Treatments), position = position_dodge(width = 0.40))+
  geom_point(size = 5, position = position_dodge(width = 0.40)) +
  #facet_wrap(~Species, nrow = 2, scales = "free") +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = precip_3_palette) +
  theme_bw() +
  ylab("Δ λ") +
  xlab("") +
  ggtitle("WC") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(-0.25, 0.8)

lambda_diff_SP_CEN_CC_lolliplot <- lambda_df_differences2 %>% 
  filter(Species == "Sibbaldia procumbens") %>% 
  filter(Treatments %in% c("CE", "CN")) %>% 
  mutate(precipitation = factor(precipitation, levels = c( "1.2 m/year", "2.3 m/year", "3.4 m/year", "Across precipitation"))) %>% 
  ggplot(aes(y = CC_comparison, x = Treatments, color = precipitation)) +
  geom_linerange(aes(ymin=0, ymax=CC_comparison, x= Treatments), position = position_dodge(width = 0.40))+
  geom_point(size = 5, position = position_dodge(width = 0.40)) +
  #facet_wrap(~Species, nrow = 2, scales = "free") +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = precip_3_palette) +
  theme_bw() +
  ylab("Δ λ") +
  xlab("") +
  ggtitle("CC") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(-0.1, 0.1)

lambda_diff_SP_WEN_WC_lolliplot <- lambda_df_differences2 %>% 
  filter(Species == "Sibbaldia procumbens") %>% 
  filter(Treatments %in% c("WE", "WN")) %>% 
  mutate(precipitation = factor(precipitation, levels = c( "1.2 m/year", "2.3 m/year", "3.4 m/year", "Across precipitation"))) %>% 
  ggplot(aes(y = WC_comparison, x = Treatments, color = precipitation)) +
  geom_linerange(aes(ymin=0, ymax=WC_comparison, x= Treatments), position = position_dodge(width = 0.40))+
  geom_point(size = 5, position = position_dodge(width = 0.40)) +
  #facet_wrap(~Species, nrow = 2, scales = "free") +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = precip_3_palette) +
  theme_bw() +
  ylab("Δ λ") +
  xlab("") +
  ggtitle("WC") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(-0.1, 0.1)

P2_VA_plot <- ((lambda_diff_VA_CEN_CC_lolliplot + lambda_diff_VA_WEN_WC_lolliplot)) + 
  plot_annotation( title = "Indirect effects via. novel interactions (extant vs. novel traits)",
                   subtitle = 'Veronica alpina') +
  plot_layout(widths = c(1,1), guides = 'collect') &
  theme(legend.position = "bottom", text = element_text(size = 14))

P2_SP_plot <- ((lambda_diff_SP_CEN_CC_lolliplot + lambda_diff_SP_WEN_WC_lolliplot)) + 
  plot_annotation( title = "Indirect effects via. novel interactions (extant vs. novel traits)",
                   subtitle = 'Sibbaldia procumbens') +
  plot_layout(widths = c(1,1), guides = 'collect') &
  theme(legend.position = "bottom", text = element_text(size = 14))


#### Final plots with everything together

  



#######################

lambda_diff_SP_R_CC_lolliplot <- lambda_df_differences2 %>% 
  filter(Species == "Sibbaldia procumbens") %>% 
  filter(Treatments %in% c("WR", "CR")) %>% 
  mutate(precipitation = factor(precipitation, levels = c( "1.2 m/year", "2.3 m/year", "3.4 m/year", "Across precipitation"))) %>% 
  ggplot(aes(y = CC_comparison, x = Treatments, color = precipitation)) +
  geom_linerange(aes(ymin=0, ymax=CC_comparison, x= Treatments), position = position_dodge(width = 0.40))+
  geom_point(size = 5, position = position_dodge(width = 0.40)) +
  #facet_wrap(~Species, nrow = 2, scales = "free") +
  geom_hline(yintercept = 0) +
  scale_x_discrete(limits = rev) +
  scale_color_manual(values = precip_3_palette) +
  theme_bw() +
  ylab("Δ λ") +
  xlab("") +
  ggtitle("CC") +
  theme(plot.title = element_text(hjust = 0.5))

lambda_diff_VA_WR_CR_lolliplot <- lambda_df_differences2 %>% 
  filter(Species == "Veronica alpina") %>% 
  filter(Treatments %in% c("WR")) %>% 
  mutate(precipitation = factor(precipitation, levels = c( "1.2 m/year", "2.3 m/year", "3.4 m/year", "Across precipitation"))) %>% 
  ggplot(aes(y = CR_comparison, x = Treatments, color = precipitation)) +
  geom_linerange(aes(ymin=0, ymax=CR_comparison, x= Treatments), position = position_dodge(width = 0.40))+
  geom_point(size = 5, position = position_dodge(width = 0.40)) +
  #facet_wrap(~Species, nrow = 2, scales = "free") +
  geom_hline(yintercept = 0) +
  scale_x_discrete(limits = rev) +
  scale_color_manual(values = precip_3_palette) +
  theme_bw() +
  ylab("Δ λ") +
  xlab("") +
  ggtitle("CR") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(-0.15, 1.0)

lambda_diff_SP_WR_CR_lolliplot <- lambda_df_differences2 %>% 
  filter(Species == "Sibbaldia procumbens") %>% 
  filter(Treatments %in% c("WR")) %>% 
  mutate(precipitation = factor(precipitation, levels = c( "1.2 m/year", "2.3 m/year", "3.4 m/year", "Across precipitation"))) %>% 
  ggplot(aes(y = CR_comparison, x = Treatments, color = precipitation)) +
  geom_linerange(aes(ymin=0, ymax=CR_comparison, x= Treatments), position = position_dodge(width = 0.40))+
  geom_point(size = 5, position = position_dodge(width = 0.40)) +
  #facet_wrap(~Species, nrow = 2, scales = "free") +
  geom_hline(yintercept = 0) +
  scale_x_discrete(limits = rev) +
  scale_color_manual(values = precip_3_palette) +
  theme_bw() +
  ylab("Δ λ") +
  xlab("") +
  ggtitle("CR") +
  theme(plot.title = element_text(hjust = 0.5))

lambda_diff_VA_WR_WC_lolliplot <- lambda_df_differences2 %>% 
  filter(Species == "Veronica alpina") %>% 
  filter(Treatments %in% c("WR")) %>% 
  mutate(precipitation = factor(precipitation, levels = c( "1.2 m/year", "2.3 m/year", "3.4 m/year", "Across precipitation"))) %>% 
  ggplot(aes(y = WC_comparison, x = Treatments, color = precipitation)) +
  geom_linerange(aes(ymin=0, ymax=WC_comparison, x= Treatments), position = position_dodge(width = 0.40))+
  geom_point(size = 5, position = position_dodge(width = 0.40)) +
  #facet_wrap(~Species, nrow = 2, scales = "free") +
  geom_hline(yintercept = 0) +
  scale_x_discrete(limits = rev) +
  scale_color_manual(values = precip_3_palette) +
  theme_bw() +
  ylab("Δ λ") +
  xlab("") +
  ggtitle("WC") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(-0.15, 1.0)

lambda_diff_SP_WR_WC_lolliplot <- lambda_df_differences2 %>% 
  filter(Species == "Sibbaldia procumbens") %>% 
  filter(Treatments %in% c("WR")) %>% 
  mutate(precipitation = factor(precipitation, levels = c( "1.2 m/year", "2.3 m/year", "3.4 m/year", "Across precipitation"))) %>% 
  ggplot(aes(y = WC_comparison, x = Treatments, color = precipitation)) +
  geom_linerange(aes(ymin=0, ymax=WC_comparison, x= Treatments), position = position_dodge(width = 0.40))+
  geom_point(size = 5, position = position_dodge(width = 0.40)) +
  #facet_wrap(~Species, nrow = 2, scales = "free") +
  geom_hline(yintercept = 0) +
  scale_x_discrete(limits = rev) +
  scale_color_manual(values = precip_3_palette) +
  theme_bw() +
  ylab("Δ λ") +
  xlab("") +
  ggtitle("WC") +
  theme(plot.title = element_text(hjust = 0.5))


((lambda_diff_VA_R_CC_lolliplot + lambda_diff_VA_WR_CR_lolliplot + lambda_diff_VA_WR_WC_lolliplot)) + 
  plot_annotation( title = 'P1) There are facilitative interactions between species in \n alpine plant communities that change to competitive when warmed',
    subtitle = 'Veronica alpina') +
  plot_layout(widths = c(2,1,1), guides = 'collect') &
  theme(legend.position = "bottom", text = element_text(size = 14))


# lambda_diff_VA_R_C <- lambda_df_differences3 %>% 
#   filter(Species == "Veronica alpina") %>% 
#   filter(Treatments %in% c("WR", "WC")) %>% 
#   mutate(precipitation = factor(precipitation, levels = c( "1.2 m/year", "2.3 m/year", "3.4 m/year", "Across precipitation"))) %>% 
#   ggplot(aes(y = value, x = Treatments, color = precipitation)) +
#   geom_linerange(aes(ymin=0, ymax=value, x= Treatments), position = position_dodge(width = 0.40))+
#   geom_point(size = 5, position = position_dodge(width = 0.40)) +
#   facet_wrap(~name, nrow = 1) +
#   geom_hline(yintercept = 0) +
#   scale_x_discrete(limits = rev) +
#   scale_color_manual(values = precip_3_palette) +
#   theme_bw() +
#   ylab("Δ λ") +
#   xlab("") +
#   ggtitle("WC") +
#   theme(plot.title = element_text(hjust = 0.5))


# lambda_diff_VA_WR_WC_lolliplot <- lambda_df_differences2 %>% 
#   filter(Treatments == "CR") %>% 
#   mutate(precipitation = factor(precipitation, levels = c( "1.2 m/year", "2.3 m/year", "3.4 m/year", "Across precipitation"))) %>% 
#   ggplot(aes(y = WC_comparison, x = Species, color = precipitation)) +
#   geom_linerange(aes(ymin=0, ymax=WC_comparison, x=Species), position = position_dodge(width = 0.40))+
#   geom_point(size = 5, position = position_dodge(width = 0.40)) +
#   #facet_wrap(~Species, nrow = 2, scales = "free") +
#   geom_hline(yintercept = 0) +
#   scale_x_discrete(limits = rev) +
#   scale_color_manual(values = precip_3_palette) +
#   theme_bw() +
#   ylab("Δ λ") +
#   xlab("") +
#   ggtitle("WR - CR") +
#   theme(plot.title = element_text(hjust = 0.5))

lambda_diff_WC_CC_lolliplot <- lambda_df_differences2 %>% 
  filter(Treatments == "WC") %>% 
  mutate(precipitation = factor(precipitation, levels = c( "1.2 m/year", "2.3 m/year", "3.4 m/year", "Across precipitation"))) %>% 
  ggplot(aes(y = CC_comparison, x = Species, color = precipitation)) +
  geom_linerange(aes(ymin=0, ymax=CC_comparison, x=Species), position = position_dodge(width = 0.40))+
  geom_point(size = 5, position = position_dodge(width = 0.40)) +
  #facet_wrap(~Species, nrow = 2, scales = "free") +
  geom_hline(yintercept = 0) +
  scale_x_discrete(limits = rev) +
  scale_color_manual(values = precip_3_palette) +
  theme_bw() +
  ylab("Δ λ") +
  xlab("") +
  ggtitle("WC - CC") +
  theme(plot.title = element_text(hjust = 0.5))

lambda_diff_WN_CN_lolliplot <- lambda_df_differences2 %>% 
  filter(Treatments == "WN") %>% 
  mutate(precipitation = factor(precipitation, levels = c( "1.2 m/year", "2.3 m/year", "3.4 m/year", "Across precipitation"))) %>% 
  ggplot(aes(y = CN_comparison, x = Species, color = precipitation)) +
  geom_linerange(aes(ymin=0, ymax=CN_comparison, x=Species), position = position_dodge(width = 0.40))+
  geom_point(size = 5, position = position_dodge(width = 0.40)) +
  #facet_wrap(~Species, nrow = 2, scales = "free") +
  geom_hline(yintercept = 0) +
  scale_x_discrete(limits = rev) +
  scale_color_manual(values = precip_3_palette) +
  theme_bw() +
  ylab("Δ λ") +
  xlab("") +
  ggtitle("WN - CN") +
  theme(plot.title = element_text(hjust = 0.5))

lambda_diff_WE_CE_lolliplot <- lambda_df_differences2 %>% 
  filter(Treatments == "WE") %>% 
  mutate(precipitation = factor(precipitation, levels = c( "1.2 m/year", "2.3 m/year", "3.4 m/year", "Across precipitation"))) %>% 
  ggplot(aes(y = CE_comparison, x = Species, color = precipitation)) +
  geom_linerange(aes(ymin=0, ymax = CE_comparison, x=Species), position = position_dodge(width = 0.40))+
  geom_point(size = 5, position = position_dodge(width = 0.40)) +
  #facet_wrap(~Species, nrow = 2, scales = "free") +
  geom_hline(yintercept = 0) +
  scale_x_discrete(limits = rev) +
  scale_color_manual(values = precip_3_palette) +
  theme_bw() +
  ylab("Δ λ") +
  xlab("") +
  ggtitle("WE - CE") +
  theme(plot.title = element_text(hjust = 0.5))

lambda_diff_CN_CE_lolliplot <- lambda_df_differences2 %>% 
  filter(Treatments == "CN") %>% 
  mutate(precipitation = factor(precipitation, levels = c( "1.2 m/year", "2.3 m/year", "3.4 m/year", "Across precipitation"))) %>% 
  ggplot(aes(y = CE_comparison, x = Species, color = precipitation)) +
  geom_linerange(aes(ymin=0, ymax = CE_comparison, x=Species), position = position_dodge(width = 0.40))+
  geom_point(size = 5, position = position_dodge(width = 0.40)) +
  #facet_wrap(~Species, nrow = 2, scales = "free") +
  geom_hline(yintercept = 0) +
  scale_x_discrete(limits = rev) +
  scale_color_manual(values = precip_3_palette) +
  theme_bw() +
  ylab("Δ λ") +
  ggtitle("CN - CE") +
  theme(plot.title = element_text(hjust = 0.5))


( lambda_horizontal /  (lambda_diff_CR_lolliplot + lambda_diff_WC_CC_lolliplot) / (lambda_diff_WN_CN_lolliplot +lambda_diff_WE_CE_lolliplot ) / (lambda_diff_CN_CE_lolliplot )) + 
  plot_layout(heights = c(2,3,3,3), guides = 'collect') &
  theme(legend.position = "bottom", text = element_text(size = 14))

