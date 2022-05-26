#### Making vital rate plots of all the treatments

library(patchwork)


##### Veronica alpina #####

#Growth by size
(plot_growth_VA_CC + ggtitle("VA CC") |plot_growth_VA_CE + ggtitle("VA CE") | plot_growth_VA_CN + ggtitle("VA CN") |plot_growth_VA_CR + ggtitle("VA CR"))  /
    (plot_growth_VA_WC + ggtitle("VA WC") |plot_growth_VA_WE + ggtitle("VA WE") | plot_growth_VA_WN + ggtitle("VA WN") |plot_growth_VA_WR + ggtitle("VA WR")) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')

#Survival by size
(plot_surv_VA_CC + ggtitle("VA CC") |plot_surv_VA_CE + ggtitle("VA CE") | plot_surv_VA_CN + ggtitle("VA CN") |plot_surv_VA_CR + ggtitle("VA CR"))  /
  (plot_surv_VA_WC + ggtitle("VA WC") |plot_surv_VA_WE + ggtitle("VA WE") | plot_surv_VA_WN + ggtitle("VA WN") |plot_surv_VA_WR + ggtitle("VA WR")) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')

#Does the individual flower?
(plot_VA_CC_floif + ggtitle("VA CC") |plot_VA_CE_floif + ggtitle("VA CE") | plot_VA_CN_floif + ggtitle("VA CN") |plot_VA_CR_floif + ggtitle("VA CR"))  /
  (plot_VA_WC_floif + ggtitle("VA WC") |plot_VA_WE_floif + ggtitle("VA WE") | plot_VA_WN_floif + ggtitle("VA WN") |plot_VA_WR_floif + ggtitle("VA WR")) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')

#If the individual flowers, how many flowers does it produce?
(plot_flo_no_VA_CC + ggtitle("VA CC") |plot_flo_no_VA_CE + ggtitle("VA CE") | plot_flo_no_VA_CN + ggtitle("VA CN") |plot_flo_no_VA_CR + ggtitle("VA CR"))  /
  (plot_flo_no_VA_WC + ggtitle("VA WC") |plot_flo_no_VA_WE + ggtitle("VA WE") | plot_flo_no_VA_WN + ggtitle("VA WN") |plot_flo_no_VA_WR + ggtitle("VA WR")) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')

#Does the individual make clones?
(plot_clo_if_VA_CC + ggtitle("VA CC") |plot_clo_if_VA_CE + ggtitle("VA CE") | plot_clo_if_VA_CN + ggtitle("VA CN") |plot_clo_if_VA_CR + ggtitle("VA CR"))  /
  (plot_clo_if_VA_WC + ggtitle("VA WC") |plot_clo_if_VA_WE + ggtitle("VA WE") | plot_clo_if_VA_WN + ggtitle("VA WN") |plot_clo_if_VA_WR + ggtitle("VA WR")) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')

#If the individual makes clones, how many clones does it make?
(plot_clo_no_VA_CC + ggtitle("VA CC") |plot_clo_no_VA_CE + ggtitle("VA CE") | plot_clo_no_VA_CN + ggtitle("VA CN") |plot_clo_no_VA_CR + ggtitle("VA CR"))  /
  (plot_clo_no_VA_WC + ggtitle("VA WC") |plot_clo_no_VA_WE + ggtitle("VA WE") | plot_clo_no_VA_WN + ggtitle("VA WN") |plot_clo_no_VA_WR + ggtitle("VA WR")) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')

#is the size of the clone dependent on the size of the parent?
(plot_clone_growth_VA_CC + ggtitle("VA CC") |plot_clone_growth_VA_CE + ggtitle("VA CE") | plot_clone_growth_VA_CN + ggtitle("VA CN") |plot_clone_growth_VA_CR + ggtitle("VA CR"))  /
  (plot_clone_growth_VA_WC + ggtitle("VA WC") |plot_clone_growth_VA_WE + ggtitle("VA WE") | plot_clone_growth_VA_WN + ggtitle("VA WN") |plot_clone_growth_VA_WR + ggtitle("VA WR")) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')

##### Sibbaldia procumbens #####

#Growth by size
(plot_growth_SP_CC + ggtitle("SP CC") |plot_growth_SP_CE + ggtitle("SP CE") | plot_growth_SP_CN + ggtitle("SP CN") |plot_growth_SP_CR + ggtitle("SP CR"))  /
  (plot_growth_SP_WC + ggtitle("SP WC") |plot_growth_SP_WE + ggtitle("SP WE") | plot_growth_SP_WN + ggtitle("SP WN") |plot_growth_SP_WR + ggtitle("SP WR")) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')

#Survival by size
(plot_surv_SP_CC + ggtitle("SP CC") |plot_surv_SP_CE + ggtitle("SP CE") | plot_surv_SP_CN + ggtitle("SP CN") |plot_surv_SP_CR + ggtitle("SP CR"))  /
  (plot_surv_SP_WC + ggtitle("SP WC") |plot_surv_SP_WE + ggtitle("SP WE") | plot_surv_SP_WN + ggtitle("SP WN") |plot_surv_SP_WR + ggtitle("SP WR")) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')

#Does the individual flower?
(plot_SP_CC_floif + ggtitle("SP CC") |plot_SP_CE_floif + ggtitle("SP CE") | plot_SP_CN_floif + ggtitle("SP CN") |plot_SP_CR_floif + ggtitle("SP CR"))  /
  (plot_SP_WC_floif + ggtitle("SP WC") |plot_SP_WE_floif + ggtitle("SP WE") | plot_SP_WN_floif + ggtitle("SP WN") |plot_SP_WR_floif + ggtitle("SP WR")) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')

#If the individual flowers, how many flowers does it produce?
(plot_flo_no_SP_CC + ggtitle("SP CC") |plot_flo_no_SP_CE + ggtitle("SP CE") | plot_flo_no_SP_CN + ggtitle("SP CN") |plot_flo_no_SP_CR + ggtitle("SP CR"))  /
  (plot_flo_no_SP_WC + ggtitle("SP WC") |plot_flo_no_SP_WE + ggtitle("SP WE") | plot_flo_no_SP_WN + ggtitle("SP WN") |plot_flo_no_SP_WR + ggtitle("SP WR")) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')

#Does the individual make clones?
(plot_clo_if_SP_CC + ggtitle("SP CC") |plot_clo_if_SP_CE + ggtitle("SP CE") | plot_clo_if_SP_CN + ggtitle("SP CN") |plot_clo_if_SP_CR + ggtitle("SP CR"))  /
  (plot_clo_if_SP_WC + ggtitle("SP WC") |plot_clo_if_SP_WE + ggtitle("SP WE") | plot_clo_if_SP_WN + ggtitle("SP WN") |plot_clo_if_SP_WR + ggtitle("SP WR")) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')

#If the individual makes clones, how many clones does it make?
(plot_clo_no_SP_CC + ggtitle("SP CC") |plot_clo_no_SP_CE + ggtitle("SP CE") | plot_clo_no_SP_CN + ggtitle("SP CN") |plot_clo_no_SP_CR + ggtitle("SP CR"))  /
  (plot_clo_no_SP_WC + ggtitle("SP WC") |plot_clo_no_SP_WE + ggtitle("SP WE") | plot_clo_no_SP_WN + ggtitle("SP WN") |plot_clo_no_SP_WR + ggtitle("SP WR")) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')

#is the size of the clone dependent on the size of the parent?
(plot_clone_growth_SP_CC + ggtitle("SP CC") |plot_clone_growth_SP_CE + ggtitle("SP CE") | plot_clone_growth_SP_CN + ggtitle("SP CN") |plot_clone_growth_SP_CR + ggtitle("SP CR"))  /
  (plot_clone_growth_SP_WC + ggtitle("SP WC") |plot_clone_growth_SP_WE + ggtitle("SP WE") | plot_clone_growth_SP_WN + ggtitle("SP WN") |plot_clone_growth_SP_WR + ggtitle("SP WR")) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')




     