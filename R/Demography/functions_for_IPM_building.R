##########################################################
### Script for functions used in the build IPM scripts ###
##########################################################

#### Functions ####
Precip_palette <- c("#BAD8F7", "#89B7E1", "#2E75B6", "#213964")
Precip_palette_black <- c("#BAD8F7", "#89B7E1", "#2E75B6", "#213964", "#000000")

plot_predictions_surv <-function(model, data, minSize, maxSize) {
  
  newdata <- expand.grid(size = seq(minSize, maxSize, length.out = 100),
                         blockID = data$blockID)
  
  newdata$predicted <- predict(object = model, newdata = newdata, re.form = NA, allow.new.levels=TRUE, type = "response")
  
  plot <- data %>% 
    ggplot(aes(x = size, y = surv, color = as.factor(precip))) +
    geom_jitter(height = 0.1) +
    geom_line(aes(x = size, y = predicted, color = "black"), data=newdata, size = 1, show.legend = TRUE) +
    ggtitle(paste0("AIC =", AIC(model))) +
    scale_color_manual(values = Precip_palette_black)+
    theme_minimal()
  
  
  
  return(plot)
}

plot_predictions_surv_precip <-function(model, data, minSize, maxSize) {
  
  newdata <- expand.grid(size = seq(minSize, maxSize, length.out = 100),
                         precip = c(1.226, 1.561, 2.130, 3.402),
                         blockID = data$blockID)
  
  newdata$predicted <- predict(object = model, newdata = newdata, re.form = NA, allow.new.levels=TRUE, type = "response")
  
  plot <- data %>% 
    ggplot(aes(x = size, y = surv, color = as.factor(precip))) +
    geom_jitter(height = 0.1) +
    geom_line(aes(x = size, y = predicted, color = factor(precip)), data=newdata, size = 1, show.legend = TRUE) +
    ggtitle(paste0("AIC =", AIC(model))) +
    scale_color_manual(values = Precip_palette) +
    theme_minimal()
  
  
  return(plot)
}

plot_predictions_growth_precip <-function(model, data, minSize, maxSize) {
  
  newdata <- expand.grid(size = seq(minSize, maxSize, length.out = 100),
                         precip = c(1.226, 1.561, 2.130, 3.402),
                         transition = c("2018-2019", "2019-2020", "2020-2021"),
                         blockID = data$blockID)
  
  newdata$predicted <- predict(object = model, newdata = newdata, re.form = NA, allow.new.levels=TRUE, type = "response")
  
  plot <- data %>% 
    ggplot(aes(x = size, y = sizeNext, color = as.factor(precip))) +
    geom_jitter(height = 0.1) +
    geom_line(aes(x = size, y = predicted, color = factor(precip)), data=newdata, size = 1, show.legend = TRUE) +
    ggtitle(paste0("AIC =", AIC(model))) +
    geom_abline() +
    ylim(minSize, maxSize) +
    scale_color_manual(values = Precip_palette) +
    theme_minimal()
  
  
  return(plot)
}

plot_predictions_growth <-function(model, data, minSize, maxSize) {
  
  newdata <- expand.grid(size = seq(minSize, maxSize, length.out = 100),
                         blockID = data$blockID)
  
  newdata$predicted <- predict(object = model, newdata = newdata, re.form = NA, allow.new.levels=TRUE, type = "response")
  
  plot <- data %>% 
    ggplot(aes(x = size, y = sizeNext, color = as.factor(precip))) +
    geom_jitter(height = 0.1) +
    geom_line(aes(x = size, y = predicted, color = "black"), data=newdata, size = 1, show.legend = TRUE) +
    ggtitle(paste0("AIC =", AIC(model))) +
    geom_abline() +
    ylim(minSize, maxSize) +
    scale_color_manual(values = Precip_palette_black) +
    theme_minimal()
  
  
  return(plot)
}


plot_predictions_floif_precip <-function(model, data, minSize, maxSize) {
  
  newdata <- expand.grid(size = seq(minSize, maxSize, length.out = 100),
                         precip = c(1.226, 1.561, 2.130, 3.402),
                         blockID = data$blockID)
  
  newdata$predicted <- predict(object = model, newdata = newdata, re.form = NA, allow.new.levels=TRUE, type = "response")
  
  plot <- data %>% 
    ggplot(aes(x = size, y = flo.if, color = as.factor(precip))) +
    geom_jitter(height = 0.1) +
    geom_line(aes(x = size, y = predicted, color = factor(precip)), data=newdata, size = 1, show.legend = TRUE) +
    ggtitle(paste0("AIC =", AIC(model))) +
    scale_color_manual(values = Precip_palette)+
    theme_minimal()
  
  
  return(plot)
}

plot_predictions_floif <-function(model, data, minSize, maxSize) {
  
  newdata <- expand.grid(size = seq(minSize, maxSize, length.out = 100),
                         blockID = data$blockID)
  
  newdata$predicted <- predict(object = model, newdata = newdata, re.form = NA, allow.new.levels=TRUE, type = "response")
  
  plot <- data %>% 
    ggplot(aes(x = size, y = flo.if, color = as.factor(precip))) +
    geom_jitter(height = 0.1) +
    geom_line(aes(x = size, y = predicted, color = "black"), data=newdata, size = 1, show.legend = TRUE) +
    ggtitle(paste0("AIC =", AIC(model))) +
    scale_color_manual(values = Precip_palette_black)+
    theme_minimal()
  
  
  return(plot)
}

plot_predictions_flono_precip <-function(model, data, minSize, maxSize, ylim) {
  
  newdata <- expand.grid(size = seq(minSize, maxSize, length.out = 100),
                         precip = c(1.226, 1.561, 2.130, 3.402),
                         blockID = data$blockID)
  
  newdata$predicted <- predict(object = model, newdata = newdata, re.form = NA, allow.new.levels=TRUE, type = "response")
  
  plot <- data %>% 
    ggplot(aes(x = size, y = flo.no, color = as.factor(precip))) +
    geom_jitter(height = 0.1) +
    geom_line(aes(x = size, y = predicted, color = factor(precip)), data=newdata, size = 1, show.legend = TRUE) +
    ggtitle(paste0("AIC =", AIC(model))) +
    ylim(0, ylim) +
    scale_color_manual(values = Precip_palette)+
    theme_minimal()
  
  
  return(plot)
}

plot_predictions_flono <-function(model, data, minSize, maxSize, ylim) {
  
  newdata <- expand.grid(size = seq(minSize, maxSize, length.out = 100),
                         blockID = data$blockID)
  
  newdata$predicted <- predict(object = model, newdata = newdata, re.form = NA, allow.new.levels=TRUE, type = "response")
  
  plot <- data %>% 
    ggplot(aes(x = size, y = flo.no, color = as.factor(precip))) +
    geom_jitter(height = 0.1) +
    geom_line(aes(x = size, y = predicted, color = "black"), data=newdata, size = 1, show.legend = TRUE) +
    ggtitle(paste0("AIC =", AIC(model))) +
    ylim(0, ylim) +
    scale_color_manual(values = Precip_palette_black) +
    theme_minimal()
  
  
  return(plot)
}

plot_predictions_cloif_precip <-function(model, data, minSize, maxSize) {
  
  newdata <- expand.grid(size = seq(minSize, maxSize, length.out = 100),
                         precip = c(1.226, 1.561, 2.130, 3.402),
                         blockID = data$blockID)
  
  newdata$predicted <- predict(object = model, newdata = newdata, re.form = NA, allow.new.levels=TRUE, type = "response")
  
  plot <- data %>% 
    ggplot(aes(x = size, y = clo.if, color = as.factor(precip))) +
    geom_jitter(height = 0.1) +
    geom_line(aes(x = size, y = predicted, color = factor(precip)), data=newdata, size = 1, show.legend = TRUE) +
    ggtitle(paste0("AIC =", AIC(model))) +
    scale_color_manual(values = Precip_palette) +
    theme_minimal()
  
  
  return(plot)
}

plot_predictions_cloif <-function(model, data, minSize, maxSize) {
  
  newdata <- expand.grid(size = seq(minSize, maxSize, length.out = 100),
                         blockID = data$blockID)
  
  newdata$predicted <- predict(object = model, newdata = newdata, re.form = NA, allow.new.levels=TRUE, type = "response")
  
  plot <- data %>% 
    ggplot(aes(x = size, y = clo.if, color = as.factor(precip))) +
    geom_jitter(height = 0.1) +
    geom_line(aes(x = size, y = predicted, color = "black"), data=newdata, size = 1, show.legend = TRUE) +
    ggtitle(paste0("AIC =", AIC(model))) +
    scale_color_manual(values = Precip_palette_black) +
    theme_minimal()
  
  
  return(plot)
}

plot_predictions_clono_precip <-function(model, data, minSize, maxSize, ylim) {
  
  newdata <- expand.grid(size = seq(minSize, maxSize, length.out = 100),
                         precip = c(1.226, 1.561, 2.130, 3.402),
                         blockID = data$blockID)
  
  newdata$predicted <- predict(object = model, newdata = newdata, re.form = NA, allow.new.levels=TRUE, type = "response")
  
  plot <- data %>% 
    ggplot(aes(x = size, y = clo.no, color = as.factor(precip))) +
    geom_jitter(height = 0.1) +
    geom_line(aes(x = size, y = predicted, color = factor(precip)), data=newdata, size = 1, show.legend = TRUE) +
    ggtitle(paste0("AIC =", AIC(model))) +
    ylim(0, ylim) +
    scale_color_manual(values = Precip_palette) +
    theme_minimal()
  
  
  return(plot)
}

plot_predictions_clono <-function(model, data, minSize, maxSize, ylim) {
  
  newdata <- expand.grid(size = seq(minSize, maxSize, length.out = 100),
                         blockID = data$blockID)
  
  newdata$predicted <- predict(object = model, newdata = newdata, re.form = NA, allow.new.levels=TRUE, type = "response")
  
  plot <- data %>% 
    ggplot(aes(x = size, y = clo.no, color = as.factor(precip))) +
    geom_jitter(height = 0.1) +
    geom_line(aes(x = size, y = predicted, color = "black"), data=newdata, size = 1, show.legend = TRUE) +
    ggtitle(paste0("AIC =", AIC(model))) +
    ylim(0, ylim) +
    scale_color_manual(values = Precip_palette_black) +
    theme_minimal()
  
  
  return(plot)
}

contourPlot2 <- function(M,meshpts,maxSize,upper,lower, title) {
  q <- sum(meshpts<=maxSize);
  plot <- filled.contour(meshpts[1:q],meshpts[1:q],M[1:q,1:q], zlim=c(upper,lower),
                         xlab="size at time t", ylab="size at time t+1", main = title, color=heat.colors, nlevels=20, cex.lab=1.5,
                         plot.axes = { axis(1); axis(2); lines(-10:50, -10:50, lty=2)});
  return(0);
}

IPM_plot <- function(IPM_control, IPM_treatment = NULL, vital = FALSE, vital_rate_matrix_treatment, vital_rate_matrix_control, minSize, maxSize, zrange) {
  
  if(isTRUE(vital)) {
    BaseIPM <- (IPM_control + IPM_treatment) / 2
    SBaseIPM <- sens(BaseIPM)
    matrix <- vital_rate_matrix_control - vital_rate_matrix_treatment
  } else {
    matrix <- IPM_control
  }
  
  long_data <- as.data.frame(matrix) %>% 
    set_names(seq(minSize, maxSize, length = ncol(matrix))) %>% 
    mutate(size = seq(minSize, maxSize, length = nrow(matrix))) %>% 
    pivot_longer(cols = -size, names_to = "sizeNext") %>% 
    mutate(sizeNext = as.numeric(sizeNext))
  
  plot <- ggplot(long_data, aes(x = sizeNext, y = size)) + 
    geom_raster(aes(fill=value)) + 
    scale_fill_viridis_c(limits = zrange, na.value = "white") +
    labs(x="Size at  time t", y="Size at time t+1") +
    theme_bw() + 
    # theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
    # axis.text.y=element_text(size=9)) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    geom_abline()
  
  return(plot);
}
