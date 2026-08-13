plot_quality_coefs <- function(target_class, coef_data) {
  
  coef_data %>%
    filter(Class == target_class) %>%
    mutate(
      abs_coef = abs(coefficient),
      Direction = ifelse(coefficient > 0, "Positive Coef", "Negative Coef")
    ) %>%
    arrange(desc(abs_coef)) %>%
    slice(1:15) %>% # Keep top 15 predictors for this specific class
    mutate(variable = fct_reorder(variable, abs_coef)) %>% # Reorder for clean bar chart
    ggplot(aes(x = coefficient, y = variable, fill = Direction)) +
    geom_col() +
    scale_fill_manual(
      values = c("Positive Coef" = "steelblue", "Negative Coef" = "firebrick")
    ) +
    labs(
      title = paste("Top Predictors for Signal Quality:", target_class),
      x = "Elastic Net Coefficient", 
      y = ""
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom")
}


plotdescriptives_signal_bar <- function(groupmeandata, subjectdata, dv, iv, ymin, ymax, ybreaks, xbreaklabels, ylabel, xlabel, plottitle, plotsubtitle) {
  
  yvarmean <- paste(dv,'_mean', sep='')
  yvarse <- paste(dv,'_se', sep='')
  
  groupmeandata$yvar <- groupmeandata[[yvarmean]]
  subjectdata$yvar <- subjectdata[[dv]]
  
  groupmeandata$yvarse <- groupmeandata[[yvarse]]

  groupmeandata$xvar <- groupmeandata[[iv]]
  subjectdata$xvar <- subjectdata[[iv]]
  
  
  ggplot(groupmeandata, aes(x = xvar, y = yvar, color = Signal, fill = Signal)) +
    geom_col(position = "identity", linewidth = mycollinewidth, width=mycolwidth, show.legend=FALSE) +
    geom_errorbar(data = groupmeandata, aes(x = xvar, ymin = yvar-2*yvarse, ymax = yvar+2*yvarse),
                  position = "identity", width = myerrorbarwidth, linewidth = myerrorbarlinewidth, show.legend = FALSE, color='#000000')+
    geom_point(data = subjectdata, aes(x = xvar, y = yvar, shape = SignalQuality),
               position = position_jitterdodge(jitter.width = mycolwidth, dodge.width = 0), color='#1C1C1C', 
               size = mysubjectpointsize, stroke=mysubjectpointstroke, alpha=1, show.legend = FALSE) +
    scale_fill_manual(values = colors_signal) +
    scale_color_manual(values = colors_signal) +
    scale_shape_manual(values=shapes_signalquality) +
    scale_y_continuous(expand=c(0,0),limits = c(ymin, ymax), 
                       breaks=ybreaks) +
    scale_x_discrete(labels = xbreaklabels) +
    labs(title = plottitle,
         subtitle = plotsubtitle,
         x = xlabel, 
         y = ylabel) +
    mytheme
  
  
}
