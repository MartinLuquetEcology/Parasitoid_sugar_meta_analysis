# A function to quickly make diagnostic plots on a meta-analytic model
# Calls the tidyverse and ggpubr
  # -- mod: the model
  # -- title: desired overall title for the plot

diagQuick <- function(mod, title) {
  
  res <- residuals(mod) %>%
    as_tibble
  
  qqP <- 
    res %>%
    ggplot(aes(sample = value)) +
    stat_qq() +
    stat_qq_line() +
    labs( x = 'Theoretical quantiles',
          y = 'Sample quantiles',
          title = 'QQ-plot')
  
  dP <- 
    res %>%
    ggplot(aes(x = value)) +
    geom_histogram() +
    xlim(-max(abs(res)), max(abs(res))) +
    geom_vline(xintercept = 0, col = "red", lty = 'dashed') +
    labs(x = NULL,
         title = 'Residual distribution')
  
  
  tGrob <- ggpubr::text_grob(title, size = 12, face = 'bold')
  tP <- as_ggplot(tGrob) + theme(plot.margin = margin(0, -6, 0, 0, "cm"))
  
  ggpubr::ggarrange(tP, NULL, qqP, dP,
                    ncol = 2,nrow = 2,heights = c(1,5))
  
}  