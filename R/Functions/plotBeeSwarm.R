# Custom orchard plots using ggbeeswarm::geom_quasirandom()
plotBeeSwarm <- function(df, res, index, es_lab,
                          point_size = 2.5, ci_lwd = 1.2, pi_lwd = 0.4,
                          move_legend = T) {
  
  p <- 
    df %>%
    ggplot() +
    ggbeeswarm::geom_quasirandom(
      aes(y = yi, x = Response, size = 1/sqrt(vi), 
          colour = Response, fill = Response),
      alpha = 0.6) +
    geom_hline(yintercept = 0, lty = "dashed") +
    xlab(NULL) +
    coord_flip() +
    guides(fill = "none",
           colour = "none") +
    # PI
    geom_linerange(data = res, 
                   aes(x = Response, 
                       ymin = lowerPR, 
                       ymax = upperPR),
                   position = ggplot2::position_dodge2(width = 0.1),
                   linewidth = pi_lwd,
                   col = 'brown') +
    # CI
    geom_linerange(data = res, 
                   aes( 
                     x = Response, 
                     ymin = lowerCL, 
                     ymax = upperCL),
                   position = ggplot2::position_dodge2(width = 0.1),
                   linewidth = ci_lwd) +
    # Points
    geom_point(data = res, 
               aes(y = estimate, 
                   x = Response, 
                   shape = fct_rev(Overall_estimate)),
               position = ggplot2::position_dodge2(width = 0.1),
               size = point_size) +
    # General stuff
    labs(title = paste("Meta-analysis of", index),
         y = es_lab,
         size = "Precision") +
    scale_shape_discrete("Overall estimate") +
    theme(legend.position = "bottom",
          axis.text.y = element_blank())+
    scale_y_continuous(breaks = scales::pretty_breaks(3))
  
  if(move_legend) {
    
    p <- p +
      guides(shape = "none") +
      theme(legend.position.inside = c(1, 0),
            legend.justification.inside = c(1, 0),
            legend.position = "inside",
            legend.direction = "horizontal",
            legend.text = element_text(
              size = 6
            ))
    
  }
  
  return(p)
  
}
