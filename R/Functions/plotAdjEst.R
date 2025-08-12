# plotAdjEst() plots both the unadjusted effet size for a meta-analytic model
# as well as the effect size adjusted for publication bias

# model should be named this way: meta."Name"."Feature".int
  # e.g. Name = "PD" (pest density) and Feature = "Mean"

# Additional arguments :
  # -- Complete_name: to give a different name to the variable in the title
  # -- postSA: set to TRUE for models that include sensitivity analyses
    ## and for which R objects include ".postSA" at the end of their name

# To be improved! Rather than using model objects we should use the raw data
# to get kStudies, kExp, etc.
# and we could get additional indices such as kES, kStud, KExp, kSeas

plotAdjEst <- function(Name, Feature, ES, Complete_name = Name, postSA = F) {
  
  res <- paste0("mResults_", Name, ".", Feature)
  if(postSA == T) res <- paste0(res, ".postSA")
  res.int <- paste0(res, ".int")
  res.adj <- paste0(res, ".Adj")
  
  if(!exists(res.adj)) 
    assign(res.adj, 
           get(res.int)$mod_table %>%
             mutate(Estimate = "Unadjusted"))
  
  col <- ifelse(Feature == "Mean", "#CC6677", "#88CCEE")
  feat_name <- ifelse(Feature == "Mean", "mean", "variance")
  
  get(res.int)$data %>%
    ggplot() +
    ggbeeswarm::geom_quasirandom(
      aes(y = yi, x = moderator, size = 1/sqrt(vi), 
          colour = moderator, fill = moderator),
      alpha = 0.6) +
    geom_hline(yintercept = 0, lty = "dashed") +
    xlab(NULL) +
    ylab(paste0(ES, " (", Complete_name, ")")) +
    coord_flip() +
    guides(fill = "none", 
           colour = "none") +
    scale_size_continuous("Precision (1/SE)") +
    theme(legend.position.inside = c(0, 1),
          legend.justification.inside = c(0, 1),
          legend.position = "inside") +
    scale_colour_manual(values = col) +
    ggnewscale::new_scale_fill() +
    geom_linerange(data = get(res.adj), 
                   aes( 
                     x = name, 
                     ymin = lowerCL, 
                     ymax = upperCL),
                   position = ggplot2::position_dodge2(width = 0.1),
                   size = 1.2) +
    geom_pointrange(data = get(res.adj), 
                    aes(y = estimate, 
                        x = name, 
                        ymin = lowerPR, 
                        ymax = upperPR,
                        fill = Estimate,
                        shape = Estimate),
                    position = ggplot2::position_dodge2(width = 0.1),
                    fatten = 6) +
    scale_shape_manual(values = c(21, 22))  +
    ggtitle(paste(Complete_name, "- Meta-analysis of", feat_name, "effect",
                  if(postSA == T) "(after sensitivity analysis and paper exclusion)"),
            subtitle = "Intercept-only model") +
    scale_fill_manual(values = c(col, NA)) +
    annotate("text", 
             y = (max(get(res.int)$data$yi) * 0.9), 
             x = 1.3, 
             label = paste0("k = ", nrow(get(res.int)$data), " (",
                            length(unique(get(res.int)$data$stdy)),
                            ")")) +
    guides(size = guide_legend(order = 1))
  
}