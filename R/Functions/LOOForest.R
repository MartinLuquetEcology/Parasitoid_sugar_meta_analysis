# LOOForest() plots the result from a sensitivity analysis
# performed using LOOTable()
# as a forest plot

# check LOOTable.R for an explanation of the analysis and for argument description
# additional argument:
  # -- ord: set to TRUE if you want to order effect sizes

LOOForest <- function(tab, int.model, level, ord = TRUE) {
  
  p <- 
  tab %>% 
    ggplot(aes(
      y = if(ord) fct_reorder(factor(level), beta, .desc = TRUE)
      else factor(level), 
      x = beta,
      xmin = ci.lb,
      xmax = ci.ub)
    ) +
    geom_point(aes(size = nb.ES)) +
    geom_errorbar() +
    # The grey line is the mean beta for all models
    geom_vline(xintercept = mean(tab$beta),
               linetype = "dashed",
               lwd = 1,
               col = "darkgrey") +
    # The red line is the beta from the full intercept-only model
    geom_vline(xintercept = coef(int.model)[1],
               col = "red",
               linetype = "dashed",
               lwd = 1) + 
    geom_vline(xintercept = int.model$ci.lb[1], 
               col="red", 
               linetype="dashed") +
    geom_vline(xintercept = int.model$ci.ub[1],
               col = "red",
               linetype = "dashed") +
    # The black line is 0
    geom_vline(xintercept = 0,
               lwd = 1.5) +
    xlab("Beta (+/- 95CI) when level removed") +
    ylab(level)
  
  return(p)
  
}
