# plotEs() plots the distribution of effect sizes (ES) in a given dataset
# It generates a forest plot and a density plot
# data has to be formatted as in tab.es (check main script)
# with columns containing effect size means and variances
# coded as lnRR_yi, lnRR_ci.lb, lnRR_ci.ub, etc.


  # -- df: the data source
  # -- var: the variable for which we want ES distribution
    # (e.g. parasitism rate, parasitoid abundance, ...)
  # -- ES: the effect size for which we want to plot the distribution
    # (e.g. Hedge's D, lnRR...)

plotES <- function(df, var, ES) {
  
    # Just checking if required columns are there
  cols_needed <- paste(ES, c("yi", "ci.lb", "ci.ub"), sep = "_")
  if (!all(cols_needed %in% colnames(df))) {
    stop("Missing required columns for effect size: ", 
         paste(cols_needed, collapse = ", "))
  }
  
  # Preparing data for plotting
  tab <- df %>%
    filter(Variable == var) %>%
    arrange(get(paste(ES, "yi", sep = "_"))) %>%
    mutate(ES_ID = row_number())
  
  # Forest plot
  fp <- ggplot(tab, 
               aes(x = get(paste(ES, "yi", sep = "_")), 
                   y = ES_ID, 
                   col = Tmt_sugar)) +
    geom_pointrange(aes(xmin = get(paste(ES, "ci.lb", sep = "_")), 
                        xmax = get(paste(ES, "ci.ub", sep = "_")))) +
    geom_vline(xintercept = 0) +
    ylab(var) +
    theme(legend.position = "bottom") +
    xlab(ES)
  
  
  # Density plot
  dp <- ggplot(tab,  
               aes(x = get(paste(ES, "yi", sep = "_")))) +
    geom_density(alpha = 0.4, color = "darkblue",  fill = "lightblue") +
    ylab(NULL) +
    xlab(ES)               
  
  # Combining, arranging and returning plots
  p <- ggpubr::ggarrange(fp, dp)
  
  return(
    annotate_figure(p, 
                    top = text_grob(paste("Distribution of", ES, "for", var),
                                    face = "bold", size = 14))
  )
  
}