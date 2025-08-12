# plotDyn() is just a wrapper to display the number of studies,
# experiments or experiment-seasons included in the systematic review
# and the meta-analysis over time

  # -- df: the data source
  # -- x: the variable to display on the x-axis (e.g. publication year)
  # -- xlab, ylab: labels for x and y axes
  # -- included_only: TRUE to distinguish data included in the systematic
        # review only or in the meta-analysis

plotDyn <- function(df, x, xlab = NULL, ylab = NULL, included_only = F) {
  
  if (!x %in% colnames(df)) stop("Variable 'x' not found in dataframe.")
  if(included_only) df <- df %>% filter(Included == "Yes")
  
  p <- ggplot(df, aes(x = !!sym(x))) +
    geom_bar() +
    xlab(xlab) +
    ylab(paste("Number of", ylab)) +
    scale_y_continuous(breaks = scales::breaks_pretty()) +
    ggtitle(if (included_only ==T) "Included in the meta-analysis" else
      "Included in the systematic review")
  
  if (included_only == F) {
    p <- p + aes(fill = Included) +
      scale_fill_discrete("Included in the meta-analysis") +
      theme(legend.position = "bottom")
  }
  
  return(p)
}
