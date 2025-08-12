# saForest() generates a forest plot of effect sizes
  # computed in a sensitivity analyses
  # were studies/experiments were iteratively removed for model fitting
  # it uses LOOTable() to refit the models and LOOForest() to generate the graph
  # Check these functions for argument description

saForest <- function(dat.table, int.model, var, level, ord = T) {
  
  tab <-  LOOTable(dat.table, int.model, var, level)
  LOOForest(tab, int.model, level)
  
}