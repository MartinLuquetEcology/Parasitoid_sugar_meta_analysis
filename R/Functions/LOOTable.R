# LOOtable() re-fits a model excluding effect sizes at a given level
# Study, experiment or effect size
# in a leave-one-out fashion (e.g. removing one study/experiment/effect size at a time
# only works for intercept-only models for now
  # updated: also works for models including additional moderators

# Argument description:
  # -- df: the dataset used to fit the model
  # -- int.model: the intercept-only meta-analytic model
  # -- ES_var: the name of the column containing the effect size (as a character)
  # -- level: the level at which LOO should be done (study, experiment, effect size)
    # as a character corresponding to a column name (e.g. "Exp_ID" for experiment)

LOOTable <- function(df, int.model, ES_var, level) {
  
  # Data frame that will record estimates for each model
  LOO.table <- 
    df %>%
    mutate(level = !!sym(level)) %>%
    group_by(level) %>%
    summarise(
      beta = NA,
      ci.lb = NA,
      ci.ub = NA,
      nb.ES = n()
    )
  
  # In this loop we extract beta and CIs for each model missing one study
  for(i in LOO.table$level) {
    
    #print(i)
    
    sub <- df %>% 
      filter(!!sym(level) != i) 
    
    vcovMat <- vcalc(vi = get(ES_var), 
                     cluster = clusterID, 
                     obs = ES_ID, 
                     data = sub, 
                     rho = 0.5)
    
    mod <- update(int.model, V = vcovMat, data = sub)
    
    lvs <- LOO.table$level == i
    LOO.table[lvs, "beta"] <- mod$beta[1]
    LOO.table[lvs, "ci.lb"] <- mod$ci.lb[1]
    LOO.table[lvs, "ci.ub"] <- mod$ci.ub[1]
    
  }
  
  return(LOO.table)
}
