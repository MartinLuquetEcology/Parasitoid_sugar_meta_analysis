# modelcompTab() compares meta-analytic models fitted with metafor
# For a given response it compares
# an intercept-only model with a model including moderators

# models should be named this way: meta."Name"."Feature".int
# e.g. Name = "PD" (pest density) and Feature = "Mean"
# the suffix ".postSA" can also be added to the model name
  # for models on which sensitivity analyses were applied
  # then set postSA = T

modResultsAdjPb <- function(Name, Feature, postSA = F) {
  
  meta <- paste("meta", Name, Feature, sep = ".")
  if(postSA) meta <- paste0(meta, ".postSA")
  meta.int <- paste0(meta, ".int")
  meta.int.pb <- paste0(meta.int, ".pb")
  
  tab <- 
  bind_rows(
    list(
      Adjusted = 
        mod_results(get(meta.int.pb), 
                    mod = "1", 
                    group = "Exp_ID", 
                    at = list(ess_se = 0, year.c = 0))$mod_table,
      Unadjusted = 
        mod_results(get(meta.int),
                    mod = "1",
                    group = "Exp_ID")$mod_table
    )
    , .id = "Estimate"
  )
  
  return(tab)
  
}
