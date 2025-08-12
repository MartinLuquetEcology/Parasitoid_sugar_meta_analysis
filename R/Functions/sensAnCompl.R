  # To run the whole sensitivity analysis process at once, iteratively removing:
    # Effect sizes
    # Experiments
    # Papers
    # Experiment type (observational vs experimental)
    # Distance category (i.e. where measurements were made in the field)

  # By default the model will not run a new analysis (run_sens_ana = FALSE) and
  # will load already existing files (load_sens_ana = TRUE)
  # This can be changed manually
    # check R/Functions/readSA.R for more details

  # Requires a dataset named this way: meta."Name".dat
  # And an (intercept-only) model named this way: meta."Name"."Feature".int

  # Additional arguments :
    # -- Complete_name: to give a different name to the variable in the title
    # -- postSA: set to TRUE for models that include sensitivity analyses
      ## and for which R objects include ".postSA" at the end of their name



sensAnCompl <- function(Name, Feature, ES, Complete_name, postSA = F, 
                        run_sens_ana = FALSE, load_sens_ana = TRUE) {
  
  sensAn <- list()
  
  if(run_sens_ana == TRUE) {
    
    dat <- paste0("meta.", Name, ".dat")
    mod <- paste("meta", Name, Feature, "int", sep = ".")
    levels <- c("ES_ID", "Exp_ID", "Paper_ID", 
                "Exp_type_1", "Distance_category")
    
    for (lv in levels) {
      
      SA <- saForest(get(dat), get(mod), paste0(ES, "_vi"), lv)
      saveRDS(SA, 
              paste0("Outputs/Sensitivity_analyses/", 
                     paste("meta", Name, Feature, "int", "SA", lv, "RDS", sep = "."))
      )
    }
    
    
    
  }
  
  if(load_sens_ana == TRUE) {
    
    if(ES %in% c("lnRR", "HedgeD")) metrics <- "mean"
    if(ES == "lnCVR") metrics <- "variance"
    
    title <- paste("Sensitivity analysis of", Complete_name, metrics)
    
    sensAn[["ES_ID"]] <- 
      readSA(Name, Feature, "ES_ID", postSA) +
      ggtitle(title, "Removing Effect sizes one by one")
    
    sensAn[["Exp_ID"]] <- 
      readSA(Name, Feature, "Exp_ID", postSA) +
      ggtitle(title, "Removing Experiments one by one")
    
    sensAn[["Paper_ID"]] <- 
      readSA(Name, Feature, "Paper_ID", postSA) +
      ggtitle(title, "Removing Papers one by one")
    
    sensAn[["Exp_type_1"]] <- 
      readSA(Name, Feature, "Exp_Type_1", postSA) +
      ggtitle(title, "Removing Experiment types one by one")
    
    sensAn[["Distance_Category"]] <- 
      readSA(Name, Feature, "Distance_Category", postSA) +
      ggtitle(title, "Removing Distance categories one by one")
    
  }
  
  return(sensAn)
  
}