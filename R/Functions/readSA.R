# readSA() loads results from a sensitivity analysis
# generated using LOOTable() and LOOForest()
# and saved with saveSA()

readSA <- function(Name, Feature, level, postSA = F) {
  
  readRDS(
    paste0("Outputs/Sensitivity_analyses/", 
           paste("meta", Name, Feature, "int", "SA", level, sep = "."),
           if(postSA ==T) ".postSA",
           ".RDS")
  )
  
}