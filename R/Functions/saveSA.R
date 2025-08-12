# saveSA() saves results from a sensitivity analysis
# generated using LOOTable() and LOOForest()

saveSA <- function(SA) {
  
  saveRDS(SA, 
          paste0("Outputs/Sensitivity_analyses/", 
                 deparse(substitute(SA)),
                 ".RDS"))
}