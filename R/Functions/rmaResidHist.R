# rmaResidHist() displays an histogram of residual distribution
# for a meta-analytic model fitted with metafor
# model should be named this way: meta."Name"."Feature".int
# e.g. Name = "PD" (pest density) and Feature = "Mean"

# Additional arguments :
  # -- Complete_name: to give a different name to the variable in the title
  # -- ES: to display the type of effect size displayed (e.g. lnRR)
  # -- pb: set to TRUE for models that adjust for publication bias
    ## and for which R objects include ".pb" at the end of their name
  # -- postSA: set to TRUE for models on which sensitivity analyses were applied
  
rmaResidHist <- function(Name, Feature, Complete_name = Name, ES = NULL, pb = F, postSA = F) {
  
  meta <- paste("meta", Name, Feature, sep = ".")
  if(postSA) meta <- paste0(meta, ".postSA")
  meta.int <- paste0(meta, ".int")
  if(pb) meta.int <- paste0(meta.int, ".pb")
  
  p <- 
  residuals(get(meta.int)) %>%
    as_tibble() %>%
    ggplot(aes(x = value)) +
    geom_histogram() +
    xlim(
      -max(abs(residuals(get(meta.int)))),
      max(abs(residuals(get(meta.int))))) +
    geom_vline(xintercept = 0, col = "red", lty = "dashed")  +
    ggtitle(paste(Complete_name, Feature, "Meta-analysis: Residual distribution"),
            subtitle = ES) +
    theme(axis.title = element_blank())
  
  return(p)
  
}