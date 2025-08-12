# rmaQQplot() displays a quantile-quantile plot 
# for a meta-analytic model fitted with metafor
# model should be named this way: meta."Name"."Feature".int
# e.g. Name = "PD" (pest density) and Feature = "Mean"

# Additional arguments :
  # -- Complete_name: to give a different name to the variable in the title
  # -- ES: to display the type of effect size displayed (e.g. lnRR)
  # -- pb: set to TRUE for models that adjust for publication bias
    ## and for which R objects include ".pb" at the end of their name
  # -- postSA: set to TRUE for models on which sensitivity analyses were applied

rmaQQplot <- function(Name, Feature, Complete_name = Name, ES = NULL, pb = F, postSA = F) {
  
  meta <- paste("meta", Name, Feature, sep = ".")
  if(postSA) meta <- paste0(meta, ".postSA")
  meta.int <- paste0(meta, ".int")
  if(pb) meta.int <- paste0(meta.int, ".pb")
  
  p <-
  residuals(get(meta.int)) %>%
    as_tibble() %>%
    ggplot(aes(sample = value)) +
    stat_qq() +
    stat_qq_line() +
    xlab("Theoretical quantiles") +
    ylab("Sample quantiles") +
    ggtitle(paste("QQ-plot of", Complete_name, Feature, "Meta-analysis"),
            subtitle = ES)
  
  return(p)
  
}

