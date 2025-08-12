# modelcompTab() compares meta-analytic models fitted with metafor
# For a given response it compares
# an intercept-only model with a model including moderators
  # Note that models are re-fitted via maximum likelihood if needed

# models should be named this way: meta."Name"."Feature".int
# e.g. Name = "PD" (pest density) and Feature = "Mean"

modelCompTab <- function(Name, Feature) {
  
  meta <- paste("meta", Name, Feature, sep = ".")
  meta.int <- paste0(meta, ".int")
  meta.mods <- paste0(meta, ".mods")
  
  tab.comp <- 
  anova.rma(get(meta.int), get(meta.mods), refit = TRUE) %>%
    as_tibble() %>%
    mutate(Model = c("With moderators", "Intercept only")) %>%
    select(Model, everything()) %>%
    arrange(-row_number())
  
  return(tab.comp)
  
}