# modelcompTab() returns various performance measures from meta-analytic models 
# fitted with metafor
# note that it calls orchaRd package (https://daniel1noble.github.io/orchaRd/)

# For a given response it gives performance for
# an intercept-only model with a model including moderators

# models should be named this way: meta."Name"."Feature".int
# e.g. Name = "PD" (pest density) and Feature = "Mean"

modelperfTab <- function(Name, Feature) {
  
  meta <- paste("meta", Name, Feature, sep = ".")
  meta.int <- paste0(meta, ".int")
  meta.mods <- paste0(meta, ".mods")
  
  tab.perf <- 
  sapply(c(meta.int, meta.mods), function(X) {
    c(
      orchaRd::i2_ml(get(X)), 
      orchaRd::r2_ml(get(X))
    )
  } 
  ) %>%
    t()
  
  return(tab.perf)
  
}