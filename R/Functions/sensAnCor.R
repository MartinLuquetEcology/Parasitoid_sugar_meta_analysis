# To check models' sensitivity to chosen correlation levels for dependent effect sizes
  # Sensitivity of the overall effect estimation
  # As well as sensitivity of publication bias detection
  
  # -- df: the dataset used to fit models
  # -- ES: the name of the Effect size (character string, e.g. "lnRR")
  # -- mod: the model not unadjusted for publication bias
  # -- mod.pb: the model adjusted for publication bias

sensAnCor <- function(df, ES, mod, mod.pb) {
  
  cor_lvls <- c(0.3, 0.5, 0.7, 0.9)
  
  l.vcov <- 
    lapply(cor_lvls, function(Rho) {
      metafor::vcalc(vi = get(paste0(ES, "_vi")), 
                     cluster = clusterID, 
                     obs = ES_ID, 
                     data = df, 
                     rho = Rho)
    } )
  
  # Model unadjusted for publication bias
  mods_nopb <- 
    lapply(l.vcov, function(v)
      update(mod, V = v))
  
  df_nopb <- 
  lapply(mods_nopb, 
         function(c) coefficients(summary(c))) %>%
    bind_rows(.id = "cor_level") %>%
    mutate(cor_level = cor_lvls) %>%
    remove_rownames
  
  # Model adjusted for publication bias
  mods_pb <- 
    lapply(l.vcov, function(v)
      update(mod.pb, V = v))
  
  df_pb <- 
  lapply(mods_pb , 
         function(c) {
           coefficients(summary(c)) %>%
             as.data.frame %>%
             rownames_to_column}) %>% 
    bind_rows(.id = "cor_level") %>%
    mutate(cor_level = rep(c(0.3, 0.5, 0.7, 0.9), each = 3))
  
  out <- list("Model unadjusted for publication bias" = df_nopb,
              "Model adjusted for publication bias" = df_pb)
  
  return(out)
}


