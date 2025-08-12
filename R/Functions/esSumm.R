esSumm <- 
  function(Name, Feature) {
    
    mod_comp <- 
      modelCompTab(Name, Feature) %>%
      select(Model, AIC, pval) %>%
      pivot_wider(names_from = Model, values_from = c(AIC),
                  names_glue = "AIC ({Model})") %>%
      select(-pval, pval) %>%
      summarise(across(everything(), ~ first(na.omit(.)))) %>%
      rename(`p-val. (Comparison)` = pval)
    
    mod_name <- paste("meta", Name, Feature, "int", sep = ".")
    mod_beta <- tibble(
      `beta (Intercept)` = get(mod_name)$beta[1],
      `p-val. (Intercept)` = get(mod_name)$pval
    )
    
    to_rename <- c(`I²` = "I2_Total")
    
    mod_int_perf <- 
      modelperfTab(Name, Feature) %>%
      as_tibble() %>%
      slice(1) %>%
      select(any_of(c("I2_Total"))) %>%
      dplyr::rename(any_of(to_rename))
    
    cross_join(tibble(Feature = Feature),
               cbind(mod_comp, mod_beta, mod_int_perf))
    
  }