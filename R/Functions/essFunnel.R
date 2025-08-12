# rmaQQplot() displays a funnel plot based on effective sample size
  # see Nakagawa et al. 2022, 2023
# for a meta-analytic model fitted with metafor

# model should be named this way: meta."Name"."Feature".int
  # e.g. Name = "PD" (pest density) and Feature = "Mean"

# the effect size variable should be coded in a column named "ES"_yi
  # e.g. lnRR_yi for log Response Ratio
  # in that case 'ES' argument would be "lnRR"

essFunnel <- function(Name, Feature, ES) {
  
  dat <- paste("meta", Name, "dat", sep = ".")
  ESname <- paste0(ES, "_yi")
  
  res <- paste0("mResults_", Name, ".", Feature)
  res.int <- paste0(res, ".int")
  
  p <-
  get(dat) %>%
    ggplot(aes(x = get(ESname), y = ess_var)) +
    geom_point() +
    xlim(-ceiling(max(abs(get(dat)[ESname]), na.rm = T)),
         ceiling(max(abs(get(dat)[ESname]), na.rm = T))) +
    geom_vline(xintercept = get(res.int)$mod_table$estimate, 
               lty = "dashed") +
    xlab(ESname)
  
  return(p)
  
}
