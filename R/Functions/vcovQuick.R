# A function to quickly evaluate a variance-covariance matrix
# calls metafor
  # -- es: type of effect size, as a character string (lnRR, lnCVR)
  # -- df: dataset
  # -- rho: value of correlation coefficient (default as 0.5)

vcovQuick <- function(es, df, rho = 0.5) {
  
  metafor::vcalc(vi = get(paste0(es, '_vi')), 
                 cluster = clusterID, 
                 obs = ES_ID, 
                 data = df, 
                 rho = rho)
}
