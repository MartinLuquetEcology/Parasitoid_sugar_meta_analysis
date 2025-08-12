# pooledSd(): computes the pooled standard deviation
# when data are measured over several distances

  # -- n: sample size for each "distance" treatment
  # -- sd: standard deviation for each "distance" treatment

pooledSd <- function(n, sd){
  num <- sum(n * sd^2)
  denom <- sum(n) - length(n)
  value <- sqrt(num/denom)
  return(value)
}