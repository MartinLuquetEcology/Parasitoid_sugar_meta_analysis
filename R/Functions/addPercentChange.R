# Adding vertical lines (pct change) to an orchard plot to guide interpretation

addPercentChange <- function(values = c(0.25, 1), col = 'black', lwd = 0.25) {
  
  back_t <- c(log(1 + values), -log(1 + values))
  geom_hline(yintercept = back_t, lty = "dashed", lwd = lwd, col = col)
  
}