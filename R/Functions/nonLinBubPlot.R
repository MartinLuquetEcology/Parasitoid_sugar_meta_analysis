nonLinBubPlot <- function(model, moderator, es,
                          xlab, resp) {
  
  df <- model$data
  moder <- pull(df, moderator)
  
  # Grid for prediction
  xi <- seq(range(moder)[1], range(moder)[2], length = 100)
  newdat <- data.frame(xi)
  names(newdat) <- moderator
  
  # Retrieving the full model matrix
  X <- model.matrix(formula(model), data = df)
  
  # Re-constructing the matrix for prediction
  
  # Using the mean (continuous variables)
  X_ref <- colMeans(X)
  
  newmods <- matrix(
    rep(X_ref[-1], length(xi)),
    # -1 because we don't need the intercept column
    nrow = length(xi),
    byrow = TRUE
  )
  
  colnames(newmods) <- colnames(X)[-1]
  
  # New values for the desired moderator
  # Converting back to the models' polynomial base
  form_poly <- as.formula(paste0("~ poly(", moderator, ", 2)"))
  X_poly <- model.matrix(form_poly, data = newdat)[, -1]
  newmods[, colnames(X_poly)] <- X_poly
  
  
  # Prediction
  pred_df <- predict(
    model,
    newmods = newmods
  )
  pred_df$x <- xi
  
  # For plot title
  index <- ifelse(es == 'lnRR', 'mean', 'variance')
  
  # Now plotting
  df %>%
    ggplot(aes(x = get(moderator), y = get(paste0(es, '_yi')))) +
    geom_point(aes(size = 1/lnRR_sei), shape = 21, fill = 'grey') +
    geom_line(
      data = as.data.frame(pred_df),
      aes(x = x, y = pred),
      linewidth = 1
    ) +
    # CI
    geom_ribbon(
      data = as.data.frame(pred_df),
      aes(x = x, ymin = ci.lb, ymax = ci.ub, y = pred), alpha = .2,
      linewidth = 1
    ) +
    # PI
    geom_line(
      data = as.data.frame(pred_df),
      aes(x = x, y = pi.lb),
      lty = 'dashed'
    ) +
    geom_line(
      data = as.data.frame(pred_df),
      aes(x = x, y = pi.ub),
      lty = 'dashed'
    ) +
    # Theme
    theme_bw() +
    theme(legend.position = c(1, 0.97), 
          legend.justification = c(1, 1),
          legend.direction = 'horizontal') +
    # Custom
    labs(x = xlab, 
         y = paste0('Effect size (', es, ')'),
         title = paste('Effect of', tolower(xlab), 'on the', index, 'of', tolower(resp)),
         size = 'Precision (1/SE)')
  
}
