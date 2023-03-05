residuals_function_model_comp <- function(data_matrix, factor_vec) {
  dat <- data_matrix[, 2:25]
  # define factors
  day <- factor(weekdays(data_matrix$date_of_day))
  month <- factor(months(data_matrix$date_of_day))
  year <- factor(year(data_matrix$date_of_day))
  # apply a regression to each time point
  mod_matrix <- apply(dat, 2, function(y) {
    y <- unlist(y)
    if (sum(factor_vec == c(1, 1, 1)) == 3) {
      mod <- lm(y ~ day + month + year)
    }
    if (sum(factor_vec == c(1, 1, 0)) == 3) {
      mod <- lm(y ~ day + month)
    }
    if (sum(factor_vec == c(1, 0, 1)) == 3) {
      mod <- lm(y ~ day + year)
    }
    if (sum(factor_vec == c(0, 1, 1)) == 3) {
      mod <- lm(y ~ month + year)
    }
    if (sum(factor_vec == c(0, 0, 1)) == 3) {
      mod <- lm(y ~ year)
    }
    if (sum(factor_vec == c(0, 1, 0)) == 3) {
      mod <- lm(y ~ month)
    }
    if (sum(factor_vec == c(1, 0, 0)) == 3) {
      mod <- lm(y ~ day)
    }
    if (sum(factor_vec == c(0, 0, 0)) == 3) {
      mod <- lm(y ~ 1)
    }
    return(mod)
  })
  return(mod_matrix)
}
