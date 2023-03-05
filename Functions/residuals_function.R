residuals_function <- function(data_matrix, factor_vec){
  data_matrix <- data_matrix[which(!is.na(data_matrix$date_of_day)),]
  if (is.na(nrow(data_matrix))){
    return(data_matrix)
  }
  if (nrow(data_matrix) <= 1){
    return(data_matrix)
  }
  dat <- data_matrix[,2:25]
  #define factors
  day <- factor(weekdays(data_matrix$date_of_day))
  month <- factor(months(data_matrix$date_of_day))
  year <- factor(year(data_matrix$date_of_day))
  n_day <- length(unique(day))
  n_month <- length(unique(month))
  n_year <- length(unique(year))
  #apply a regression to each time point
  res_matrix <- apply(dat, 2, function(y) {
    if (sum(factor_vec == c(1,1,1))==3){
      if (n_month > 1 & n_year > 1 & n_day > 1){
        mod <- lm(y ~ day + month + year)
      }
      else{
        mod <- lm(y ~ 1)
      }
    }
    if (sum(factor_vec == c(1,1,0))==3){
      if (n_month > 1 & n_day > 1){
        mod <- lm(y ~ month + day)
      }
      else{
        mod <- lm(y ~ 1)
      }
    }
    if (sum(factor_vec == c(1,0,1))==3){
      if (n_day > 1 & n_year > 1){
        mod <- lm(y ~ day + year)
      }
      else{
        mod <- lm(y ~ 1)
      }
    }
    if (sum(factor_vec == c(0,1,1))==3){
      if (n_month > 1 & n_year > 1){
        mod <- lm(y ~ month + year)
      }
      else{
        mod <- lm(y ~ 1)
      }
    }
    if (sum(factor_vec == c(0,0,1))==3){
      if (n_year > 1){
        mod <- lm(y ~ year)
      }
      else{
        mod <- lm(y ~ 1)
      }
    }
    if (sum(factor_vec == c(0,1,0))==3){
      if (n_month > 1){
        mod <- lm(y ~ month)
      }
      else{
        mod <- lm(y ~ 1)
      }    }
    if (sum(factor_vec == c(1,0,0))==3){
      if (n_day > 1){
        mod <- lm(y ~ day)
      }
      else{
        mod <- lm(y ~ 1)
      }
    }
    if (sum(factor_vec == c(0,0,0))==3){
      mod <- lm(y ~ 1)
    }
    res <- mod$residuals
    return(res)
  })
  colnames(res_matrix) <- c(0:23)
  output <- cbind(tibble(data_matrix[,1]), res_matrix)
  colnames(output) <- c("date_of_day", 0:23)
  return(tibble(output))
}


