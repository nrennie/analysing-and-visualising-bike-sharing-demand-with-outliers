sse_function <- function(data_matrix, factor_vec){
  err <- numeric(length=nrow(data_matrix))
  for (i in 1:nrow(data_matrix)){
    dat <- data_matrix[-i,]
    #fit model 
    mod_matrix <- residuals_function_model_comp(dat, factor_vec) 
    #amke prediction
    da <- weekdays(data_matrix$date_of_day[i])
    mo <- months(data_matrix$date_of_day[i])
    ye <- as.character(year(data_matrix$date_of_day[i]))
    m <- unlist(lapply(mod_matrix, function(x) predict(x, newdata=data.frame(day=da, month=mo, year=ye))))
    err[i] <- sum((unlist(data_matrix[i,2:25]) - m))^2
  }
  return(sum(err, na.rm=T))
}