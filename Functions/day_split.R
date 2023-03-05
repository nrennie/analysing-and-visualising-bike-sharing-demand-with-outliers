day_split <- function(data, weekday, weekend){
  output <- list()
  output[[1]] <- data[which(weekdays(data$date_of_day) %in% weekday),]
  output[[2]] <- data[which(weekdays(data$date_of_day) %in% weekend),]
  names(output) <- c("weekday", "weekend")
  return(output)
}