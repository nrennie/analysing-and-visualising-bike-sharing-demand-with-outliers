day_season_split <- function(data, weekday, weekend, summer, winter){
  output <- list()
  output[[1]] <- data[which(weekdays(data$date_of_day) %in% weekday & months(data$date_of_day) %in% summer),]
  output[[2]] <- data[which(weekdays(data$date_of_day) %in% weekend & months(data$date_of_day) %in% summer),]
  output[[3]] <- data[which(weekdays(data$date_of_day) %in% weekday & months(data$date_of_day) %in% winter),]
  output[[4]] <- data[which(weekdays(data$date_of_day) %in% weekend & months(data$date_of_day) %in% winter),]
  names(output) <- c("weekday_s", "weekend_s", "weekday_w", "weekend_w")
  return(output)
}