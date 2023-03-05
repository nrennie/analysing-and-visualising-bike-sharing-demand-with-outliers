season_split <- function(data, summer=c("April", "May", "June", "July", "August", "September", "October"), winter=c("November", "December", "January", "February", "March")){
  output <- list()
  output[[1]] <- data[which(months(data$date_of_day) %in% summer),]
  output[[2]] <- data[which(months(data$date_of_day) %in% winter),]
  names(output) <- c("summer", "winter")
  return(output)
}