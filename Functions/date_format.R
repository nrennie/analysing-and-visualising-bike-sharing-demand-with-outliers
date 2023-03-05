date_format <- function(date) {
  year <- year(as.Date(date, format = "%Y-%m-%d"))
  month <- month(as.Date(date, format = "%Y-%m-%d"))
  day <- day(as.Date(date, format = "%Y-%m-%d"))
  return(paste(year, "-", month, "-", day, sep = ""))
}