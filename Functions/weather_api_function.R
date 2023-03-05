# get data from Visual Crossing API
weather_api_function <- function(date) {
  dashes <- which(unlist(strsplit(date, "")) == "-")
  year <- as.numeric(paste(unlist(strsplit(date, ""))[1:(dashes[1] - 1)], collapse = ""))
  month <- month(as.Date(date, format = "%Y-%m-%d"))
  day <- as.numeric(paste(unlist(strsplit(date, ""))[(dashes[2] + 1):nchar(date)], collapse = ""))
  myurl <- paste0("https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/timeline/washington%20dc/", year, "-", month, "-", day, "?unitGroup=us&key=", key, "&include=obs%2Chours", sep = "")
  WeatherData <- read.csv(myurl)
  # temp
  k2 <- sapply(names(WeatherData), function(x) substr(x, 1, 5))
  k3 <- which(k2 == "temp.")
  temp_vec <- sapply(names(k3)[1:24], function(x) temp_extract(x))
  names(temp_vec) <- 0:23
  # rain
  k4 <- sapply(names(WeatherData), function(x) substr(x, 1, 7))
  k5 <- which(k4 == "precip.")
  rain_vec <- sapply(names(k5), function(x) rain_extract(x))
  names(rain_vec) <- 0:23
  # save
  saveRDS(rain_vec, paste0("Weather Data/", date, ".rds", sep = ""))
  saveRDS(temp_vec, paste0("Weather Data/", date, ".rds", sep = ""))
}

# process temperature
temp_extract <- function(temp_val) {
  pos <- which(unlist(strsplit(temp_val, "")) == ".")[1]
  pos2 <- which(unlist(strsplit(temp_val, "")) == ".")[2]
  output <- paste(unlist(strsplit(temp_val, ""))[(pos + 1):(pos2 + 1)], collapse = "")
  return(as.numeric(output))
}

# process rain
rain_extract <- function(rain_val) {
  pos <- which(unlist(strsplit(rain_val, "")) == ".")[1]
  pos2 <- which(unlist(strsplit(rain_val, "")) == ".")[2]
  if (is.na(pos2)) {
    return(0)
  } else {
    output <- paste(unlist(strsplit(rain_val, ""))[(pos + 1):(pos2 + 1)], collapse = "")
    return(as.numeric(output))
  }
}