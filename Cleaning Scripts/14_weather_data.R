# Source functions script -------------------------------------------------

source("Cleaning Scripts/00_packages.R")


# Read data ---------------------------------------------------------------

start_station_matrix <- readRDS("Data/start_station_matrix.rds")
end_station_matrix <- readRDS("Data/end_station_matrix.rds")


# Get and process weather data --------------------------------------------

my_dates <- seq.Date(as.Date("2017/1/1"), as.Date("2019/12/31"), "days")
choose_date <- sapply(my_dates, function(x) date_format(x))
sapply(choose_date[1:1095], function(x) weather_api_function(x))
file_names <- sapply(choose_date, function(x) paste(x, ".rds", sep = ""))

# save rain data
rain_matrix <- data.frame(matrix(NA, ncol = 25, nrow = length(file_names)))
rain_matrix[, 1] <- as.Date(my_dates, origin = "1970-01-01", format = "%Y-%m-%d")
colnames(rain_matrix) <- c("date_of_day", 0:23)
for (i in 1:length(file_names)) {
  rain_matrix[i, 2:25] <- as.numeric(readRDS(file_names[i]))[1:24]
}
saveRDS(rain_matrix, "Weather Data/rain_matrix.rds")

# save temp data
temp_matrix <- data.frame(matrix(NA, ncol = 25, nrow = length(file_names)))
temp_matrix[, 1] <- as.Date(my_dates, origin = "1970-01-01", format = "%Y-%m-%d")
colnames(temp_matrix) <- c("date_of_day", 0:23)
for (i in 1:length(file_names)) {
  temp_matrix[i, 2:25] <- as.numeric(readRDS(file_names[i]))[1:24]
}
saveRDS(temp_matrix, "Weather Data/temp_matrix.rds")
