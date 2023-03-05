# Source functions script -------------------------------------------------

source("Cleaning Scripts/00_packages.R")


# Read data ---------------------------------------------------------------

OD_data <- readRDS("Data/OD_data.rds")


# Convert to matrix -------------------------------------------------------

data_by_start_station <- split(OD_data, OD_data$Start.station.number)
start_station_matrix <- lapply(data_by_start_station, function(x) matrix_function(x))
saveRDS(start_station_matrix, "Data/start_station_matrix.rds")

data_by_end_station <- split(OD_data, OD_data$End.station.number)
end_station_matrix <- lapply(data_by_end_station, function(x) matrix_function_end(x))
saveRDS(end_station_matrix, "Data/end_station_matrix.rds")
