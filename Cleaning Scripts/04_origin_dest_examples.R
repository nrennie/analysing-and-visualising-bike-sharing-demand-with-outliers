# Source functions script -------------------------------------------------

source("Cleaning Scripts/00_packages.R")


# Read data ---------------------------------------------------------------

OD_data <- readRDS("Data/OD_data.rds")
agg_station_matrix <- readRDS("Data/agg_station_matrix.rds")


# Origin destination data -------------------------------------------------

data_by_OD <- split(OD_data, OD_data$OD)
d1 <- data_by_OD$`31094-31924`
d2 <- data_by_OD$`31203-31654`
d3 <- data_by_OD$`31204-31002`
saveRDS(d1, "Data/OD1.rds")
saveRDS(d2, "Data/OD2.rds")
saveRDS(d3, "Data/OD3.rds")
