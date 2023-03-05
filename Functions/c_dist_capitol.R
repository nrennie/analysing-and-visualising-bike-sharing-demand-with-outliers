library(geosphere)

#load station coordinates
setwd("C:/Users/rennien/OneDrive - Lancaster University/PhD/Simulation/Project 4 - Bike Visualisation/Bike_Data/Stations")
station_data <- readRDS("station_data.rds")

c_dist_capitol <- function(station){
  a <- which(station_data$TERMINAL_NUMBER == station)
  lat_a <- station_data$LATITUDE[a]
  lat_b <- 38.8899
  lon_a <- station_data$LONGITUDE[a]
  lon_b <- -77.0091
  distance <- distm(c(lon_a, lat_a), c(lon_b, lat_b), fun = distHaversine)
  return(distance)
}

#for each distance, compute distance to centre of dc
capitol_distances <- sapply(station_data$TERMINAL_NUMBER, function(x) c_dist_capitol(x))
station_data$capitol_distances <- capitol_distances

