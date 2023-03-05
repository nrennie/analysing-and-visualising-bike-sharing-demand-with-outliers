# library(geosphere)
# library(ICSNP)
# #load station coordinates
# setwd("C:/Users/rennien/OneDrive - Lancaster University/PhD/Simulation/Project 4 - Bike Visualisation/Bike_Data/Stations")
# station_data <- readRDS("station_data.rds")
# c <- spatial.median(station_data[,c(3,2)])

c_dist_centre <- function(station, c){
  a <- which(station_data$TERMINAL_NUMBER == station)
  lat_a <- station_data$LATITUDE[a]
  lat_b <- c[2]
  lon_a <- station_data$LONGITUDE[a]
  lon_b <- c[1]
  distance <- distm(c(lon_a, lat_a), c(lon_b, lat_b), fun = distHaversine)
  return(distance)
}

