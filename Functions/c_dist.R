c_dist <- function(station_a, station_b){
  a <- which(station_data$TERMINAL_NUMBER == station_a)
  b <- which(station_data$TERMINAL_NUMBER == station_b)
  lat_a <- station_data$LATITUDE[a]
  lat_b <- station_data$LATITUDE[b]
  lon_a <- station_data$LONGITUDE[a]
  lon_b <- station_data$LONGITUDE[b]
  distance <- distm(c(lon_a, lat_a), c(lon_b, lat_b), fun = distHaversine)
  return(distance)
}
