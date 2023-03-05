# Source functions script -------------------------------------------------

source("Cleaning Scripts/00_packages.R")


# Read data ---------------------------------------------------------------

station_data <- readRDS("Data/station_data.rds")


# Make adjacency matrices -------------------------------------------------

adj_mat <- matrix(0, nrow = nrow(station_data), ncol = nrow(station_data))
for (i in 1:nrow(station_data)) {
  for (j in 1:nrow(station_data)) {
    if (i > j) {
      if (c_dist(station_data$TERMINAL_NUMBER[i], station_data$TERMINAL_NUMBER[j]) < 1000) {
        adj_mat[i, j] <- 1
        adj_mat[j, i] <- 1
      }
    }
  }
}


adj_mat2 <- matrix(0, nrow = nrow(station_data), ncol = nrow(station_data))
for (i in 1:nrow(station_data)) {
  for (j in 1:nrow(station_data)) {
    if (i > j) {
      # dist i and j < x from capitol
      if (station_data$capitol_distances[i] < 10000 & station_data$capitol_distances[i] < 10000) {
        if (c_dist(station_data$TERMINAL_NUMBER[i], station_data$TERMINAL_NUMBER[j]) < 500) {
          adj_mat2[i, j] <- 1
          adj_mat2[j, i] <- 1
        }
      }
      # else
      else {
        if (c_dist(station_data$TERMINAL_NUMBER[i], station_data$TERMINAL_NUMBER[j]) < 1000) {
          adj_mat2[i, j] <- 1
          adj_mat2[j, i] <- 1
        }
      }
    }
  }
}


adj_mat2b <- matrix(0, nrow = nrow(station_data), ncol = nrow(station_data))
for (i in 1:nrow(station_data)) {
  for (j in 1:nrow(station_data)) {
    if (i > j) {
      # dist i and j < x from capitol
      if (station_data$centre_distances[i] < 5000 & station_data$centre_distances[i] < 5000) {
        if (c_dist(station_data$TERMINAL_NUMBER[i], station_data$TERMINAL_NUMBER[j]) < 500) {
          adj_mat2b[i, j] <- 1
          adj_mat2b[j, i] <- 1
        }
      }
      # else
      else {
        if (c_dist(station_data$TERMINAL_NUMBER[i], station_data$TERMINAL_NUMBER[j]) < 1000) {
          adj_mat2b[i, j] <- 1
          adj_mat2b[j, i] <- 1
        }
      }
    }
  }
}



# Save files --------------------------------------------------------------

saveRDS(adj_mat, "Data/adj_mat.rds")
saveRDS(adj_mat2, "Data/adj_mat2.rds")
saveRDS(adj_mat2b, "Data/adj_mat2b.rds")
