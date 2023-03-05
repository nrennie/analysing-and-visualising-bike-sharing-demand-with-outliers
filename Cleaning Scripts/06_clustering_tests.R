# Source functions script -------------------------------------------------

source("Cleaning Scripts/00_packages.R")
source("Cleaning Scripts/03_process_matrices.R")

# Read data ---------------------------------------------------------------

station_data <- readRDS("Data/station_data.rds")
agg_cor_mat <- readRDS("Data/agg_cor_mat.rds")


# Run tests ---------------------------------------------------------------

# Test clustering threshold
c <- seq(-1, 1, 0.05)
num_clusts_c <- numeric(length(c))
sdcs_c <- numeric(length(c))
for (i in 1:length(c)) {
  clustering_c <- clustering(cor_mat = agg_cor_mat, station_data = station_data, D_inner = 500, D_outer = 1000, R = 5000, corr_threshold = c[i])
  num_clusts_c[i] <- length(clustering_c)
  size_clusts <- sapply(clustering_c, function(x) length(x))
  sdcs_c[i] <- sdcs_function(size_clusts)
}

# Test radius
R <- seq(0, 30000, 1000)
num_clusts_R <- numeric(length(R))
sdcs_R <- numeric(length(R))
for (i in 1:length(R)) {
  clustering_R <- clustering(cor_mat = agg_cor_mat, station_data = station_data, D_inner = 500, D_outer = 1000, R = R[i], corr_threshold = 0.15)
  num_clusts_R[i] <- length(clustering_R)
  size_clusts <- sapply(clustering_R, function(x) length(x))
  sdcs_R[i] <- sdcs_function(size_clusts)
}

# Test inner distances
D_i <- seq(0, 5000, 250)
num_clusts_D_i <- numeric(length(D_i))
sdcs_D_i <- numeric(length(D_i))
for (i in 1:length(D_i)) {
  clustering_D_i <- clustering(cor_mat = agg_cor_mat, station_data = station_data, D_inner = D_i[i], D_outer = 1000, R = 5000, corr_threshold = 0.15)
  num_clusts_D_i[i] <- length(clustering_D_i)
  size_clusts <- sapply(clustering_D_i, function(x) length(x))
  sdcs_D_i[i] <- sdcs_function(size_clusts)
}

# Test outer distances
D_o <- seq(0, 5000, 250)
num_clusts_D_o <- numeric(length(D_o))
sdcs_D_o <- numeric(length(D_o))
for (i in 1:length(D_o)) {
  clustering_D_o <- clustering(cor_mat = agg_cor_mat, station_data = station_data, D_inner = 500, D_outer = D_o[i], R = 5000, corr_threshold = 0.15)
  num_clusts_D_o[i] <- length(clustering_D_o)
  size_clusts <- sapply(clustering_D_o, function(x) length(x))
  sdcs_D_o[i] <- sdcs_function(size_clusts)
}


# Save results ------------------------------------------------------------

saveRDS(num_clusts_c, "Data/num_clusts_c.rds")
saveRDS(sdcs_c, "Data/sdcs_c.rds")
saveRDS(num_clusts_R, "Data/num_clusts_R.rds")
saveRDS(sdcs_R, "Data/sdcs_R.rds")
saveRDS(num_clusts_D_i, "Data/num_clusts_D_i.rds")
saveRDS(sdcs_D_i, "Data/sdcs_D_i.rds")
saveRDS(num_clusts_D_o, "Data/num_clusts_D_o.rds")
saveRDS(sdcs_D_o, "Data/sdcs_D_o.rds")
