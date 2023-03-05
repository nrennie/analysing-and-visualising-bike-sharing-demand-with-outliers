# Source functions script -------------------------------------------------

source("Cleaning Scripts/00_packages.R")


# Read data ---------------------------------------------------------------

adj_mat <- readRDS("Data/adj_mat2b.rds")
cor_mat <- readRDS("Data/agg_cor_mat.rds")
station_data <- readRDS("Data/station_data.rds")
start_station_matrix <- readRDS("Data/start_station_matrix.rds")
agg_station_matrix <- readRDS("Data/agg_station_matrix.rds")
agg_cluster_output_beta <- readRDS("Data/agg_cluster_output_beta.rds")


# Calculate start/end clusters --------------------------------------------

# rho = 0 --------------------------------------------------------------

input_data <- agg_station_matrix
input_mat <- adj_mat * (1 - cor_mat)
my_dates <- seq.Date(as.Date("2017/1/1"), as.Date("2019/12/31"), "days")
c <- spatial.median(station_data[, c(3, 2)])
input_data <- start_station_matrix
input_mat <- adj_mat * (1 - cor_mat)
start_output_beta <- wrapper_function(input_data, input_mat, corr_threshold = 0)
start_results_beta <- start_output_beta[[1]]
start_cluster_output <- data.frame(matrix(NA, ncol = (length(start_results_beta) + 1),
                                          nrow = length(my_dates)))
start_cluster_output[, 1] <- my_dates
colnames(start_cluster_output) <- c("date", names(start_results_beta))
cluster_names <- names(start_results_beta)
for (i in 1:length(start_results_beta)) {
  cluster_list <- start_results_beta[[i]]
  if (nrow(cluster_list) > 0) {
    for (j in 1:nrow(cluster_list)) {
      select_date <- cluster_list[j, 1]
      if (!is.na(select_date)) {
        start_cluster_output[which(start_cluster_output$date == select_date), 
                             i + 1] <- cluster_list[j, 2]
      }
    }
  }
}
start_cluster_output_beta <- start_cluster_output
clusters <- mst_clustering_threshold(input_mat, corr_threshold = 0)
clustering <- clusters$cluster_list
cluster_num <- 1:length(clustering)
dist <- numeric(length(clustering))
for (i in 1:length(dist)) {
  if (length(clustering[[i]]) == 1) {
    dist[i] <- c_dist_centre(clustering[[i]], c)
  } else {
    d <- filter(station_data, TERMINAL_NUMBER %in% clustering[[i]])
    m <- spatial.median(d[, c(3, 2)])
    dist[i] <- distm(c(c[1], c[2]), c(m[1], m[2]), fun = distHaversine)
  }
}
cluster_hm <- data.frame(cluster_num, dist)
order(dist)
agg_mat_cos000 <- agg_cluster_output_beta[, -1][, order(dist)]
start_mat_cos000 <- start_cluster_output_beta[, -1][, order(dist)]
saveRDS(agg_mat_cos000, "Data/agg_mat_cos000.rds")
saveRDS(start_mat_cos000, "Data/start_mat_cos000.rds")

# rho = 0.15 --------------------------------------------------------------

input_data <- agg_station_matrix
input_mat <- adj_mat * (1 - cor_mat)
agg_output_beta <- wrapper_function(input_data, input_mat, corr_threshold = 0.15)
agg_results_beta <- agg_output_beta[[1]]
agg_cluster_output <- data.frame(matrix(NA, ncol = (length(agg_results_beta) + 1),
                                        nrow = length(my_dates)))
agg_cluster_output[, 1] <- my_dates
colnames(agg_cluster_output) <- c("date", names(agg_results_beta))
cluster_names <- names(agg_results_beta)
for (i in 1:length(agg_results_beta)) {
  cluster_list <- agg_results_beta[[i]]
  if (nrow(cluster_list) > 0) {
    for (j in 1:nrow(cluster_list)) {
      select_date <- cluster_list[j, 1]
      if (!is.na(select_date)) {
        agg_cluster_output[which(agg_cluster_output$date == select_date),
                           i + 1] <- cluster_list[j, 2]
      }
    }
  }
}
agg_cluster_output_beta <- agg_cluster_output
input_data <- start_station_matrix
input_mat <- adj_mat * (1 - cor_mat)
start_output_beta <- wrapper_function(input_data, input_mat, corr_threshold = 0.15)
start_results_beta <- start_output_beta[[1]]
start_cluster_output <- data.frame(matrix(NA, ncol = (length(start_results_beta) + 1),
                                          nrow = length(my_dates)))
start_cluster_output[, 1] <- my_dates
colnames(start_cluster_output) <- c("date", names(start_results_beta))
cluster_names <- names(start_results_beta)
for (i in 1:length(start_results_beta)) {
  cluster_list <- start_results_beta[[i]]
  if (nrow(cluster_list) > 0) {
    for (j in 1:nrow(cluster_list)) {
      select_date <- cluster_list[j, 1]
      if (!is.na(select_date)) {
        start_cluster_output[which(start_cluster_output$date == select_date), 
                             i + 1] <- cluster_list[j, 2]
      }
    }
  }
}
start_cluster_output_beta <- start_cluster_output
clusters <- mst_clustering_threshold(input_mat, corr_threshold = 0.15)
clustering <- clusters$cluster_list
cluster_num <- 1:length(clustering)
dist <- numeric(length(clustering))
for (i in 1:length(dist)) {
  if (length(clustering[[i]]) == 1) {
    dist[i] <- c_dist_centre(clustering[[i]], c)
  } else {
    d <- filter(station_data, TERMINAL_NUMBER %in% clustering[[i]])
    m <- spatial.median(d[, c(3, 2)])
    dist[i] <- distm(c(c[1], c[2]), c(m[1], m[2]), fun = distHaversine)
  }
}
cluster_hm <- data.frame(cluster_num, dist)
agg_mat_cos015 <- agg_cluster_output_beta[, -1][, order(dist)]
start_mat_cos015 <- start_cluster_output_beta[, -1][, order(dist)]
saveRDS(agg_mat_cos015, "Data/agg_mat_cos015.rds")
saveRDS(start_mat_cos015, "Data/start_mat_cos015.rds")

# rho = 0.3 ---------------------------------------------------------------

input_data <- agg_station_matrix
input_mat <- adj_mat * (1 - cor_mat)
agg_output_beta <- wrapper_function(input_data, input_mat, corr_threshold = 0.3)
agg_results_beta <- agg_output_beta[[1]]
agg_cluster_output <- data.frame(matrix(NA, ncol = (length(agg_results_beta) + 1),
                                        nrow = length(my_dates)))
agg_cluster_output[, 1] <- my_dates
colnames(agg_cluster_output) <- c("date", names(agg_results_beta))
cluster_names <- names(agg_results_beta)
for (i in 1:length(agg_results_beta)) {
  cluster_list <- agg_results_beta[[i]]
  if (nrow(cluster_list) > 0) {
    for (j in 1:nrow(cluster_list)) {
      select_date <- cluster_list[j, 1]
      if (!is.na(select_date)) {
        agg_cluster_output[which(agg_cluster_output$date == select_date),
                           i + 1] <- cluster_list[j, 2]
      }
    }
  }
}
agg_cluster_output_beta <- agg_cluster_output
input_data <- start_station_matrix
input_mat <- adj_mat * (1 - cor_mat)
start_output_beta <- wrapper_function(input_data, input_mat, corr_threshold = 0.3)
start_results_beta <- start_output_beta[[1]]
start_cluster_output <- data.frame(matrix(NA, ncol = (length(start_results_beta) + 1),
                                          nrow = length(my_dates)))
start_cluster_output[, 1] <- my_dates
colnames(start_cluster_output) <- c("date", names(start_results_beta))
cluster_names <- names(start_results_beta)
for (i in 1:length(start_results_beta)) {
  cluster_list <- start_results_beta[[i]]
  if (nrow(cluster_list) > 0) {
    for (j in 1:nrow(cluster_list)) {
      select_date <- cluster_list[j, 1]
      if (!is.na(select_date)) {
        start_cluster_output[which(start_cluster_output$date == select_date), 
                             i + 1] <- cluster_list[j, 2]
      }
    }
  }
}
start_cluster_output_beta <- start_cluster_output
clusters <- mst_clustering_threshold(input_mat, corr_threshold = 0.3)
clustering <- clusters$cluster_list
cluster_num <- 1:length(clustering)
dist <- numeric(length(clustering))
for (i in 1:length(dist)) {
  if (length(clustering[[i]]) == 1) {
    dist[i] <- c_dist_centre(clustering[[i]], c)
  } else {
    d <- filter(station_data, TERMINAL_NUMBER %in% clustering[[i]])
    m <- spatial.median(d[, c(3, 2)])
    dist[i] <- distm(c(c[1], c[2]), c(m[1], m[2]), fun = distHaversine)
  }
}
cluster_hm <- data.frame(cluster_num, dist)
agg_mat_cos030 <- agg_cluster_output_beta[, -1][, order(dist)]
start_mat_cos030 <- start_cluster_output_beta[, -1][, order(dist)]
saveRDS(agg_mat_cos030, "Data/agg_mat_cos030.rds")
saveRDS(start_mat_cos030, "Data/start_mat_cos030.rds")


# Calculate similarity between start and end ------------------------------

start_end_cos000 <- numeric(length = 78)
for (i in 1:78) {
  start_end_cos000[i] <- cosine(start_mat_cos000[, i], agg_mat_cos000[, i])
}
saveRDS(start_end_cos000, "Data/start_end_cos000.rds")

start_end_cos015 <- numeric(length = 195)
for (i in 1:195) {
  start_end_cos015[i] <- cosine(start_mat_cos015[, i], agg_mat_cos015[, i])
}
saveRDS(start_end_cos015, "Data/start_end_cos015.rds")

start_end_cos030 <- numeric(length = 373)
for (i in 1:373) {
  start_end_cos030[i] <- cosine(start_mat_cos030[, i], agg_mat_cos030[, i])
}
saveRDS(start_end_cos030, "Data/start_end_cos030.rds")

