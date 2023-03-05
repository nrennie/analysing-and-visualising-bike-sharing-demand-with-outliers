# Source functions script -------------------------------------------------

source("Cleaning Scripts/00_packages.R")


# Read data ---------------------------------------------------------------

start_station_matrix <- readRDS("Data/start_station_matrix.rds")
end_station_matrix <- readRDS("Data/end_station_matrix.rds")
agg_station_matrix <- readRDS("Data/agg_station_matrix.rds")
all_cor_mat <- readRDS("Data/cor_mat.rds")
cor_mat <- readRDS("Data/agg_cor_mat.rds")
cor_mat_end <- readRDS("Data/cor_mat_end.rds")
adj_mat <- readRDS("Data/adj_mat2b.rds")

# Set dates ---------------------------------------------------------------

my_dates <- seq.Date(as.Date("2017/1/1"), as.Date("2019/12/31"), "days")


# Run wrapper function ----------------------------------------------------

start_results <- wrapper_function(start_station_matrix, adj_mat * (1 - all_cor_mat))
end_results <- wrapper_function(end_station_matrix, adj_mat * (1 - cor_mat_end))
agg_results_beta <- wrapper_function(agg_station_matrix, adj_mat * (1 - agg_cor_mat))
agg_output_reg <- wrapper_function(agg_station_matrix, adj_mat * (1 - agg_cor_mat), corr_threshold = 0.15)

saveRDS(start_results[[1]], "Data/start_results.rds")
saveRDS(end_results[[1]], "Data/end_results.rds")
saveRDS(agg_results_beta[[1]], "Data/agg_results_beta.rds")
saveRDS(agg_results_reg[[1]], "agg_results_reg.rds")


# Process outputs for stations --------------------------------------------

start_station_output <- data.frame(matrix(NA,
  ncol = (length(names(start_station_matrix)) + 1),
  nrow = length(my_dates)
))
start_station_output[, 1] <- my_dates
colnames(start_station_output) <- c("date", names(start_station_matrix))
for (i in 1:length(start_results)) {
  cluster_list <- start_results[[i]]
  if (nrow(cluster_list) > 0) {
    for (j in 1:nrow(cluster_list)) {
      all_legs <- cluster_list[j, 3]
      select_date <- cluster_list[j, 1]
      leg_list <- trimws(unlist(strsplit(all_legs, ",")))
      for (k in 1:length(leg_list)) {
        if (!is.na(select_date) & !is.na(leg_list[k])) {
          start_station_output[
            which(start_station_output$date == select_date),
            leg_list[k]
          ] <- cluster_list[j, 2]
        }
      }
    }
  }
}
saveRDS(start_station_output, "Data/start_station_output.rds")

end_station_output <- data.frame(matrix(NA,
  ncol = (length(names(end_station_matrix)) + 1),
  nrow = length(my_dates)
))
end_station_output[, 1] <- my_dates
colnames(end_station_output) <- c("date", names(end_station_matrix))
for (i in 1:length(end_results)) {
  cluster_list <- end_results[[i]]
  if (nrow(cluster_list) > 0) {
    for (j in 1:nrow(cluster_list)) {
      all_legs <- cluster_list[j, 3]
      select_date <- cluster_list[j, 1]
      leg_list <- trimws(unlist(strsplit(all_legs, ",")))
      for (k in 1:length(leg_list)) {
        if (!is.na(select_date) & !is.na(leg_list[k])) {
          end_station_output[
            which(end_station_output$date == select_date),
            leg_list[k]
          ] <- cluster_list[j, 2]
        }
      }
    }
  }
}
saveRDS(end_station_output, "Data/end_station_output.rds")

agg_station_output_beta <- data.frame(matrix(NA,
  ncol = (length(names(agg_station_matrix)) + 1),
  nrow = length(my_dates)
))
agg_station_output_beta[, 1] <- my_dates
colnames(agg_station_output_beta) <- c("date", names(agg_station_matrix))
for (i in 1:length(agg_results)) {
  cluster_list <- agg_results[[i]]
  if (nrow(cluster_list) > 0) {
    for (j in 1:nrow(cluster_list)) {
      all_legs <- cluster_list[j, 3]
      select_date <- cluster_list[j, 1]
      leg_list <- trimws(unlist(strsplit(all_legs, ",")))
      for (k in 1:length(leg_list)) {
        if (!is.na(select_date) & !is.na(leg_list[k])) {
          agg_station_output_beta[
            which(agg_station_output_beta$date == select_date),
            leg_list[k]
          ] <- cluster_list[j, 2]
        }
      }
    }
  }
}
saveRDS(agg_station_output_beta, "Data/agg_station_output_beta.rds")

# Process outputs for clusters --------------------------------------------

start_cluster_output <- data.frame(matrix(NA,
  ncol = (length(names(start_station_matrix)) + 1),
  nrow = length(my_dates)
))
start_cluster_output[, 1] <- my_dates
colnames(start_cluster_output) <- c("date", names(start_station_matrix))
cluster_names <- names(start_results)
for (i in 1:length(start_results)) {
  cluster_list <- start_results[[i]]
  cluster_legs <- trimws(unlist(strsplit(cluster_names[i], " ")))
  if (nrow(cluster_list) > 0) {
    for (j in 1:nrow(cluster_list)) {
      select_date <- cluster_list[j, 1]
      for (k in 1:length(cluster_legs)) {
        if (!is.na(select_date) & !is.na(cluster_legs[k])) {
          start_cluster_output[
            which(start_cluster_output$date == select_date),
            cluster_legs[k]
          ] <- cluster_list[j, 2]
        }
      }
    }
  }
}
saveRDS(start_cluster_output, "Data/start_cluster_output.rds")

end_cluster_output <- data.frame(matrix(NA,
  ncol = (length(names(end_station_matrix)) + 1),
  nrow = length(my_dates)
))
end_cluster_output[, 1] <- my_dates
colnames(end_cluster_output) <- c("date", names(end_station_matrix))
cluster_names <- names(end_results)
for (i in 1:length(end_results)) {
  cluster_list <- end_results[[i]]
  cluster_legs <- trimws(unlist(strsplit(cluster_names[i], " ")))
  if (nrow(cluster_list) > 0) {
    for (j in 1:nrow(cluster_list)) {
      select_date <- cluster_list[j, 1]
      for (k in 1:length(cluster_legs)) {
        if (!is.na(select_date) & !is.na(cluster_legs[k])) {
          end_cluster_output[
            which(end_cluster_output$date == select_date),
            cluster_legs[k]
          ] <- cluster_list[j, 2]
        }
      }
    }
  }
}
saveRDS(end_cluster_output, "Data/end_cluster_output.rds")

agg_cluster_output <- data.frame(matrix(NA,
  ncol = (length(agg_results_beta) + 1),
  nrow = length(my_dates)
))
agg_cluster_output[, 1] <- my_dates
colnames(agg_cluster_output) <- c("date", names(agg_results_beta))
cluster_names <- names(agg_results_beta)
for (i in 1:length(agg_results_beta)) {
  cluster_list <- agg_results_beta[[i]]
  if (nrow(cluster_list) > 0) {
    for (j in 1:nrow(cluster_list)) {
      select_date <- cluster_list[j, 1]
      if (!is.na(select_date)) {
        agg_cluster_output[which(agg_cluster_output$date == select_date), i + 1] <- cluster_list[j, 2]
      }
    }
  }
}
agg_cluster_output_beta_hm <- agg_cluster_output
saveRDS(agg_cluster_output_beta_hm, "Data/agg_cluster_output_beta_hm.rds")
