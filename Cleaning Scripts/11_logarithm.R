# Source functions script -------------------------------------------------

source("Cleaning Scripts/00_packages.R")


# Read data ---------------------------------------------------------------

agg_station_matrix <- readRDS("Data/agg_station_matrix.rds")
adj_mat <- readRDS("Data/adj_mat2b.rds")
cor_mat <- readRDS("Data/agg_cor_mat.rds")


# Run on log data ---------------------------------------------------------

input_data <- agg_station_matrix
input_mat <- adj_mat * (1 - cor_mat)
log_agg_output <- wrapper_log_function(input_data, input_mat)


# Save log results --------------------------------------------------------

log_agg_results <- log_agg_output[[1]]
log_agg_residuals_sum <- log_agg_output[[2]]
log_agg_residuals_win <- log_agg_output[[3]]
saveRDS(log_agg_results, "Data/log_agg_results.rds")
saveRDS(log_agg_residuals_sum, "Data/log_agg_residuals_sum.rds")
saveRDS(log_agg_residuals_win, "Data/log_agg_residuals_win.rds")


# Process output ----------------------------------------------------------

my_dates <- seq.Date(as.Date("2017/1/1"), as.Date("2019/12/31"), "days")
log_agg_station_output <- data.frame(matrix(NA, ncol = (length(names(agg_station_matrix)) + 1), nrow = length(my_dates)))
log_agg_station_output[, 1] <- my_dates
colnames(log_agg_station_output) <- c("date", names(agg_station_matrix))
for (i in 1:length(log_agg_results)) {
  cluster_list <- log_agg_results[[i]]
  if (nrow(cluster_list) > 0) {
    for (j in 1:nrow(cluster_list)) {
      all_legs <- cluster_list[j, 3]
      select_date <- cluster_list[j, 1]
      leg_list <- trimws(unlist(strsplit(all_legs, ",")))
      for (k in 1:length(leg_list)) {
        if (!is.na(select_date) & !is.na(leg_list[k])) {
          log_agg_station_output[which(log_agg_station_output$date == select_date), leg_list[k]] <- cluster_list[j, 2]
        }
      }
    }
  }
}
saveRDS(log_agg_station_output, "Data/log_agg_station_output.rds")


# Get positive and negative outliers --------------------------------------

log_agg_station_pos_neg <- data.frame(matrix(NA, ncol = ncol(log_agg_station_output), nrow = nrow(log_agg_station_output)))
log_agg_station_pos_neg[, 1] <- my_dates
colnames(log_agg_station_pos_neg) <- colnames(log_agg_station_output)
for (i in 1:nrow(log_agg_station_pos_neg)) {
  for (j in 2:ncol(log_agg_station_pos_neg)) {
    print(c(i, j))
    if (!is.na(log_agg_station_output[i, j]) & log_agg_station_output[i, j] > 0) {
      # what is date
      date_s <- log_agg_station_pos_neg[i, 1]
      terminal <- names(log_agg_station_pos_neg)[j]
      # winter
      if (months(date_s) %in% winter) {
        d <- filter(log_agg_residuals_win[[which(names(log_agg_residuals_win) == terminal)]], date_of_day == date_s)
        sum_d <- sum(d[, 2:25])
        if (sum_d >= 0) {
          log_agg_station_pos_neg[i, j] <- 1
        }
        if (sum_d < 0) {
          log_agg_station_pos_neg[i, j] <- -1
        }
      }
      # winter
      if (months(date_s) %in% summer) {
        d <- filter(log_agg_residuals_sum[[which(names(log_agg_residuals_sum) == terminal)]], date_of_day == date_s)
        sum_d <- sum(d[, 2:25])
        if (sum_d >= 0) {
          log_agg_station_pos_neg[i, j] <- 1
        }
        if (sum_d < 0) {
          log_agg_station_pos_neg[i, j] <- -1
        }
      }
    }
  }
}
saveRDS(log_agg_station_pos_neg, "Data/log_agg_station_pos_neg.rds")
