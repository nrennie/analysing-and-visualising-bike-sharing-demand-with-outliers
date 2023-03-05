# Source functions script -------------------------------------------------

source("Cleaning Scripts/00_packages.R")


# Read data ---------------------------------------------------------------

agg_station_matrix <- readRDS("Data/agg_station_matrix.rds")
agg_results_reg <- readRDS("Data/agg_results_reg.rds")
adj_mat <- readRDS("Data/adj_mat2b.rds")
cor_mat <- readRDS("Data/agg_cor_mat.rds")


# Paritition + regression --------------------------------------------------

input_data <- agg_station_matrix
input_mat <- adj_mat * (1 - cor_mat)
agg_station_output <- data.frame(matrix(NA, ncol = (length(names(agg_station_matrix)) + 1), nrow = length(my_dates)))
agg_station_output[, 1] <- my_dates
colnames(agg_station_output) <- c("date", names(agg_station_matrix))
for (i in 1:length(agg_results_reg)) {
  cluster_list <- agg_results_reg[[i]]
  if (nrow(cluster_list) > 0) {
    for (j in 1:nrow(cluster_list)) {
      all_legs <- cluster_list[j, 3]
      select_date <- cluster_list[j, 1]
      leg_list <- trimws(unlist(strsplit(all_legs, ",")))
      for (k in 1:length(leg_list)) {
        if (!is.na(select_date) & !is.na(leg_list[k])) {
          agg_station_output[which(agg_station_output$date == select_date), leg_list[k]] <- cluster_list[j, 2]
        }
      }
    }
  }
}
day_of_week <- weekdays(agg_station_output$date)
num_outs <- apply(agg_station_output[, 2:579], 1, function(x) sum(x > 0, na.rm = T))
plot_data1_reg <- tibble(day_of_week, num_outs)
plot_data1_reg <- plot_data1_reg %>% group_by(day_of_week)
plot_data1_reg <- plot_data1_reg %>% summarise(count = sum(num_outs))
plot_data1_reg$count <- plot_data1_reg$count / sum(plot_data1_reg$count)
plot_data1_reg$type <- rep("S/W/Wd/We Partition\n+ Regression", 7)
month_of_year <- months(agg_station_output$date)
num_outs <- apply(agg_station_output[, 2:579], 1, function(x) sum(x > 0, na.rm = T))
plot_data2_reg <- tibble(month_of_year, num_outs)
plot_data2_reg <- plot_data2_reg %>% group_by(month_of_year)
plot_data2_reg <- plot_data2_reg %>% summarise(count = sum(num_outs))
plot_data2_reg$count <- plot_data2_reg$count / sum(plot_data2_reg$count)
plot_data2_reg$type <- rep("S/W/Wd/We Partition\n+ Regression", 12)


# Partition + no regression ------------------------------------------------

wrapper_function_no_reg <- function(input_data,
                                    input_mat,
                                    times = 0:23,
                                    perc = 0.01,
                                    B = 10,
                                    corr_threshold = 0,
                                    weekday = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
                                    weekend = c("Saturday", "Sunday"),
                                    summer = c("April", "May", "June", "July", "August", "September", "October"),
                                    winter = c("November", "December", "January", "February", "March")) {
  # clustering procedure
  clusters <- mst_clustering_threshold(input_mat, corr_threshold = corr_threshold)
  clustering <- clusters$cluster_list
  # split data into summer and winter
  day_data <- sapply(input_data, function(x) {
    day_season_split(x,
      weekday = weekday,
      weekend = weekend,
      summer = summer,
      winter = winter
    )
  })
  weekday_s_data <- day_data["weekday_s", ]
  weekend_s_data <- day_data["weekend_s", ]
  weekday_w_data <- day_data["weekday_w", ]
  weekend_w_data <- day_data["weekend_w", ]
  # run outlier detection
  weekday_s_depths_list <- list()
  for (i in 1:length(weekday_s_data)) {
    weekday_s_depths_list[[i]] <- depth(weekday_s_data[[i]], times = times, perc = perc, B = B)
  }
  names(weekday_s_depths_list) <- names(weekday_s_data)
  weekend_s_depths_list <- list()
  for (i in 1:length(weekend_s_data)) {
    weekend_s_depths_list[[i]] <- depth(weekend_s_data[[i]], times = times, perc = perc, B = B)
  }
  names(weekend_s_depths_list) <- names(weekend_s_data)
  weekday_w_depths_list <- list()
  for (i in 1:length(weekday_w_data)) {
    weekday_w_depths_list[[i]] <- depth(weekday_w_data[[i]], times = times, perc = perc, B = B)
  }
  names(weekday_w_depths_list) <- names(weekday_w_data)
  weekend_w_depths_list <- list()
  for (i in 1:length(weekend_w_data)) {
    weekend_w_depths_list[[i]] <- depth(weekend_w_data[[i]], times = times, perc = perc, B = B)
  }
  names(weekend_w_depths_list) <- names(weekend_w_data)
  # join data back together
  depths_list <- mapply(c, weekday_s_depths_list, weekend_s_depths_list, weekday_w_depths_list, weekend_w_depths_list, SIMPLIFY = FALSE)
  # for loop for each cluster
  alert_lists <- list()
  for (c in 1:length(clustering)) {
    # extract elements in cluster c from input list
    cluster_c <- depths_list[which(names(depths_list) %in% clustering[[c]])]
    # if cluster empty, return an empty data frame
    if (length(cluster_c) == 1 & length(cluster_c[[1]]) == 0) {
      alert_lists[[c]] <- data.frame(Dep = character(), Prob = double(), Legs = character())
    } else {
      # merge the legs in the cluster
      x <- merge_differences(l = cluster_c)
      colnames(x) <- sapply(colnames(x), function(y) name_rem_x(y))
      # obtain the probability table
      alert_lists[[c]] <- gpd_probs(x)
    }
  }
  names(alert_lists) <- unlist(lapply(clustering, function(x) paste(x, collapse = " ", sep = ",")))
  return(list(alert_lists))
}
agg_output_no_reg <- wrapper_function_no_reg(input_data, input_mat)
agg_results_no_reg <- agg_output_no_reg[[1]]
saveRDS(agg_results_no_reg, "agg_results_no_reg.rds")
agg_station_output <- data.frame(matrix(NA, ncol = (length(names(agg_station_matrix)) + 1), nrow = length(my_dates)))
agg_station_output[, 1] <- my_dates
colnames(agg_station_output) <- c("date", names(agg_station_matrix))
for (i in 1:length(agg_results_no_reg)) {
  cluster_list <- agg_results_no_reg[[i]]
  if (nrow(cluster_list) > 0) {
    for (j in 1:nrow(cluster_list)) {
      all_legs <- cluster_list[j, 3]
      select_date <- cluster_list[j, 1]
      leg_list <- trimws(unlist(strsplit(all_legs, ",")))
      for (k in 1:length(leg_list)) {
        if (!is.na(select_date) & !is.na(leg_list[k])) {
          agg_station_output[which(agg_station_output$date == select_date), leg_list[k]] <- cluster_list[j, 2]
        }
      }
    }
  }
}
day_of_week <- weekdays(agg_station_output$date)
num_outs <- apply(agg_station_output[, 2:579], 1, function(x) sum(x > 0, na.rm = T))
plot_data1_no_reg <- tibble(day_of_week, num_outs)
plot_data1_no_reg <- plot_data1_no_reg %>% group_by(day_of_week)
plot_data1_no_reg <- plot_data1_no_reg %>% summarise(count = sum(num_outs))
plot_data1_no_reg$count <- plot_data1_no_reg$count / sum(plot_data1_no_reg$count)
plot_data1_no_reg$type <- rep("S/W/Wd/We Partition\n+ No Regressionn", 7)
month_of_year <- months(agg_station_output$date)
num_outs <- apply(agg_station_output[, 2:579], 1, function(x) sum(x > 0, na.rm = T))
plot_data2_no_reg <- tibble(month_of_year, num_outs)
plot_data2_no_reg <- plot_data2_no_reg %>% group_by(month_of_year)
plot_data2_no_reg <- plot_data2_no_reg %>% summarise(count = sum(num_outs))
plot_data2_no_reg$count <- plot_data2_no_reg$count / sum(plot_data2_no_reg$count)
plot_data2_no_reg$type <- rep("S/W/Wd/We Partition\n+ No Regression", 12)



# Regression + No partition ------------------------------------------------------------

wrapper_function_reg_np <- function(input_data,
                                    input_mat,
                                    times = 0:23,
                                    perc = 0.01,
                                    B = 10,
                                    corr_threshold = 0,
                                    summer = c("April", "May", "June", "July", "August", "September", "October"),
                                    winter = c("November", "December", "January", "February", "March")) {
  # clustering procedure
  clusters <- mst_clustering_threshold(input_mat, corr_threshold = corr_threshold)
  clustering <- clusters$cluster_list
  # run residuals on every list item
  residuals <- list()
  for (i in 1:length(input_data)) {
    residuals[[i]] <- residuals_function(input_data[[i]], c(1, 1, 1))
  }
  names(residuals) <- names(input_data)
  # residuals <- lapply(input_data, function(x) residuals_function(x, c(1,1,1)))
  # run outlier detection
  depths_list <- list()
  for (i in 1:length(residuals)) {
    depths_list[[i]] <- depth(residuals[[i]], times = times, perc = perc, B = B)
  }
  names(depths_list) <- names(residuals)
  # for loop for each cluster
  alert_lists <- list()
  for (c in 1:length(clustering)) {
    print(c)
    # extract elements in cluster c from input list
    cluster_c <- depths_list[which(names(depths_list) %in% clustering[[c]])]
    # if cluster empty, return an empty data frame
    if (length(cluster_c) == 1 & length(cluster_c[[1]]) == 0) {
      alert_lists[[c]] <- data.frame(Dep = character(), Prob = double(), Legs = character())
    } else {
      # merge the legs in the cluster
      x <- merge_differences(l = cluster_c)
      colnames(x) <- sapply(colnames(x), function(y) name_rem_x(y))
      # obtain the probability table
      alert_lists[[c]] <- gpd_probs(x)
    }
  }
  names(alert_lists) <- unlist(lapply(clustering, function(x) paste(x, collapse = " ", sep = ",")))
  return(list(alert_lists))
}
agg_output_reg_np <- wrapper_function_reg_np(input_data, input_mat)
agg_results_reg_np <- agg_output_reg_np[[1]]
saveRDS(agg_results_reg_np, "agg_results_reg_np.rds")
agg_station_output <- data.frame(matrix(NA, ncol = (length(names(agg_station_matrix)) + 1), nrow = length(my_dates)))
agg_station_output[, 1] <- my_dates
colnames(agg_station_output) <- c("date", names(agg_station_matrix))
for (i in 1:length(agg_results_reg_np)) {
  cluster_list <- agg_results_reg_np[[i]]
  if (nrow(cluster_list) > 0) {
    for (j in 1:nrow(cluster_list)) {
      all_legs <- cluster_list[j, 3]
      select_date <- cluster_list[j, 1]
      leg_list <- trimws(unlist(strsplit(all_legs, ",")))
      for (k in 1:length(leg_list)) {
        if (!is.na(select_date) & !is.na(leg_list[k])) {
          agg_station_output[which(agg_station_output$date == select_date), leg_list[k]] <- cluster_list[j, 2]
        }
      }
    }
  }
}

day_of_week <- weekdays(agg_station_output$date)
num_outs <- apply(agg_station_output[, 2:579], 1, function(x) sum(x > 0, na.rm = T))
plot_data1_reg_np <- tibble(day_of_week, num_outs)
plot_data1_reg_np <- plot_data1_reg_np %>% group_by(day_of_week)
plot_data1_reg_np <- plot_data1_reg_np %>% summarise(count = sum(num_outs))
plot_data1_reg_np$count <- plot_data1_reg_np$count / sum(plot_data1_reg_np$count)
plot_data1_reg_np$type <- rep("No Partition\n+ Regression", 7)
month_of_year <- months(agg_station_output$date)
num_outs <- apply(agg_station_output[, 2:579], 1, function(x) sum(x > 0, na.rm = T))
plot_data2_reg_np <- tibble(month_of_year, num_outs)
plot_data2_reg_np <- plot_data2_reg_np %>% group_by(month_of_year)
plot_data2_reg_np <- plot_data2_reg_np %>% summarise(count = sum(num_outs))
plot_data2_reg_np$count <- plot_data2_reg_np$count / sum(plot_data2_reg_np$count)
plot_data2_reg_np$type <- rep("No Partition\n+ Regression", 12)



# No regression + no partition --------------------------------------------

wrapper_function_no_reg_np <- function(input_data,
                                       input_mat,
                                       times = 0:23,
                                       perc = 0.01,
                                       B = 10,
                                       corr_threshold = 0,
                                       summer = c("April", "May", "June", "July", "August", "September", "October"),
                                       winter = c("November", "December", "January", "February", "March")) {
  # clustering procedure
  clusters <- mst_clustering_threshold(input_mat, corr_threshold = corr_threshold)
  clustering <- clusters$cluster_list
  # run residuals on every list item
  residuals <- list()
  for (i in 1:length(input_data)) {
    residuals[[i]] <- residuals_function(input_data[[i]], c(1, 1, 1))
  }
  names(residuals) <- names(input_data)
  # run outlier detection
  depths_list <- list()
  for (i in 1:length(residuals)) {
    depths_list[[i]] <- depth(residuals[[i]], times = times, perc = perc, B = B)
  }
  names(depths_list) <- names(residuals)
  # for loop for each cluster
  alert_lists <- list()
  for (c in 1:length(clustering)) {
    print(c)
    # extract elements in cluster c from input list
    cluster_c <- depths_list[which(names(depths_list) %in% clustering[[c]])]
    # if cluster empty, return an empty data frame
    if (length(cluster_c) == 1 & length(cluster_c[[1]]) == 0) {
      alert_lists[[c]] <- data.frame(Dep = character(), Prob = double(), Legs = character())
    } else {
      # merge the legs in the cluster
      x <- merge_differences(l = cluster_c)
      colnames(x) <- sapply(colnames(x), function(y) name_rem_x(y))
      # obtain the probability table
      alert_lists[[c]] <- gpd_probs(x)
    }
  }
  names(alert_lists) <- unlist(lapply(clustering, function(x) paste(x, collapse = " ", sep = ",")))
  return(list(alert_lists))
}
agg_output_no_reg_np <- wrapper_function_no_reg_np(input_data, input_mat)
agg_results_no_reg_np <- agg_output_no_reg_np[[1]]
saveRDS(agg_results_no_reg_np, "Data/agg_results_no_reg_np.rds")
agg_station_output <- data.frame(matrix(NA, ncol = (length(names(agg_station_matrix)) + 1), nrow = length(my_dates)))
agg_station_output[, 1] <- my_dates
colnames(agg_station_output) <- c("date", names(agg_station_matrix))
for (i in 1:length(agg_results_no_reg_np)) {
  cluster_list <- agg_results_no_reg_np[[i]]
  if (nrow(cluster_list) > 0) {
    for (j in 1:nrow(cluster_list)) {
      all_legs <- cluster_list[j, 3]
      select_date <- cluster_list[j, 1]
      leg_list <- trimws(unlist(strsplit(all_legs, ",")))
      for (k in 1:length(leg_list)) {
        if (!is.na(select_date) & !is.na(leg_list[k])) {
          agg_station_output[which(agg_station_output$date == select_date), leg_list[k]] <- cluster_list[j, 2]
        }
      }
    }
  }
}
day_of_week <- weekdays(agg_station_output$date)
num_outs <- apply(agg_station_output[, 2:579], 1, function(x) sum(x > 0, na.rm = T))
plot_data1_no_reg_np <- tibble(day_of_week, num_outs)
plot_data1_no_reg_np <- plot_data1_no_reg_np %>% group_by(day_of_week)
plot_data1_no_reg_np <- plot_data1_no_reg_np %>% summarise(count = sum(num_outs))
plot_data1_no_reg_np$count <- plot_data1_no_reg_np$count / sum(plot_data1_no_reg_np$count)
plot_data1_no_reg_np$type <- rep("No Partition\n+ No Regression", 7)
month_of_year <- months(agg_station_output$date)
num_outs <- apply(agg_station_output[, 2:579], 1, function(x) sum(x > 0, na.rm = T))
plot_data2_no_reg_np <- tibble(month_of_year, num_outs)
plot_data2_no_reg_np <- plot_data2_no_reg_np %>% group_by(month_of_year)
plot_data2_no_reg_np <- plot_data2_no_reg_np %>% summarise(count = sum(num_outs))
plot_data2_no_reg_np$count <- plot_data2_no_reg_np$count / sum(plot_data2_no_reg_np$count)
plot_data2_no_reg_np$type <- rep("No Partition\n+ No Regression", 12)


# Compare splits ---------------------------------------------------------

plot_data1 <- rbind(plot_data1_reg, plot_data1_no_reg, plot_data1_reg_np, plot_data1_no_reg_np)
plot_data1$day_of_week <- factor(plot_data1$day_of_week,
  levels = c(
    "Monday", "Tuesday", "Wednesday",
    "Thursday", "Friday", "Saturday", "Sunday"
  )
)
plot_data1$diff <- plot_data1$count - (1 / 7)
saveRDS(plot_data1, "Data/regression_weekday.rds")

plot_data2 <- rbind(plot_data2_reg, plot_data2_no_reg, plot_data2_reg_np, plot_data2_no_reg_np)
plot_data2$month_of_year <- factor(month.abb[match(plot_data2$month_of_year, month.name)],
  levels = c(
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
  )
)
plot_data2$diff <- plot_data2$count - (1 / 12)
saveRDS(plot_data2, "Data/regression_month.rds")
