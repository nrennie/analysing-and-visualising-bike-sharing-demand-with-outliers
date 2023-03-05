wrapper_function <- function(input_data,
                             input_mat,
                             times = 0:23,
                             perc = 0.01,
                             B = 10,
                             corr_threshold = 0.15,
                             weekday = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
                             weekend = c("Saturday", "Sunday"),
                             summer = c("April", "May", "June", "July", "August", "September", "October"),
                             winter = c("November", "December", "January", "February", "March")) {
  # clustering procedure
  clusters <- mst_clustering_threshold(input_mat, corr_threshold = corr_threshold)
  clustering <- clusters$cluster_list
  # split data into summer and winter
  day_data <- sapply(input_data, function(x) day_season_split(x, weekday = weekday, weekend = weekend, summer = summer, winter = winter))
  weekday_s_data <- day_data["weekday_s", ]
  weekend_s_data <- day_data["weekend_s", ]
  weekday_w_data <- day_data["weekday_w", ]
  weekend_w_data <- day_data["weekend_w", ]
  # run residuals on every list item
  weekday_s_residuals <- lapply(weekday_s_data, function(x) residuals_function(x, c(1, 1, 1)))
  weekend_s_residuals <- lapply(weekend_s_data, function(x) residuals_function(x, c(1, 1, 1)))
  weekday_w_residuals <- lapply(weekday_w_data, function(x) residuals_function(x, c(1, 1, 1)))
  weekend_w_residuals <- lapply(weekend_w_data, function(x) residuals_function(x, c(1, 1, 1)))
  # run outlier detection
  # run outlier detection
  weekday_s_depths_list <- list()
  for (i in 1:length(weekday_s_residuals)) {
    weekday_s_depths_list[[i]] <- depth(weekday_s_residuals[[i]], times = times, perc = perc, B = B)
  }
  names(weekday_s_depths_list) <- names(weekday_s_residuals)
  weekend_s_depths_list <- list()
  for (i in 1:length(weekend_s_residuals)) {
    weekend_s_depths_list[[i]] <- depth(weekend_s_residuals[[i]], times = times, perc = perc, B = B)
  }
  names(weekend_s_depths_list) <- names(weekend_s_residuals)
  weekday_w_depths_list <- list()
  for (i in 1:length(weekday_w_residuals)) {
    weekday_w_depths_list[[i]] <- depth(weekday_w_residuals[[i]], times = times, perc = perc, B = B)
  }
  names(weekday_w_depths_list) <- names(weekday_w_residuals)
  weekend_w_depths_list <- list()
  for (i in 1:length(weekend_w_residuals)) {
    weekend_w_depths_list[[i]] <- depth(weekend_w_residuals[[i]], times = times, perc = perc, B = B)
  }
  names(weekend_w_depths_list) <- names(weekend_w_residuals)
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
      alert_lists[[c]] <- beta4_probs(x)
    }
  }
  names(alert_lists) <- unlist(lapply(clustering, function(x) paste(x, collapse = " ", sep = ",")))
  return(list(alert_lists))
}
