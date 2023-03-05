correlation_matrix_function <- function(station_names, input_matrix, times = 1:24) {
  output <- matrix(NA, ncol = length(station_names), nrow = length(station_names))
  colnames(output) <- station_names
  rownames(output) <- station_names
  for (i in 1:length(station_names)) {
    # load ith data
    d1 <- input_matrix[[i]]
    for (j in 1:length(station_names)) {
      # load jth data
      d2 <- input_matrix[[j]]
      if (i > j) {
        # get common dates
        common_dates <- intersect(d1$date_of_day, d2$date_of_day)
        if (length(common_dates) > 1) {
          fcor <- mean(DynCorr(as.matrix(d1[which(d1$date_of_day %in% common_dates), 2:25]),
                               as.matrix(d2[which(d2$date_of_day %in% common_dates), 2:25]),
                               t = times))
          output[i, j] <- fcor
          output[j, i] <- fcor
        } else {
          output[i, j] <- 0
          output[j, i] <- 0
        }
      }
    }
  }
  return(output)
}
