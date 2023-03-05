aggregate_start_end <- function(start, end) {
  start_names <- colnames(start)
  # add start dates into end dates
  missing_end <- which(start$date_of_day %notin% end$date_of_day)
  if (length(missing_end) > 0) {
    for (i in 1:length(missing_end)) {
      k <- data.frame(list(
        as.Date(start$date_of_day[missing_end[i]]),
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
      ))
      names(k) <- start_names
      end <- rbind(end, k)
    }
  }
  end <- end[order(end$date_of_day), ]
  # add end dates into start dates
  missing_start <- which(end$date_of_day %notin% start$date_of_day)
  if (length(missing_start) > 0) {
    for (i in 1:length(missing_start)) {
      k <- data.frame(list(
        as.Date(end$date_of_day[missing_start[i]]),
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
      ))
      names(k) <- start_names
      start <- rbind(start, k)
    }
  }
  start <- start[order(start$date_of_day), ]
  # add by date
  agg <- start[, 2:25] + end[, 2:25]
  agg_df <- tibble(date_of_day = start$date_of_day, agg)
  return(agg_df)
}
