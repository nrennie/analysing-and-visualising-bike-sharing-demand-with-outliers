matrix_function_start <- function(d1) {
  d1 <- mutate(d1, hour_of_day = hour(as.POSIXct(strptime(Start.date, "%Y-%m-%d %H:%M"))))
  d1 <- mutate(d1, date_of_day = date(as.POSIXct(strptime(Start.date, "%Y-%m-%d %H:%M"))))
  d1 <- mutate(d1, count = 1)
  d1_grouped <- d1 %>% group_by(date_of_day, hour_of_day)
  d1_grouped <- d1_grouped %>% summarise(count = sum(count))
  d1_wide <- d1_grouped %>% spread(key = hour_of_day, value = count, fill = 0)
  # add in blank columns for missing hours
  colnames(d1_wide)
  hours <- as.character(0:23)
  missing_hours <- hours[which(!(hours %in% colnames(d1_wide)))]
  if (length(missing_hours) > 0) {
    m_df <- data.frame(matrix(0, nrow = nrow(d1_wide), ncol = length(missing_hours)))
    colnames(m_df) <- missing_hours
    d1_wide <- cbind(d1_wide, m_df)
  }
  # sort columns
  d1_wide <- d1_wide[, c("date_of_day", 0:23)]
  return(d1_wide)
}

matrix_function_end <- function(d1) {
  d1 <- mutate(d1, hour_of_day = hour(as.POSIXct(strptime(End.date, "%Y-%m-%d %H:%M"))))
  d1 <- mutate(d1, date_of_day = date(as.POSIXct(strptime(End.date, "%Y-%m-%d %H:%M"))))
  d1 <- mutate(d1, count = 1)
  d1_grouped <- d1 %>% group_by(date_of_day, hour_of_day)
  d1_grouped <- d1_grouped %>% summarise(count = sum(count))
  d1_wide <- d1_grouped %>% spread(key = hour_of_day, value = count, fill = 0)
  # add in blank columns for missing hours
  colnames(d1_wide)
  hours <- as.character(0:23)
  missing_hours <- hours[which(!(hours %in% colnames(d1_wide)))]
  if (length(missing_hours) > 0) {
    m_df <- data.frame(matrix(0, nrow = nrow(d1_wide), ncol = length(missing_hours)))
    colnames(m_df) <- missing_hours
    d1_wide <- cbind(d1_wide, m_df)
  }
  # sort columns
  d1_wide <- d1_wide[, c("date_of_day", 0:23)]
  return(d1_wide)
}
