# Source functions script -------------------------------------------------

source("Cleaning Scripts/00_packages.R")


# Read data ---------------------------------------------------------------

start_station_output <- readRDS("Data/start_station_output.rds")
end_station_output <- readRDS("Data/end_station_output.rds")
agg_station_output <- readRDS("Data/agg_station_output.rds")

# Calculate dates ---------------------------------------------------------

my_dates <- seq.Date(as.Date("2017/1/1"), as.Date("2019/12/31"), "days")
summer <- c("April", "May", "June", "July", "August", "September", "October")
winter <- c("November", "December", "January", "February", "March")


# Start stations ----------------------------------------------------------

start_station_pos_neg <- data.frame(matrix(NA, ncol=ncol(start_station_output),
                                           nrow=nrow(start_station_output)))
start_station_pos_neg[,1] <- my_dates
colnames(start_station_pos_neg) <- colnames(start_station_output)
for (i in 1:nrow(start_station_pos_neg)){
  for (j in 2:ncol(start_station_pos_neg)){
    print(c(i,j))
    if (!is.na(start_station_output[i,j]) & start_station_output[i,j] > 0){
      #what is date
      date_s <- start_station_pos_neg[i,1]
      terminal <- names(start_station_pos_neg)[j]
      #winter
      if (months(date_s) %in% winter){
        d <- filter(start_residuals_win[[which(names(start_residuals_win) == terminal)]], date_of_day == date_s)
        sum_d <- sum(d[,2:25])
        if (sum_d >= 0){
          start_station_pos_neg[i,j] <- 1
        } 
        if (sum_d < 0){
          start_station_pos_neg[i,j] <- -1
        }
      }
      #winter
      if (months(date_s) %in% summer){
        d <- filter(start_residuals_sum[[which(names(start_residuals_sum) == terminal)]], date_of_day == date_s)
        sum_d <- sum(d[,2:25])
        if (sum_d >= 0){
          start_station_pos_neg[i,j] <- 1
        } 
        if (sum_d < 0){
          start_station_pos_neg[i,j] <- -1
        }
      }
    }
  }
}
saveRDS(start_station_pos_neg, "Data/start_station_pos_neg.rds")


# End stations ------------------------------------------------------------

end_station_pos_neg <- data.frame(matrix(NA, ncol=ncol(end_station_output), nrow=nrow(end_station_output)))
end_station_pos_neg[,1] <- my_dates
colnames(end_station_pos_neg) <- colnames(end_station_output)
for (i in 1:nrow(end_station_pos_neg)){
  for (j in 2:ncol(end_station_pos_neg)){
    print(c(i,j))
    if (!is.na(end_station_output[i,j]) & end_station_output[i,j] > 0){
      #what is date
      date_s <- end_station_pos_neg[i,1]
      terminal <- names(end_station_pos_neg)[j]
      #winter
      if (months(date_s) %in% winter){
        d <- filter(end_residuals_win[[which(names(end_residuals_win) == terminal)]], date_of_day == date_s)
        sum_d <- sum(d[,2:25])
        if (sum_d >= 0){
          end_station_pos_neg[i,j] <- 1
        } 
        if (sum_d < 0){
          end_station_pos_neg[i,j] <- -1
        }
      }
      #winter
      if (months(date_s) %in% summer){
        d <- filter(end_residuals_sum[[which(names(end_residuals_sum) == terminal)]], date_of_day == date_s)
        sum_d <- sum(d[,2:25])
        if (sum_d >= 0){
          end_station_pos_neg[i,j] <- 1
        } 
        if (sum_d < 0){
          end_station_pos_neg[i,j] <- -1
        }
      }
    }
  }
}
saveRDS(end_station_pos_neg, "Data/end_station_pos_neg.rds")


# Aggregated stations -----------------------------------------------------

agg_station_pos_neg <- data.frame(matrix(NA, ncol=ncol(agg_station_output), nrow=nrow(agg_station_output)))
agg_station_pos_neg[,1] <- my_dates
colnames(agg_station_pos_neg) <- colnames(agg_station_output)
for (i in 1:nrow(agg_station_pos_neg)){
  for (j in 2:ncol(agg_station_pos_neg)){
    print(c(i,j))
    if (!is.na(agg_station_output[i,j]) & agg_station_output[i,j] > 0){
      #what is date
      date_s <- agg_station_pos_neg[i,1]
      terminal <- names(agg_station_pos_neg)[j]
      #winter
      if (months(date_s) %in% winter){
        d <- filter(agg_residuals_win[[which(names(agg_residuals_win) == terminal)]], date_of_day == date_s)
        sum_d <- sum(d[,2:25])
        if (sum_d >= 0){
          agg_station_pos_neg[i,j] <- 1
        } 
        if (sum_d < 0){
          agg_station_pos_neg[i,j] <- -1
        }
      }
      #winter
      if (months(date_s) %in% summer){
        d <- filter(agg_residuals_sum[[which(names(agg_residuals_sum) == terminal)]], date_of_day == date_s)
        sum_d <- sum(d[,2:25])
        if (sum_d >= 0){
          agg_station_pos_neg[i,j] <- 1
        } 
        if (sum_d < 0){
          agg_station_pos_neg[i,j] <- -1
        }
      }
    }
  }
}
saveRDS(agg_station_pos_neg, "Data/agg_station_pos_neg.rds")
