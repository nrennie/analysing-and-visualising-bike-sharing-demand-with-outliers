# Source functions script -------------------------------------------------

source("Cleaning Scripts/00_packages.R")


# Read data ---------------------------------------------------------------

rain_matrix <- readRDS("Weather Data/rain_matrix.rds")
temp_matrix <- readRDS("Weather Data/temp_matrix.rds")
agg_station_output <- readRDS("Data/agg_station_output_beta.rds")
agg_station_pos_neg <- readRDS("Data/agg_station_pos_neg.rds")


# Rain data ---------------------------------------------------------------

agg_neg_outliers <- (agg_station_pos_neg[, 2:579] == -1) * (agg_station_output[, 2:579])
agg_neg_outliers[is.na(agg_neg_outliers)] <- 0
agg_max_prob <- apply(agg_neg_outliers, 1, function(x) max(x, na.rm = T))
rainfall_day <- rowSums(rain_matrix[, 2:25], na.rm = T)
rain_prob_data <- data.frame(dates = agg_station_output[, 1], agg_max_prob, rainfall_day)
rain_seq <- seq(0.25, 4.5, 0.25)
prob_seq <- seq(0.2, 1, 0.2)
rain_prob_matrix <- matrix(NA, nrow = length(prob_seq), ncol = length(rain_seq))
for (i in 1:(length(prob_seq))) {
  for (j in 1:length(rain_seq)) {
    rainy_days <- which(rain_prob_data$rainfall_day >= rain_seq[j])
    if (i == 1) {
      k <- (sum(rain_prob_data$agg_max_prob[rainy_days] < prob_seq[i] & rain_prob_data$agg_max_prob[rainy_days] > 0)) / length(rainy_days)
    } else {
      k <- (sum(rain_prob_data$agg_max_prob[rainy_days] > prob_seq[i - 1] & rain_prob_data$agg_max_prob[rainy_days] <= prob_seq[i])) / length(rainy_days)
    }
    rain_prob_matrix[i, j] <- k
  }
}
colnames(rain_prob_matrix) <- rain_seq
rownames(rain_prob_matrix) <- prob_seq[1:(length(prob_seq))]
rain_prob_plot <- as.data.frame(rain_prob_matrix)
rain_prob_plot$max_prob <- prob_seq[1:(length(prob_seq))]
rain_prob_plot <- pivot_longer(rain_prob_plot,
                               cols = 1:(ncol(rain_prob_plot) - 1),
                               names_to = "rain",
                               values_to = "values"
)
saveRDS(rain_prob_plot, "Data/rain_prob_plot.rds")


# Temp data ---------------------------------------------------------------

agg_station_output[is.na(agg_station_output)] <- 0
agg_max_prob <- apply(agg_station_output[, 2:579], 1, function(x) max(x, na.rm = T))
temp_day <- rowMeans(temp_matrix[, 2:25], na.rm = T)
temp_prob_data <- data.frame(dates = agg_station_output[, 1], agg_max_prob, temp_day)
temp_seq <- seq(25, 90, 5)
prob_seq <- seq(0.2, 1, 0.2)
temp_prob_matrix <- matrix(NA, nrow = length(prob_seq), ncol = length(temp_seq))
for (j in 1:length(temp_seq)) {
  if (j == 1) {
    temp_days <- which(temp_prob_data$temp_day <= temp_seq[j])
  } else {
    temp_days <- which(temp_prob_data$temp_day > temp_seq[j - 1] & temp_prob_data$temp_day <= temp_seq[j])
  }
  for (i in 1:(length(prob_seq))) {
    if (i == 1) {
      k <- (sum(temp_prob_data$agg_max_prob[temp_days] <= prob_seq[i])) / length(temp_days)
    } else {
      k <- (sum(temp_prob_data$agg_max_prob[temp_days] > prob_seq[i - 1] & temp_prob_data$agg_max_prob[temp_days] <= prob_seq[i])) / length(temp_days)
    }
    temp_prob_matrix[i, j] <- k
  }
}
colnames(temp_prob_matrix) <- temp_seq[1:(length(temp_seq))]
rownames(temp_prob_matrix) <- prob_seq[1:(length(prob_seq))]
temp_prob_plot <- as.data.frame(temp_prob_matrix)
temp_prob_plot$max_prob <- prob_seq[1:(length(prob_seq))]
temp_prob_plot <- pivot_longer(temp_prob_plot,
                               cols = 1:(ncol(temp_prob_plot) - 1),
                               names_to = "temp",
                               values_to = "values"
)
saveRDS(temp_prob_plot, "Data/temp_prob_plot.rds")

