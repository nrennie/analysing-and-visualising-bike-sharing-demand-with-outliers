# Source functions script -------------------------------------------------

source("Cleaning Scripts/00_packages.R")


# Read data ---------------------------------------------------------------

start_station_matrix <- readRDS("Data/start_station_matrix.rds")
end_station_matrix <- readRDS("Data/end_station_matrix.rds")


# Aggregate data ----------------------------------------------------------

end_station_matrix$`31718` <- tibble()
end_station_matrix <- end_station_matrix[order(names(end_station_matrix))]
num <- length(start_station_matrix)
agg_station_matrix <- list()
for (i in 1:num) {
  print(i)
  agg_station_matrix[[i]] <- aggregate_start_end(
    start_station_matrix[[i]],
    end_station_matrix[[i]]
  )
}
names(agg_station_matrix) <- names(start_station_matrix)
saveRDS(agg_station_matrix, "Data/agg_station_matrix.rds")


# Calculate correlations --------------------------------------------------

agg_cor_mat <- correlation_matrix_function(station_data$TERMINAL_NUMBER, input_matrix = agg_station_matrix)
saveRDS(agg_cor_mat, "Data/agg_cor_mat.rds")

all_cor_mat <- correlation_matrix_function(station_data$TERMINAL_NUMBER, input_matrix = start_station_matrix) 
saveRDS(all_cor_mat, "Data/cor_mat.rds")

cor_mat_end <- correlation_matrix_function(station_data$TERMINAL_NUMBER, input_matrix = end_station_matrix) 
saveRDS(cor_mat_end, "Data/cor_mat_end.rds")