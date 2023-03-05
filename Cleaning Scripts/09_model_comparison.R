# Source functions script -------------------------------------------------

source("Cleaning Scripts/00_packages.R")


# Read data ---------------------------------------------------------------

agg_station_matrix <- readRDS("Data/agg_station_matrix.rds")


# Select data -------------------------------------------------------------

d1 <- agg_station_matrix[[233]]
d2 <- agg_station_matrix[[238]]
d3 <- agg_station_matrix[[239]]
d4 <- agg_station_matrix[[244]]
d5 <- agg_station_matrix[[245]]
d6 <- agg_station_matrix[[246]]
d7 <- agg_station_matrix[[248]]
d8 <- agg_station_matrix[[442]]
d9 <- agg_station_matrix[[467]]


# Possible models ---------------------------------------------------------

model_matrix <- matrix(c(0,0,0,
                         1,0,0,
                         0,1,0,
                         0,0,1,
                         1,1,0,
                         0,1,1,
                         1,0,1,
                         1,1,1), ncol=3, byrow=T)


# Apply to data -----------------------------------------------------------

terminal1_sse <- apply(model_matrix, 1, function(x) sse_function(d1, x))
terminal2_sse <- apply(model_matrix, 1, function(x) sse_function(d2, x))
terminal3_sse <- apply(model_matrix, 1, function(x) sse_function(d3, x))
terminal4_sse <- apply(model_matrix, 1, function(x) sse_function(d4, x))
terminal5_sse <- apply(model_matrix, 1, function(x) sse_function(d5, x))
terminal6_sse <- apply(model_matrix, 1, function(x) sse_function(d6, x))
terminal7_sse <- apply(model_matrix, 1, function(x) sse_function(d7, x))
terminal8_sse <- apply(model_matrix, 1, function(x) sse_function(d8, x))
terminal9_sse <- apply(model_matrix, 1, function(x) sse_function(d9, x))

