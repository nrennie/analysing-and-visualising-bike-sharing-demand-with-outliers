# Download data -----------------------------------------------------------

# Data from https://s3.amazonaws.com/capitalbikeshare-data/index.html
# Save in a directory called `Raw Data`


# Load Data ---------------------------------------------------------------
d_2017_Q1 <- read.csv("Raw Data/2017Q1-capitalbikeshare-tripdata.csv")
d_2017_Q2 <- read.csv("Raw Data/2017Q2-capitalbikeshare-tripdata.csv")
d_2017_Q3 <- read.csv("Raw Data/2017Q3-capitalbikeshare-tripdata.csv")
d_2017_Q4 <- read.csv("Raw Data/2017Q4-capitalbikeshare-tripdata.csv")
d_2018_01 <- read.csv("Raw Data/201801-capitalbikeshare-tripdata.csv")
d_2018_02 <- read.csv("Raw Data/201802-capitalbikeshare-tripdata.csv")
d_2018_03 <- read.csv("Raw Data/201803-capitalbikeshare-tripdata.csv")
d_2018_04 <- read.csv("Raw Data/201804-capitalbikeshare-tripdata.csv")
d_2018_05 <- read.csv("Raw Data/201805-capitalbikeshare-tripdata.csv")
d_2018_06 <- read.csv("Raw Data/201806-capitalbikeshare-tripdata.csv")
d_2018_07 <- read.csv("Raw Data/201807-capitalbikeshare-tripdata.csv")
d_2018_08 <- read.csv("Raw Data/201808-capitalbikeshare-tripdata.csv")
d_2018_09 <- read.csv("Raw Data/201809-capitalbikeshare-tripdata.csv")
d_2018_10 <- read.csv("Raw Data/201810-capitalbikeshare-tripdata.csv")
d_2018_11 <- read.csv("Raw Data/201811-capitalbikeshare-tripdata.csv")
d_2018_12 <- read.csv("Raw Data/201812-capitalbikeshare-tripdata.csv")
d_2019_01 <- read.csv("Raw Data/201901-capitalbikeshare-tripdata.csv")
d_2019_02 <- read.csv("Raw Data/201902-capitalbikeshare-tripdata.csv")
d_2019_03 <- read.csv("Raw Data/201903-capitalbikeshare-tripdata.csv")
d_2019_04 <- read.csv("Raw Data/201904-capitalbikeshare-tripdata.csv")
d_2019_05 <- read.csv("Raw Data/201905-capitalbikeshare-tripdata.csv")
d_2019_06 <- read.csv("Raw Data/201906-capitalbikeshare-tripdata.csv")
d_2019_07 <- read.csv("Raw Data/201907-capitalbikeshare-tripdata.csv")
d_2019_08 <- read.csv("Raw Data/201908-capitalbikeshare-tripdata.csv")
d_2019_09 <- read.csv("Raw Data/201909-capitalbikeshare-tripdata.csv")
d_2019_10 <- read.csv("Raw Data/201910-capitalbikeshare-tripdata.csv")
d_2019_11 <- read.csv("Raw Data/201911-capitalbikeshare-tripdata.csv")
d_2019_12 <- read.csv("Raw Data/201912-capitalbikeshare-tripdata.csv")


# Combine raw data sets ---------------------------------------------------

d_2017 <- bind_rows(list(d_2017_Q1, d_2017_Q2, d_2017_Q3, d_2017_Q4))
d_2018 <- bind_rows(list(d_2018_01, d_2018_02, d_2018_03, d_2018_04,
                         d_2018_05, d_2018_06, d_2018_07, d_2018_08,
                         d_2018_09, d_2018_10, d_2018_11, d_2018_12))
d_2019 <- bind_rows(list(d_2019_01, d_2019_02, d_2019_03, d_2019_04,
                         d_2019_05, d_2019_06, d_2019_07, d_2019_08,
                         d_2019_09, d_2019_10, d_2019_11, d_2019_12))
saveRDS(d_2017, file = "Data/d_2017.rds")
saveRDS(d_2018, file = "Data/d_2018.rds")
saveRDS(d_2019, file = "Data/d_2019.rds")

d_2017 <- readRDS("Data/d_2017.rds")
d_2018 <- readRDS("Data/d_2018.rds")
d_2019 <- readRDS("Data/d_2019.rds")
OD_data <- bind_rows(list(d_2017, d_2018, d_2019))


# Load station data -------------------------------------------------------

# Data from https://opendata.dc.gov/datasets/capital-bike-share-locations/geoservice?geometry=-78.017%2C38.734%2C-75.931%2C39.108
station_list <- unique(c(OD_data$Start.station.number, OD_data$End.station.number))
stations <- read.csv("Raw Data/Capital_Bike_Share_Locations.csv")
station_list <- station_list[which(station_list %in% stations$TERMINAL_NUMBER)]


# Process combined data ---------------------------------------------------

OD <- apply(OD_data, 1, function(x) gsub(" ", "", paste(x[4],"-",x[6], sep="")))
OD_data$OD <- OD
OD_data <- OD_data[which(OD_data$Start.station.number %in% station_list & OD_data$End.station.number %in% station_list),]
saveRDS(OD_data, file = "Data/OD_data.rds")


# Process station data-----------------------------------------------------

OD_data <- readRDS("Data/OD_data.rds")
station_list <- sort(unique(c(OD_data$Start.station.number, OD_data$End.station.number)))
s1 <- stations[which(stations$TERMINAL_NUMBER %in% station_list),c(4,5,6)]
station_data <- s1[order(s1$TERMINAL_NUMBER),]
saveRDS(station_data, file = "Data/station_data_new.rds")
