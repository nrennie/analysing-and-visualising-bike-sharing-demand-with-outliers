# Source functions script -------------------------------------------------

source("Cleaning Scripts/00_packages.R")


# Read data ---------------------------------------------------------------

agg_station_matrix <- readRDS("Data/agg_station_matrix.rds")


# Apply regression --------------------------------------------------------

d1 <- agg_station_matrix[[233]]
d2 <- agg_station_matrix[[238]]
d3 <- agg_station_matrix[[239]]
d4 <- agg_station_matrix[[244]]
d5 <- agg_station_matrix[[245]]
d6 <- agg_station_matrix[[246]]
d7 <- agg_station_matrix[[248]]
d8 <- agg_station_matrix[[442]]
d9 <- agg_station_matrix[[467]]

summer <- c("April", "May", "June", "July", "August", "September", "October")
winter <- c("November", "December", "January", "February", "March")

d1_s <- d1[which(months(d1$date_of_day) %in% summer), ]
d2_s <- d2[which(months(d2$date_of_day) %in% summer), ]
d3_s <- d3[which(months(d3$date_of_day) %in% summer), ]
d4_s <- d4[which(months(d4$date_of_day) %in% summer), ]
d5_s <- d5[which(months(d5$date_of_day) %in% summer), ]
d6_s <- d6[which(months(d6$date_of_day) %in% summer), ]
d7_s <- d7[which(months(d7$date_of_day) %in% summer), ]
d8_s <- d8[which(months(d8$date_of_day) %in% summer), ]
d9_s <- d9[which(months(d9$date_of_day) %in% summer), ]

d1_w <- d1[which(months(d1$date_of_day) %in% winter), ]
d2_w <- d2[which(months(d2$date_of_day) %in% winter), ]
d3_w <- d3[which(months(d3$date_of_day) %in% winter), ]
d4_w <- d4[which(months(d4$date_of_day) %in% winter), ]
d5_w <- d5[which(months(d5$date_of_day) %in% winter), ]
d6_w <- d6[which(months(d6$date_of_day) %in% winter), ]
d7_w <- d7[which(months(d7$date_of_day) %in% winter), ]
d8_w <- d8[which(months(d8$date_of_day) %in% winter), ]
d9_w <- d9[which(months(d9$date_of_day) %in% winter), ]

res1_s <- residuals_function(d1_s, c(1, 1, 1))
res2_s <- residuals_function(d2_s, c(1, 1, 1))
res3_s <- residuals_function(d3_s, c(1, 1, 1))
res4_s <- residuals_function(d4_s, c(1, 1, 1))
res5_s <- residuals_function(d5_s, c(1, 1, 1))
res6_s <- residuals_function(d6_s, c(1, 1, 1))
res7_s <- residuals_function(d7_s, c(1, 1, 1))
res8_s <- residuals_function(d8_s, c(1, 1, 1))
res9_s <- residuals_function(d9_s, c(1, 1, 1))

res1_w <- residuals_function(d1_w, c(1, 1, 1))
res2_w <- residuals_function(d2_w, c(1, 1, 1))
res3_w <- residuals_function(d3_w, c(1, 1, 1))
res4_w <- residuals_function(d4_w, c(1, 1, 1))
res5_w <- residuals_function(d5_w, c(1, 1, 1))
res6_w <- residuals_function(d6_w, c(1, 1, 1))
res7_w <- residuals_function(d7_w, c(1, 1, 1))
res8_w <- residuals_function(d8_w, c(1, 1, 1))
res9_w <- residuals_function(d9_w, c(1, 1, 1))


# Calculate depths --------------------------------------------------------

depths1_s <- mfd(array(t(res1_s[, 2:25]), dim = c(ncol(res1_s[, 2:25]), nrow(res1_s[, 2:25]), 1)), time = 0:23, type = "projdepth")$MFDdepthZ
c1_s <- depth_threshold(res1_s[, 2:25], perc = 0.01)
dates1_s <- as.Date(res1_s$date_of_day, origin = "1970-01-01")
depths1_w <- mfd(array(t(res1_w[, 2:25]), dim = c(ncol(res1_w[, 2:25]), nrow(res1_w[, 2:25]), 1)), time = 0:23, type = "projdepth")$MFDdepthZ
c1_w <- depth_threshold(res1_w[, 2:25], perc = 0.01)
dates1_w <- as.Date(res1_w$date_of_day, origin = "1970-01-01")
df1 <- data.frame(depths1 = c(depths1_s, depths1_w), dates1 = c(dates1_s, dates1_w))
df1 <- df1[order(df1$dates1), ]
df1$obs1 <- 1:nrow(df1)

depths2_s <- mfd(array(t(res2_s[, 2:25]), dim = c(ncol(res2_s[, 2:25]), nrow(res2_s[, 2:25]), 1)), time = 0:23, type = "projdepth")$MFDdepthZ
c2_s <- depth_threshold(res2_s[, 2:25], perc = 0.01)
dates2_s <- as.Date(res2_s$date_of_day, origin = "1970-01-01")
depths2_w <- mfd(array(t(res2_w[, 2:25]), dim = c(ncol(res2_w[, 2:25]), nrow(res2_w[, 2:25]), 1)), time = 0:23, type = "projdepth")$MFDdepthZ
c2_w <- depth_threshold(res2_w[, 2:25], perc = 0.01)
dates2_w <- as.Date(res2_w$date_of_day, origin = "1970-01-01")
df2 <- data.frame(depths2 = c(depths2_s, depths2_w), dates2 = c(dates2_s, dates2_w))
df2 <- df2[order(df2$dates2), ]
df2$obs2 <- 1:nrow(df2)

depths3_s <- mfd(array(t(res3_s[, 2:25]), dim = c(ncol(res3_s[, 2:25]), nrow(res3_s[, 2:25]), 1)), time = 0:23, type = "projdepth")$MFDdepthZ
c3_s <- depth_threshold(res3_s[, 2:25], perc = 0.01)
dates3_s <- as.Date(res3_s$date_of_day, origin = "1970-01-01")
depths3_w <- mfd(array(t(res3_w[, 2:25]), dim = c(ncol(res3_w[, 2:25]), nrow(res3_w[, 2:25]), 1)), time = 0:23, type = "projdepth")$MFDdepthZ
c3_w <- depth_threshold(res3_w[, 2:25], perc = 0.01)
dates3_w <- as.Date(res3_w$date_of_day, origin = "1970-01-01")
df3 <- data.frame(depths3 = c(depths3_s, depths3_w), dates3 = c(dates3_s, dates3_w))
df3 <- df3[order(df3$dates3), ]
df3$obs3 <- 1:nrow(df3)

depths4_s <- mfd(array(t(res4_s[, 2:25]), dim = c(ncol(res4_s[, 2:25]), nrow(res4_s[, 2:25]), 1)), time = 0:23, type = "projdepth")$MFDdepthZ
c4_s <- depth_threshold(res4_s[, 2:25], perc = 0.01)
dates4_s <- as.Date(res4_s$date_of_day, origin = "1970-01-01")
depths4_w <- mfd(array(t(res4_w[, 2:25]), dim = c(ncol(res4_w[, 2:25]), nrow(res4_w[, 2:25]), 1)), time = 0:23, type = "projdepth")$MFDdepthZ
c4_w <- depth_threshold(res4_w[, 2:25], perc = 0.01)
dates4_w <- as.Date(res4_w$date_of_day, origin = "1970-01-01")
df4 <- data.frame(depths4 = c(depths4_s, depths4_w), dates4 = c(dates4_s, dates4_w))
df4 <- df4[order(df4$dates4), ]
df4$obs4 <- 1:nrow(df4)

depths5_s <- mfd(array(t(res5_s[, 2:25]), dim = c(ncol(res5_s[, 2:25]), nrow(res5_s[, 2:25]), 1)), time = 0:23, type = "projdepth")$MFDdepthZ
c5_s <- depth_threshold(res5_s[, 2:25], perc = 0.01)
dates5_s <- as.Date(res5_s$date_of_day, origin = "1970-01-01")
depths5_w <- mfd(array(t(res5_w[, 2:25]), dim = c(ncol(res5_w[, 2:25]), nrow(res5_w[, 2:25]), 1)), time = 0:23, type = "projdepth")$MFDdepthZ
c5_w <- depth_threshold(res5_w[, 2:25], perc = 0.01)
dates5_w <- as.Date(res5_w$date_of_day, origin = "1970-01-01")
df5 <- data.frame(depths5 = c(depths5_s, depths5_w), dates5 = c(dates5_s, dates5_w))
df5 <- df5[order(df5$dates5), ]
df5$obs5 <- 1:nrow(df5)

depths6_s <- mfd(array(t(res6_s[, 2:25]), dim = c(ncol(res6_s[, 2:25]), nrow(res6_s[, 2:25]), 1)), time = 0:23, type = "projdepth")$MFDdepthZ
c6_s <- depth_threshold(res6_s[, 2:25], perc = 0.01)
dates6_s <- as.Date(res6_s$date_of_day, origin = "1970-01-01")
depths6_w <- mfd(array(t(res6_w[, 2:25]), dim = c(ncol(res6_w[, 2:25]), nrow(res6_w[, 2:25]), 1)), time = 0:23, type = "projdepth")$MFDdepthZ
c6_w <- depth_threshold(res6_w[, 2:25], perc = 0.01)
dates6_w <- as.Date(res6_w$date_of_day, origin = "1970-01-01")
df6 <- data.frame(depths6 = c(depths6_s, depths6_w), dates6 = c(dates6_s, dates6_w))
df6 <- df6[order(df6$dates6), ]
df6$obs6 <- 1:nrow(df6)

depths7_s <- mfd(array(t(res7_s[, 2:25]), dim = c(ncol(res7_s[, 2:25]), nrow(res7_s[, 2:25]), 1)), time = 0:23, type = "projdepth")$MFDdepthZ
c7_s <- depth_threshold(res7_s[, 2:25], perc = 0.01)
dates7_s <- as.Date(res7_s$date_of_day, origin = "1970-01-01")
depths7_w <- mfd(array(t(res7_w[, 2:25]), dim = c(ncol(res7_w[, 2:25]), nrow(res7_w[, 2:25]), 1)), time = 0:23, type = "projdepth")$MFDdepthZ
c7_w <- depth_threshold(res7_w[, 2:25], perc = 0.01)
dates7_w <- as.Date(res7_w$date_of_day, origin = "1970-01-01")
df7 <- data.frame(depths7 = c(depths7_s, depths7_w), dates7 = c(dates7_s, dates7_w))
df7 <- df7[order(df7$dates7), ]
df7$obs7 <- 1:nrow(df7)

depths8_s <- mfd(array(t(res8_s[, 2:25]), dim = c(ncol(res8_s[, 2:25]), nrow(res8_s[, 2:25]), 1)), time = 0:23, type = "projdepth")$MFDdepthZ
c8_s <- depth_threshold(res8_s[, 2:25], perc = 0.01)
dates8_s <- as.Date(res8_s$date_of_day, origin = "1970-01-01")
depths8_w <- mfd(array(t(res8_w[, 2:25]), dim = c(ncol(res8_w[, 2:25]), nrow(res8_w[, 2:25]), 1)), time = 0:23, type = "projdepth")$MFDdepthZ
c8_w <- depth_threshold(res8_w[, 2:25], perc = 0.01)
dates8_w <- as.Date(res8_w$date_of_day, origin = "1970-01-01")
df8 <- data.frame(depths8 = c(depths8_s, depths8_w), dates8 = c(dates8_s, dates8_w))
df8 <- df8[order(df8$dates8), ]
df8$obs8 <- 1:nrow(df8)

depths9_s <- mfd(array(t(res9_s[, 2:25]), dim = c(ncol(res9_s[, 2:25]), nrow(res9_s[, 2:25]), 1)), time = 0:23, type = "projdepth")$MFDdepthZ
c9_s <- depth_threshold(res9_s[, 2:25], perc = 0.01)
dates9_s <- as.Date(res9_s$date_of_day, origin = "1970-01-01")
depths9_w <- mfd(array(t(res9_w[, 2:25]), dim = c(ncol(res9_w[, 2:25]), nrow(res9_w[, 2:25]), 1)), time = 0:23, type = "projdepth")$MFDdepthZ
c9_w <- depth_threshold(res9_w[, 2:25], perc = 0.01)
dates9_w <- as.Date(res9_w$date_of_day, origin = "1970-01-01")
df9 <- data.frame(depths9 = c(depths9_s, depths9_w), dates9 = c(dates9_s, dates9_w))
df9 <- df9[order(df9$dates9), ]
df9$obs9 <- 1:nrow(df9)


# Calculate differences ---------------------------------------------------

diffs1_s <- (c1_s - depths1_s) / c1_s
diffs1_w <- (c1_w - depths1_w) / c1_w
df1 <- data.frame(diffs1 = c(diffs1_s, diffs1_w), dates1 = c(dates1_s, dates1_w))
df1 <- df1[order(df1$dates1), ]
df1$obs1 <- 1:nrow(df1)

diffs2_s <- (c2_s - depths2_s) / c2_s
diffs2_w <- (c2_w - depths2_w) / c2_w
df2 <- data.frame(diffs2 = c(diffs2_s, diffs2_w), dates2 = c(dates2_s, dates2_w))
df2 <- df2[order(df2$dates2), ]
df2$obs2 <- 1:nrow(df2)

diffs3_s <- (c3_s - depths3_s) / c3_s
diffs3_w <- (c3_w - depths3_w) / c3_w
df3 <- data.frame(diffs3 = c(diffs3_s, diffs3_w), dates3 = c(dates3_s, dates3_w))
df3 <- df3[order(df3$dates3), ]
df3$obs3 <- 1:nrow(df3)

diffs1_s <- (c1_s - depths1_s) / c1_s
diffs1_w <- (c1_w - depths1_w) / c1_w
df1 <- data.frame(diffs1 = c(diffs1_s, diffs1_w), dates1 = c(dates1_s, dates1_w))
df1 <- df1[order(df1$dates1), ]
df1$obs1 <- 1:nrow(df1)

diffs4_s <- (c4_s - depths4_s) / c4_s
diffs4_w <- (c4_w - depths4_w) / c4_w
df4 <- data.frame(diffs4 = c(diffs4_s, diffs4_w), dates4 = c(dates4_s, dates4_w))
df4 <- df4[order(df4$dates4), ]
df4$obs4 <- 1:nrow(df4)

diffs5_s <- (c5_s - depths5_s) / c5_s
diffs5_w <- (c5_w - depths5_w) / c5_w
df5 <- data.frame(diffs5 = c(diffs5_s, diffs5_w), dates5 = c(dates5_s, dates5_w))
df5 <- df5[order(df5$dates5), ]
df5$obs5 <- 1:nrow(df5)

diffs6_s <- (c6_s - depths6_s) / c6_s
diffs6_w <- (c6_w - depths6_w) / c6_w
df6 <- data.frame(diffs6 = c(diffs6_s, diffs6_w), dates6 = c(dates6_s, dates6_w))
df6 <- df6[order(df6$dates6), ]
df6$obs6 <- 1:nrow(df6)

diffs7_s <- (c7_s - depths7_s) / c7_s
diffs7_w <- (c7_w - depths7_w) / c7_w
df7 <- data.frame(diffs7 = c(diffs7_s, diffs7_w), dates7 = c(dates7_s, dates7_w))
df7 <- df7[order(df7$dates7), ]
df7$obs7 <- 1:nrow(df7)

diffs8_s <- (c8_s - depths8_s) / c8_s
diffs8_w <- (c8_w - depths8_w) / c8_w
df8 <- data.frame(diffs8 = c(diffs8_s, diffs8_w), dates8 = c(dates8_s, dates8_w))
df8 <- df8[order(df8$dates8), ]
df8$obs8 <- 1:nrow(df8)

diffs9_s <- (c9_s - depths9_s) / c9_s
diffs9_w <- (c9_w - depths9_w) / c9_w
df9 <- data.frame(diffs9 = c(diffs9_s, diffs9_w), dates9 = c(dates9_s, dates9_w))
df9 <- df9[order(df9$dates9), ]
df9$obs9 <- 1:nrow(df9)

diffs1 <- c(diffs1_s, diffs1_w)
names(diffs1) <- c(dates1_s, dates1_w)
diffs2 <- c(diffs2_s, diffs2_w)
names(diffs2) <- c(dates2_s, dates2_w)
diffs3 <- c(diffs3_s, diffs3_w)
names(diffs3) <- c(dates3_s, dates3_w)
diffs4 <- c(diffs4_s, diffs4_w)
names(diffs4) <- c(dates4_s, dates4_w)
diffs5 <- c(diffs5_s, diffs5_w)
names(diffs5) <- c(dates5_s, dates5_w)
diffs6 <- c(diffs6_s, diffs6_w)
names(diffs6) <- c(dates6_s, dates6_w)
diffs7 <- c(diffs7_s, diffs7_w)
names(diffs7) <- c(dates7_s, dates7_w)
diffs8 <- c(diffs8_s, diffs8_w)
names(diffs8) <- c(dates8_s, dates8_w)
diffs9 <- c(diffs9_s, diffs9_w)
names(diffs9) <- c(dates9_s, dates9_w)


# Get exceedances summed --------------------------------------------------

x <- merge_differences(list(
  t31303 = diffs1, t31308 = diffs2, t31309 = diffs3,
  t31315 = diffs4, t31316 = diffs5, t31317 = diffs6,
  t31319 = diffs7, t32014 = diffs8, t32040 = diffs9
))
c <- apply(x, 1, function(y) sum(y[y > 0]))
m <- fitgpd(data = c, threshold = 0, est = "mle")
exceedances <- na.omit(c[c > 0])


# Save data ---------------------------------------------------------------

saveRDS(exceedances, "Data/exceedances.rds")
