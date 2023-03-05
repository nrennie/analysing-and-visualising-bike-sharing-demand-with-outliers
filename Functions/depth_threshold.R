# depth_threshold <- function(data,times=0:23,perc=0.01,B=1000){
#   #data is a data frame of cumulative bookings/forecasts for each flight
#   n <- nrow(data)
#   #array should be t (30) x n (500) x p (1)
#   diffs <- array(t(data), dim=c(ncol(data),nrow(data),1))
#   weights <- mfd(diffs, time=times, type="projdepth")$MFDdepthZ
#   #print(length(weights))
#   #create an empty list to store bootstrap samples
#   w <- numeric(length = B)
#   diff1 <- as.matrix(diffs[,,1])
#   for (i in 1:B){
#     #create bootstrap samples
#     b <- diff1[,sample(1:ncol(diff1),size=ncol(diff1),replace=TRUE,prob=weights)]
#     #calculate smoothing matrix
#     s <- matrix(mvrnorm(nrow(data),rep(0,ncol(data)),Sigma=0.05*cov(data)),nrow=ncol(data),ncol=n,byrow=TRUE)
#     #smooth samples
#     y <- array(b+s,dim=c(ncol(data),nrow(data),1))
#     #calculate percentile of each set of weights
#     k <- mfd(y, time=times, type="projdepth")$MFDdepthZ
#     w[[i]] <- sort(k)[ceiling(perc*B)]
#   }
#   return(median(w))
# }


depth_threshold <- function(data,times=0:23,perc=0.01,B=10){
  #data is a data frame of cumulative bookings/forecasts for each flight
  n <- nrow(data)
  #array should be t (30) x n (500) x p (1)
  diffs <- array(t(data), dim=c(ncol(data),nrow(data),1))
  weights <- suppressWarnings(mfd(diffs, time=times, type="projdepth")$MFDdepthZ)
  if (sum(!is.na(weights)) == 0){
    return(NA)
  }
  if (sum(weights, na.rm=T) == 0){
    return(NA)
  }
  if (B == 0){
    m <- mean(weights)
    s <- var(weights)
    alpha <- (m^2)*(((1-m)/s) - (1/m))
    beta <- alpha*((1/m)-1)
    #return percentile of distribution
    threshold <- qbeta(perc, shape1=alpha, shape2=beta)
    return(threshold)
  } else{
    #create an empty list to store bootstrap samples
    w <- numeric(length = B)
    diff1 <- as.matrix(diffs[,,1])
    for (i in 1:B){
      #create bootstrap samples
      b <- diff1[,sample(1:ncol(diff1),size=ncol(diff1),replace=TRUE,prob=weights)]
      #calculate smoothing matrix
      s <- matrix(mvrnorm(nrow(data),rep(0,ncol(data)),Sigma=0.05*cov(data)),nrow=ncol(data),ncol=n,byrow=TRUE)
      #smooth samples
      y <- array(b+s,dim=c(ncol(data),nrow(data),1))
      #calculate percentile of each set of weights
      k <- suppressWarnings(mfd(y, time=times, type="projdepth")$MFDdepthZ)
      m <- mean(k)
      s <- var(k)
      alpha <- (m^2)*(((1-m)/s) - (1/m))
      beta <- alpha*((1/m)-1)
      #return percentile of distribution
      w[[i]] <- qbeta(perc, shape1=alpha, shape2=beta)
    }
    return(median(w))
  }
}
