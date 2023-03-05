depth <- function(dat, times=0:23, perc=0.01, B=10){
  if (is.na(nrow(dat))){
    return(numeric())
  }
  if (nrow(dat) <= 1){
    return(numeric())
  }
  data <- dat[,2:25]
  C <- depth_threshold(data, times=times, perc=perc, B=B)
  if (is.na(C)){
    d_diffs <- rep(0, nrow(dat))
    names(d_diffs) <- dat$date_of_day
    return(d_diffs)
  }
  n <- nrow(data)
  outlier_names <- dat$date_of_day
  #array should be t (18) x n x p (1)
  d <- array(t(data), dim=c(ncol(data),nrow(data),1))
  depths <- suppressWarnings(mfd(d, time=times, type="projdepth")$MFDdepthZ)
  #which are below threshold
  depth_diffs <- numeric(length=n)
  for (i in 1:n){
    depth_diffs[i] <- ((C - depths[i])/C)
  }
  names(depth_diffs) <- outlier_names
  return(depth_diffs)
}

