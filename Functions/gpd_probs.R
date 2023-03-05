gpd_probs <- function(data_matrix){
  #data matrix is matrix of 
  n <- nrow(data_matrix)
  output_matrix <- data.frame(Dep=character(n), Prob=double(n), Legs=character(n))
  output_matrix$Dep <- rownames(data_matrix)
  #count how many legs an outliers in (vector)
  num_legs_outlier <- apply(data_matrix, 1, function(x) sum(x > 0))
  #sum up values
  diffs <- apply(data_matrix, 1, function(x) sum(x*(x>0), na.rm=T))
  #fit gpd
  if (sum(diffs > 0, na.rm=T) == 0 | sum(diffs) == 0){
    output_matrix[,2] <- rep(0,n)
  }  else {
    m <- suppressWarnings(fitgpd(data=diffs, threshold=0, est = "mle"))
    #output_matrix[,2] <- pgpd(q=diffs, loc=0, scale=m$fitted.values[1], shape=m$fitted.values[2])
    output_matrix[,2] <- pgpd(q=diffs, mu=0, sigma=m$fitted.values[1], xi=m$fitted.values[2])
  }
  for (i in 1:nrow(data_matrix)){
    #add legs affected
    legs_affected <- which(data_matrix[i,] > 0)
    if (length(legs_affected) > 0){
      output_matrix[i,3] <- paste(colnames(data_matrix)[legs_affected], collapse = ', ')
    }
  }
  p <- output_matrix[,2]
  #only return non-neg values
  sorted_df <- output_matrix[order(-as.numeric(p)),]
  to_return <- sorted_df[which(sorted_df[,2] > 0),]
  return(to_return)
}



