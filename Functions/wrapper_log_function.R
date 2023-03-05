wrapper_log_function <- function(input_data, input_mat, times=0:23, perc=0.01, B=10, corr_threshold=0, summer=c("April", "May", "June", "July", "August", "September", "October"), winter=c("November", "December", "January", "February", "March")){
  #transform data
  log_input_data <- lapply(input_data, function(x) tibble(cbind(x[,1],log(tibble(x[,2:25]+1)))))
  #clustering procedure
  clusters <- mst_clustering_threshold(input_mat, corr_threshold=corr_threshold)
  clustering <- clusters$cluster_list
  #split data into summer and winter
  season_data <- sapply(log_input_data, function(x) season_split(x))
  summer_data <- season_data["summer",]
  winter_data <- season_data["winter",]
  #run residuals on every list item
  summer_residuals <- lapply(summer_data, function(x) residuals_function(x, c(1,1,1))) 
  winter_residuals <- lapply(winter_data, function(x) residuals_function(x, c(1,1,1))) 
  #run outlier detection
  #summer_depths_list <- lapply(summer_residuals, function(x) depth(x, times=times, perc=perc, B=B))
  summer_depths_list <- list()
  for (i in 1:length(summer_residuals)){
    print(i)
    summer_depths_list[[i]] <- depth(summer_residuals[[i]], times=times, perc=perc, B=B)
  }
  names(summer_depths_list) <- names(summer_residuals)
  #winter_depths_list <- lapply(winter_residuals, function(x) depth(x, times=times, perc=perc, B=B))
  winter_depths_list <- list()
  for (i in 1:length(winter_residuals)){
    print(i)
    winter_depths_list[[i]] <- depth(winter_residuals[[i]], times=times, perc=perc, B=B)
  }
  names(winter_depths_list) <- names(winter_residuals)
  #join data back together
  depths_list <- mapply(c, summer_depths_list, winter_depths_list, SIMPLIFY=FALSE)
  #for loop for each cluster 
  alert_lists <- list()
  for (c in 1:length(clustering)){
    #extract elements in cluster c from input list
    cluster_c <- depths_list[which(names(depths_list) %in% clustering[[c]])]
    #if cluster empty, return an empty data frame
    if (length(cluster_c) == 1 & length(cluster_c[[1]]) == 0) {
      alert_lists[[c]] <- data.frame(Dep=character(), Prob=double(), Legs=character())
    } else {
      #merge the legs in the cluster
      x <- merge_differences(l=cluster_c)
      colnames(x) <- sapply(colnames(x), function(y) name_rem_x(y))
      #obtain the probability table
      alert_lists[[c]] <- gpd_probs(x)
    }
  }
  names(alert_lists) <- unlist(lapply(clustering, function(x) paste(x,collapse=" ", sep=",")))
  return(list(alert_lists, summer_residuals, winter_residuals))
}