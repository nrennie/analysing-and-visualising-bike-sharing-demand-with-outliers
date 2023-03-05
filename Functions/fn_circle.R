fn_circle <- function(lon1, lat1, radius){ 
  data.frame(degree = 1:360) %>%
    rowwise() %>%
    mutate(lon = destPoint(c(lon1, lat1), degree, radius)[1]) %>%
    mutate(lat = destPoint(c(lon1, lat1), degree, radius)[2]) 
}