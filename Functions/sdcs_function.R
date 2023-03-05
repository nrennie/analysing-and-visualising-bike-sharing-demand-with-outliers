sdcs_function <- function(input_vec){
  k <- length(input_vec)
  n <- sum(input_vec)
  output <- sqrt((sum((input_vec - (n/k))^2))/(k-1))
  return(output)
}
