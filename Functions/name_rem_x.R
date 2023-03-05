name_rem_x <- function(name_char){
  return(paste(unlist(strsplit(name_char, ""))[2:(nchar(name_char))], collapse=''))
}