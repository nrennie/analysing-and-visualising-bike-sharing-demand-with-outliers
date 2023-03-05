merge_differences <- function(l){
  #input is a named list of vectors with names
  m <- list()
  for (i in 1:length(l)){
    if (length(l[[i]]) > 0){
      m[[i]] <- data.frame(rn=names(l[[i]]), l[[i]])
      colnames(m[[i]])[2] <- names(l)[i]
    } else{
      m[[i]] <- data.frame(rn=numeric())
    }
  }
  k <- data.frame(Reduce(function(x,y) merge(x=x, y=y, by="rn", all=TRUE), m)) %>% remove_rownames %>% column_to_rownames(var="rn")
  return(data.matrix(k))
}
