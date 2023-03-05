#clustering function
mst_clustering_threshold <- function(input_mat, corr_threshold=0){
  g_t <- graph.adjacency(adjmatrix=input_mat, mode="undirected", weighted=TRUE)
  #obtain minimum spanning tree
  mst_g_t <- mst(g_t)
  #obtain clusters
  mst_weights <- edge_attr(mst_g_t)$weight
  edges_remove <- which(mst_weights >= (1 - corr_threshold))
  new_graph <- delete_edges(mst_g_t, edges_remove)
  #return clusters
  components <- decompose(new_graph, min.vertices=1)
  cluster_list <- list()
  for (c in 1:length(components)){
    cluster_list[[c]] <- get.vertex.attribute(components[[c]])$name 
  }
  return(list(cluster_list=cluster_list, adj_mat_corr=as_adjacency_matrix(new_graph, type="both", sparse=F)))
}

