clustering <- function(cor_mat, station_data, D_inner, D_outer, R, corr_threshold) {
  adj_mat <- matrix(0, nrow = nrow(station_data), ncol = nrow(station_data))
  for (i in 1:nrow(station_data)) {
    for (j in 1:nrow(station_data)) {
      if (i > j) {
        if (station_data$centre_distances[i] < R & station_data$centre_distances[i] < R) {
          if (c_dist(station_data$TERMINAL_NUMBER[i], station_data$TERMINAL_NUMBER[j]) < D_inner) {
            adj_mat[i, j] <- 1
            adj_mat[j, i] <- 1
          }
        } else {
          if (c_dist(station_data$TERMINAL_NUMBER[i], station_data$TERMINAL_NUMBER[j]) < D_outer) {
            adj_mat[i, j] <- 1
            adj_mat[j, i] <- 1
          }
        }
      }
    }
  }
  input_mat <- adj_mat * (1 - cor_mat)
  g_t <- graph.adjacency(adjmatrix = input_mat, mode = "undirected", weighted = TRUE)
  mst_g_t <- mst(g_t)
  mst_weights <- edge_attr(mst_g_t)$weight
  edges_remove <- which(mst_weights >= (1 - corr_threshold))
  new_graph <- delete_edges(mst_g_t, edges_remove)
  components <- decompose(new_graph, min.vertices = 1)
  cluster_list <- list()
  for (c in 1:length(components)) {
    cluster_list[[c]] <- get.vertex.attribute(components[[c]])$name
  }
  return(cluster_list)
}