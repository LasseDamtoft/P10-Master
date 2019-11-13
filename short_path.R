short_paths = function(graph, paths = F, distances = F){
  if (!paths & !distances) {
    stop('Choose paths or distances')
  }
  library(igraph)

  mat = rep(0,(max(graph[,c(2,3)])-min(graph[,c(2,3)]))^2) %>% 
    matrix(nrow = (max(graph[,c(2,3)])-min(graph[,c(2,3)])))
  
  
  for (arc in 1:max(graph$EdgeNumber)) {
    arc_current = graph %>% slice(which(EdgeNumber == arc))
    node1 = arc_current$StartNodeNumber
    node2 = arc_current$EndNodeNumber
    mat[node1,node2] = mat[node2,node1] = arc_current$Lenght
    
    
  }
  g <- graph.adjacency(mat, weighted=TRUE)
  if (distances) {
    s.paths <- shortest.paths(g, algorithm = "dijkstra")
    return(s.paths)
  }
  if (paths) {
    paths = lapply(1:max(graph[,c(2,3)]), function(node1){
      lapply(1:max(graph[,c(2,3)]), function(node2){
        if (node1 != node2) {
          shortest_paths(g, from = node1, node2)$vpath[[1]] %>% as.numeric()
        }
      })
    })
    return(paths)
  }

  
  
  
}