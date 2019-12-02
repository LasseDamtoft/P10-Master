short_paths = function(graph, paths = F, distances = F){
  if (!paths & !distances) {
    stop('Choose paths or distances')
  }
  library(igraph)

  mat = rep(0,(max(graph[,c(2,3)])-min(graph[,c(2,3)])+1)^2) %>% 
    matrix(nrow = (max(graph[,c(2,3)])-min(graph[,c(2,3)])+1))
  
  # browser()
  
  for (arc in graph$EdgeNumber) {
    arc_current = graph %>% slice(which(EdgeNumber == arc))
    node1 = arc_current$StartNodeNumber
    node2 = arc_current$EndNodeNumber
    # print(c(node1, node2))
    mat[node1,node2] = mat[node2,node1] = arc_current$Lenght
    
    
  }
  
  g <- graph.adjacency(mat, weighted=TRUE, mode = 'undirected')
  # browser()
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