graph_loader = function(path = "CARP graphs/CARP_F1_g_graph.dat", seed = 1337){
  library(dplyr)
  graph_raw = read.table(path, sep ="\t", skip = 7, fill = T, header = T)
  graph = graph_raw %>% 
    slice(2:(which(graph_raw$EdgeNumber == 'END')-1)) %>% 
    select(-EdgeId)
  names(graph)[c(4,5,6)] = c('Lenght', 'Width', 'Class')
  graph$Class = 0
  graph$StartNodeNumber = graph$StartNodeNumber %>% as.character %>% as.numeric()
  graph$EdgeNumber = graph$EdgeNumber %>% as.character %>% as.numeric()
  graph$EndNodeNumber = graph$EndNodeNumber %>% as.character %>% as.numeric()
  set.seed(seed)
  # browser()
  graph = graph %>% 
    mutate(service = if_else(Width != 0, 1, 0),
           Width = runif(n(), 6,12))
  graph = graph[order(graph$EdgeNumber),]
  rownames(graph) = NULL
  return(graph)
}