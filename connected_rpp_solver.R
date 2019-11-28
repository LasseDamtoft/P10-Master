connected_rpp_solver = function(arcs){
  arcs = arcs[-1,]
  node_table = arcs[,c("StartNodeNumber", "EndNodeNumber")] %>% 
    unlist %>%
    as.numeric() %>%
    table() %>%
    data.frame()
  browser()
  s.paths[which(node_table$Freq %% 2 != 0),which(node_table$Freq %% 2 != 0)]
  
}