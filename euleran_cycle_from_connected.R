euleran_cycle_from_connected = function(arcs, graph){
  graph = graph[-1,]
  # browser()
  rownames(arcs) = NULL
  node_table_old = arcs[,c("StartNodeNumber", "EndNodeNumber")] %>% 
    unlist %>%
    as.numeric() %>%
    table() %>%
    data.frame()
  # browser()
  
    arcs = add_eulerian_need_cycle(arcs,graph)

  # browser()
  
  arcs2 = arcs
  node = arcs$StartNodeNumber %>% cbind(arcs$EndNodeNumber) %>% unlist %>% as.numeric() %>% unique %>% sort
  for (j in arcs2[,1:3] %>% duplicated() %>% which()) {
    new_arc = arcs2[j,]
    new_arc$StartNodeNumber = arcs2$EndNodeNumber[j] = max(node)+j
    arcs2 = rbind(arcs2, new_arc)
  } 
  # browser()
  node2 = arcs2$StartNodeNumber %>% cbind(arcs2$EndNodeNumber) %>% unlist %>% as.numeric() %>% unique %>% sort
  g <- new("graphNEL", nodes=as.character(node2), edgemode="undirected")
  g <- addEdge(graph=g, from=as.character(arcs2$StartNodeNumber), 
               to=as.character(arcs2$EndNodeNumber))
  start_point = as.character(node[s.paths[1,node] %>% which.min()])

  ep = eulerian(g, start = start_point) %>% as.numeric()
  # browser()
  inserted_path = ep %>% c(head(paths_list[[1]][[as.numeric(start_point)]],-1),.)
  
  arcs_part= lapply(1:(length(inserted_path)-1), function(j){
    c(inserted_path[j],inserted_path[j+1])
  }) %>% do.call(rbind,.) %>% as.data.frame()
  r_match = cbind(row.match(arcs_part[,1:2],arcs2[,c("StartNodeNumber", "EndNodeNumber")], nomatch = F),
                  row.match(arcs_part[,2:1],arcs2[,c("StartNodeNumber", "EndNodeNumber")], nomatch = F))
  # browser()
  service_0 = apply(as.data.frame(r_match), MARGIN = 1, FUN = max) %>% duplicated()
  arcs_new = lapply(1:nrow(r_match), function(i){
    # browser()
    direction = which(r_match[i,] != 0)
    if (direction == 1) {
      temp = arcs2[r_match[i,] %>% max(), ]
      if (service_0[i]) {
        temp$service = 0
      }
      temp
    }else{
      temp = arcs2[r_match[i,] %>% max(), ]
      temp$StartNodeNumber = arcs2$EndNodeNumber[r_match[i,] %>% max()]
      temp$EndNodeNumber = arcs2$StartNodeNumber[r_match[i,] %>% max()]
      if (service_0[i]) {
        temp$service = 0
      }
      temp
    }
  }) %>% do.call(rbind,.)
  # browser()
  for (k in node2[node2>max(node)]) {
    arcs_new$EndNodeNumber[arcs_new$EndNodeNumber == k] = arcs_new$EndNodeNumber[arcs_new$StartNodeNumber == k]
    arcs_new = arcs_new[-which(arcs_new$StartNodeNumber == k),]
  }
  rownames(arcs_new) = NULL
  tail_removal = (((arcs_new$service != 0) %>% rev %>% cumsum) == 0) %>% max()
  # browser()
  arcs_new = arcs_new[1:(nrow(arcs_new)-tail_removal),c("StartNodeNumber", "EndNodeNumber", "Lenght", "service")]
  return(arcs_new)
}
