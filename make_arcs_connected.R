connect_shortest = function(arcs, graph){
  arcs2  = arcs
  connected_subs = list()
  i=1
  
  while(nrow(arcs2) != 0){
    rest = arcs2[,c("StartNodeNumber", "EndNodeNumber")] 
    current = arcs2[1,c("StartNodeNumber", "EndNodeNumber")]
    row_include = 1
    while(length(row_include) != 0 ){
      rest = rest[-row_include,]
      row_include = (which(as.matrix(rest) %in% as.matrix(current)) %% nrow(rest))
      row_include[row_include == 0] = nrow(rest)
      row_include = row_include %>% unique()
      current = rbind(current, rest[row_include,])
      
    } 
    connected_subs[[i]] = current
    i=i+1
    arcs2 = rest
  }
  # browser()
  
  node_sets = lapply(1:length(connected_subs), function(j){
    connected_subs[[j]] %>% unlist %>%  as.numeric() %>% unique
  })
  connect_length = lapply(node_sets[2:length(node_sets)], function(nodes){
    s.paths[node_sets[[1]], nodes] %>% min()
  })
  nodes = node_sets[[1+which.min(connect_length)]]
  min_path = s.paths[node_sets[[1]], nodes] %>% which.min()
  min_node1 = min_path %% length(node_sets[[1]])
  min_node1 = ifelse(min_node1 == 0, length(node_sets[[1]]), min_node1)
  min_node2 = ceiling(min_path/length(node_sets[[1]]))
  inserted_path = paths_list[[node_sets[[1]][min_node1]]][[nodes[min_node2]]]
  
  arcs_part= lapply(1:(length(inserted_path)-1), function(j){
    c(inserted_path[j],inserted_path[j+1], s.paths[inserted_path[j],inserted_path[j+1]])
  }) %>% do.call(rbind,.) %>% as.data.frame()
  arcs_part$service = 0
  names(arcs_part) = names(arcs)
  arcs = rbind(arcs, arcs_part)
  rownames(arcs) = NULL
  return(arcs)
}

make_arcs_connected = function(arcs, graph){
  while(!connected_rpp(arcs)){
    arcs = connect_shortest(arcs, graph)
  }
  # browser()
  return(arcs)
}
