connect_shortest = function(arcs, graph, connected_subs){
  # browser()
  node_sets = lapply(1:length(connected_subs), function(j){
    connected_subs[[j]][,c('StartNodeNumber', 'EndNodeNumber')] %>% unlist %>%  as.numeric() %>% unique
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
  # browser()
  names(arcs_part) = names(arcs)
  arcs = rbind(connected_subs[[1]], arcs_part, connected_subs[[(1+which.min(connect_length))]])
  rownames(arcs) = NULL
  connected_subs[[1]] = arcs
  # browser()
  connected_subs2 = connected_subs[-(1+which.min(connect_length))]
  return(connected_subs2)
}

make_arcs_connected = function(arcs, graph){
  connected_list = sub_graphs2(arcs,graph)
  for (i in 1:(length(connected_list)-1)) {
    connected_list = connect_shortest(arcs, graph, connected_list)
  }
  # browser()
  connected_list = connected_list %>% do.call(rbind,.)
  return(connected_list)
}
