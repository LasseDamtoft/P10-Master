add_eulerian_need_paths = function(arcs,graph){
  rownames(arcs) = NULL
  node_table = arcs[,c("StartNodeNumber", "EndNodeNumber")] %>% 
    unlist %>%
    as.numeric() %>%
    table() %>%
    data.frame()
  # browser()
  node_table$. = node_table$. %>% as.character %>% as.numeric()
  mst_temp = s.paths[node_table$.[node_table$Freq %% 2 != 0],node_table$.[node_table$Freq %% 2 != 0]]
  n_nodes = sqrt(length(mst_temp))
  mst_temp[mst_temp == 0] = max(mst_temp)+1
  n_1 = node_table$.[node_table$Freq %% 2 != 0][ceiling(which.min(mst_temp)/n_nodes)]
  n_2 = node_table$.[node_table$Freq %% 2 != 0][if_else(which.min(mst_temp) %% n_nodes == 0,
                                                        n_nodes,
                                                        which.min(mst_temp) %% n_nodes)]
  inserted_path = paths_list[[n_1]][[n_2]]
  arcs_part= lapply(1:(length(inserted_path)-1), function(j){
    c(inserted_path[j],inserted_path[j+1], s.paths[inserted_path[j],inserted_path[j+1]])
  }) %>% do.call(rbind,.) %>% as.data.frame()
  
  r_match = rbind(row.match(arcs_part[,c(2,1,3)],graph[,c("StartNodeNumber", "EndNodeNumber", "Lenght")], nomatch = F),
                  row.match(arcs_part[,1:3],graph[,c("StartNodeNumber", "EndNodeNumber", "Lenght")], nomatch = F)) %>%
    as.data.frame() %>% 
    sapply(max) %>% 
    as.numeric()
  # browser()
  temppp = graph[r_match,c("StartNodeNumber", "EndNodeNumber", "Lenght", "service")]
  temppp$service = 0
  
  arcs = rbind(arcs,temppp)
  rownames(arcs) = NULL
  return(arcs)
}
