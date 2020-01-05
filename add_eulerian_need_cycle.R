add_eulerian_need_cycle = function(arcs,graph){
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
  
  
  nodene = node_table$.[node_table$Freq %% 2 != 0] %>% as.data.frame()
  
  input_txt = lapply(1:nrow(nodene), function(node1){
    lapply(1:nrow(nodene), function(node2){
      if (node1>node2) {
        c(node1-1,node2-1, s.paths[nodene[node1,],nodene[node2,]]) 
      }
    }) %>% do.call(rbind,.)
  }) %>% do.call(rbind,.) %>% as.matrix()
# browser()
  input_file <- tempfile()
  write(n_nodes, file = input_file)
  write(nrow(input_txt), file = input_file, append = TRUE)
  write.table(input_txt, file = input_file, append = TRUE, row.names = FALSE, col.names = FALSE)
  
  result <- minimum_cost_perfect_matching(input_file)
  
  result$edges
  mcpm_edges = nodene[result$edges+1,]  %>% 
    matrix(ncol = 2, byrow = T)
  # browser()
  arcs_part = list()
  for (i in 1:nrow(mcpm_edges)) {
    inserted_path = paths_list[[mcpm_edges[i,1]]][[mcpm_edges[i,2]]]
    arcs_part[[i]]= lapply(1:(length(inserted_path)-1), function(j){
      c(inserted_path[j],inserted_path[j+1], s.paths[inserted_path[j],inserted_path[j+1]])
    }) %>% do.call(rbind,.) %>% as.data.frame()
    r_match = rbind(row.match(arcs_part[[i]][,c(2,1,3)],graph[,c("StartNodeNumber", "EndNodeNumber", "Lenght")], nomatch = F),
                    row.match(arcs_part[[i]][,1:3],graph[,c("StartNodeNumber", "EndNodeNumber", "Lenght")], nomatch = F)) %>%
      as.data.frame() %>% 
      sapply(max) %>% 
      as.numeric()
    # browser()
    temppp = graph[r_match,c("StartNodeNumber", "EndNodeNumber", "Lenght", "service")]
    temppp$service = 0
    
    arcs = rbind(arcs,temppp)
  }
  # browser()
  

  rownames(arcs) = NULL
  return(arcs)
}
