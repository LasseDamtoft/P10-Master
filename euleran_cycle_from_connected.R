euleran_cycle_from_connected = function(arcs, graph){
  # graph = graph[-1,]
  # browser()
  rownames(arcs) = NULL
  node_table_old = arcs[,c("StartNodeNumber", "EndNodeNumber")] %>% 
    unlist %>%
    as.numeric() %>%
    table() %>%
    data.frame()
  # browser()
  node_table_old$. = node_table_old$. %>% as.character %>% as.numeric()
  if (length(node_table_old$.[node_table_old$Freq %% 2 != 0])>0) {
    arcs = add_eulerian_need_cycle(arcs,graph)
  }
  
  
  # browser()
  
  arcs2 = arcs
  arcs2[,c("StartNodeNumber", "EndNodeNumber")] = apply(arcs2[,c("StartNodeNumber", "EndNodeNumber")], MARGIN = 1, FUN = sort) %>% t()
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
  # browser()
  ep = eulerian(g, start = start_point) %>% as.numeric()
  i = 0
  while (length(ep) != (nrow(arcs2)+1)& i<max(node2)) {
    # browser()
    i = i+1
    if (i %in% node2) {
      if (hasEulerianCycle(g)) {
        ep = eulerian(g, start = as.character(i)) %>% as.numeric()
      }
    }
  }   
  # if (ep[1] != start_point) {
  #   first_start = min(which(ep == start_point))
  #   # browser()
  #   ep = c(ep[first_start:length(ep)],ep[2:first_start])
  # }
  # browser()
  init_path = paths_list[[1]][[as.numeric(start_point)]]

  if (length(init_path)>0) {
    init_arcs= lapply(1:(length(init_path)-1), function(j){
      c(init_path[j],init_path[j+1], s.paths[init_path[j],init_path[j+1]])
    }) %>% do.call(rbind,.) %>% as.data.frame()
    
    # r_match = rbind(row.match(init_arcs[,c(2,1,3)],graph[,c("StartNodeNumber", "EndNodeNumber", "Lenght")], nomatch = F),
    #                 row.match(init_arcs[,1:3],graph[,c("StartNodeNumber", "EndNodeNumber", "Lenght")], nomatch = F)) %>%
    #   as.data.frame() %>% 
    #   sapply(max) %>% 
    #   as.numeric()
    # # browser()
    # temppp = graph[r_match,c("StartNodeNumber", "EndNodeNumber", "Lenght", "service")]
    # temppp$service = 0
    # 
    # arcs2 = rbind(arcs2,temppp)
    # rownames(arcs) = NULL
    init_arcs$service = 0
    names(init_arcs) = names(arcs2)
  }else{
    init_arcs = NULL
  }
  if (length(ep) != (nrow(arcs2)+1) | ep[1] != start_point) {
    # browser()
    return(rbind(init_arcs,arcs2))
  }
  
  inserted_path = ep
  
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
    if (length(direction)>1) {
      browser()
    }
    if (length(direction) == 0) {
      browser()
    }
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
  arcs_new = rbind(init_arcs, arcs_new)
  rownames(arcs_new) = NULL
  tail_removal = (((arcs_new$service != 0) %>% rev %>% cumsum) == 0) %>% max()
  # browser()
  arcs_new = arcs_new[1:(nrow(arcs_new)-tail_removal),c("StartNodeNumber", "EndNodeNumber", "Lenght", "service")]
  return(arcs_new)
}
