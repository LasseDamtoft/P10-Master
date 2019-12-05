sub_graphs = function(arc_allocation, vehicles, graph){
  sub_graphs = lapply(unique(arc_allocation$vehicle), function(vehicle_id){
    cur_vehicle = vehicles[vehicles$ID == vehicle_id,]
    vehicle_service = graph 
    vehicle_service$service = 0
    vehicle_service$service[which(vehicle_service$EdgeNumber %in% 
                                    which(arc_allocation$vehicle == vehicle_id))] =1
    
    vehicle_service = vehicle_service %>% 
      mutate(service = service*ceiling(Width/cur_vehicle$spreadwidth))
    
    arcs = vehicle_service %>% 
      slice(sort(c(which(service>0),
                   which(service>1),
                   which(service>2),
                   which(service>3),
                   which(service>4)))) %>% 
      select(StartNodeNumber, EndNodeNumber, Lenght) 
    # browser()
    arcs$service = 1
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
    node_sets = lapply(1:length(connected_subs), function(j){
      connected_subs[[j]] %>% unlist %>%  as.numeric() %>% unique
    })
    connect_length = lapply(node_sets[2:length(node_sets)], function(nodes){
      s.paths[node_sets[[1]], nodes] %>% min()
    })
    browser()
    length(connected_subs)
  }) %>% do.call(rbind,.)
  # browser()
  return(max(sub_graphs))
}