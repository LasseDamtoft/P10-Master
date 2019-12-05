fixed_cost2 = function(arc_allocation, vehicles, graph){
  fixed_cost = lapply(unique(arc_allocation$vehicle), function(vehicle_id){
    cur_vehicle = vehicles[vehicles$ID == vehicle_id,]
    # browser()
    vehicle_service = graph 
    vehicle_service$service = 0
    vehicle_service$service[which(vehicle_service$EdgeNumber %in% 
                                    which(arc_allocation$vehicle == vehicle_id))] =1
    
    vehicle_service = vehicle_service %>% 
      mutate(service = service*ceiling(Width/cur_vehicle$spreadwidth))
    
    # browser()
    # fixed_length = vehicle_service$service %*% vehicle_service$Lenght / cur_vehicle$service_speed*60
    arcs = vehicle_service %>% 
      slice(sort(c(which(service>0),
                   which(service>1),
                   which(service>2),
                   which(service>3),
                   which(service>4)))) %>% 
      select(StartNodeNumber, EndNodeNumber, Lenght) 
    # browser()
    arcs$service = 1
    
    # browser()
    if (!connected_rpp(arcs) ) {
      arcs = make_arcs_connected(arcs, graph)
      arcs = arcs %>% mutate(Timing = ifelse(arcs$service == 1,
                                      arcs$Lenght/cur_vehicle$service_speed*60,
                                      arcs$Lenght/cur_vehicle$deadhead_speed*60))
      sum(arcs$Timing)
    }else{
      fixed_length = vehicle_service$service %*% vehicle_service$Lenght / cur_vehicle$service_speed*60
    }
   
  }) %>% do.call(rbind,.)
  # browser()
  return(max(fixed_cost))
}