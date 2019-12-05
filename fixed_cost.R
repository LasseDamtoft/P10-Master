fixed_cost = function(arc_allocation, vehicles, graph){
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
    fixed_length = vehicle_service$service %*% vehicle_service$Lenght / cur_vehicle$service_speed*60

  }) %>% do.call(rbind,.)
  # browser()
  return(max(fixed_cost))
}