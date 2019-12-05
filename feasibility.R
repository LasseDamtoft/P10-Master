feasibility = function(arc_allocation, vehicles, graph){
  feas = lapply(unique(arc_allocation$vehicle), function(vehicle_id){
    cur_vehicle = vehicles[vehicles$ID == vehicle_id,]
    # browser()
    vehicle_service = graph 
    vehicle_service$service = 0
    vehicle_service$service[which(vehicle_service$EdgeNumber %in% 
                                    which(arc_allocation$vehicle == vehicle_id))] =1
    
    vehicle_service = vehicle_service %>% 
      mutate(service = service*ceiling(Width/cur_vehicle$spreadwidth))


    salt_usage = vehicle_service$service %*% vehicle_service$Lenght * cur_vehicle$spreadwidth * 17/1000
    salt_capacity = cur_vehicle$capacity_m3 * 1200 ## 1.2 ton pr cubic meter from COWI
    salt_usage < salt_capacity
  }) %>% do.call(rbind,.)
  # browser()
  return(all(feas))
}