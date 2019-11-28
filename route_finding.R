source('rpp_heuristic.R')
route_finding = function(arc_allocation, vehicles, graph, N = 1000){
  route_finding = lapply(1:2, function(vehicle_id){
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
    best_route = rpp_heuristic(arcs, N)
    
    best_route %>% mutate(Timing = ifelse(best_route$service == 1,
                                           best_route$Lenght/cur_vehicle$service_speed*60,
                                            best_route$Lenght/cur_vehicle$deadhead_speed*60))
  })
  return(route_finding)
}
