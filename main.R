library(dplyr)
source('graph_loader.R')
source('vehicle_loader.R')
source('short_path.R')
source('example.R')
source('arcs_to_route.R')
source('direction_search.R')
source('rpp_heuristic.R')

arc_allocation = data.frame(arc = example_graph$EdgeNumber[example_graph$service == 1],
                            vehicle = c(1,2,1,2,2,1,2,1,1,2,1,2,1,2))
s.paths = short_paths(example_graph, distances = T)
paths_list = short_paths(example_graph, paths = T)

route_finding = lapply(1:2, function(vehicle_id){
  cur_vehicle = example_vehicles[example_vehicles$ID == vehicle_id,]
  vehicle_service = example_graph
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
  
  arcs$service = 1
  best_route = rpp_heuristic(arcs, 1000)

  best_route %>% mutate(timeing = ifelse(best_route$service == 1,
                                          best_route$Lenght/cur_vehicle$service_speed*60,
                                          best_route$Lenght/cur_vehicle$deadhead_speed*60))
})

route_time = lapply(route_finding, function(route){
  route$timeing %>% sum
})

temp = route_finding[[which.max(route_time)]][route_finding[[which.max(route_time)]]$service == 1,]

vehicle_service = example_graph

temp2 = vehicle_service[which(vehicle_service$EdgeNumber %in% 
                                which(arc_allocation$vehicle == which.max(route_time))),] 
r_match = rbind(row.match(temp[,c(2,1)],temp2[,2:3], nomatch = F),
                row.match(temp[,1:2],temp2[,2:3], nomatch = F)) %>%
  as.data.frame() %>% 
  sapply(max) %>% 
  as.numeric()

temp$arc_ID = temp2[r_match,1]


arc_times = lapply(unique(temp$arc_ID), function(arc_inde){
  sum(temp$timeing[temp$arc_ID == arc_inde])
}) %>% do.call(rbind,.) %>% cbind(unique(temp$arc_ID))

most_expensive_edge = arc_times[which.max(arc_times[,1]),2]

temp3 = sort(example_vehicles$ID)

arc_allocation[most_expensive_edge,2] =temp3[temp3 !=arc_allocation[most_expensive_edge,2]]






