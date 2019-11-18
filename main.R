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

vehicle_id = 1

vehicle_service = example_graph
vehicle_service$service = 0
vehicle_service$service[which(vehicle_service$EdgeNumber %in% 
                                        which(arc_allocation$vehicle == vehicle_id))] =1

s.paths = short_paths(example_graph, distances = T)
paths_list = short_paths(example_graph, paths = T)

vehicle_service = vehicle_service %>% 
  mutate(service = service*ceiling(Width/example_vehicles$spreadwidth[2]))

arcs = vehicle_service %>% 
  slice(sort(c(which(service>0),
               which(service>1),
               which(service>2),
               which(service>3),
               which(service>4)))) %>% 
  select(StartNodeNumber, EndNodeNumber, Lenght) 


times = rep(0,100)
values = rep(100,100)
for (i in 1:100) {
  t = Sys.time()
  best_route = rpp_heuristic(arcs, 100)
  values[i] = best_route %>% summarise(sum(Lenght)) %>% as.numeric()
  times[i] = Sys.time() - t
}











