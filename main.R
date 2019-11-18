library(dplyr)
source('graph_loader.R')
source('vehicle_loader.R')
source('short_path.R')
source('example.R')
source('arcs_to_route.R')
source('direction_search.R')

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

arcs_to_route(arcs) %>% summarise(sum(Lenght))

arcs %>% summarise(sum(Lenght))
arcs_test = arcs %>% sample_n(nrow(arcs))
best_route_value = arcs_to_route(arcs_test) %>% summarise(sum(Lenght))
best_route = arcs_to_route(arcs_test)
t = Sys.time()
for (i in 1:1000) {
  print(i)
  arcs_test = arcs %>% sample_n(nrow(arcs))
  current_route = arcs_test  %>% 
    arcs_to_route()
  current_route_value = current_route  %>% summarise(sum(Lenght))
  if (current_route_value < best_route_value) {
    best_route_value = current_route_value
    best_route = arcs_test
  }
}
Sys.time() - t











