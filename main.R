library(dplyr)
source('graph_loader.R')
source('vehicle_loader.R')
source('short_path.R')
source('example.R')
source('arcs_to_route.R')
source('direction_search.R')
source('rpp_heuristic.R')
source('route_finding.R')

s.paths = short_paths(example_graph, distances = T)
paths_list = short_paths(example_graph, paths = T)


arc_allocation = data.frame(arc = example_graph$EdgeNumber[example_graph$service == 1],
                            vehicle = c(1,2,1,2,2,1,2,1,1,2,1,2,1,2))


connected_rpp_solver(example_graph)
routes = route_finding(arc_allocation, example_vehicles, example_graph, N = 100)
graph_loader()
route_time = lapply(routes, function(route){
  route$Timing %>% sum
}) 
best = route_time[[which.max(route_time)]]
old = best+1
tic = Sys.time()
while (best < old) {
  old = best 
  neighbourhood = lapply(1:nrow(arc_allocation), function(i){
    arc_allocation$vehicle[i] = example_vehicles$ID[which(example_vehicles$ID != arc_allocation$vehicle[i])] 
    arc_allocation
  })
  neigbour_results = lapply(neighbourhood, function(arc_allocation){
    routes = route_finding(arc_allocation, example_vehicles, example_graph, N = 100)
    
    route_time = lapply(routes, function(route){
      route$Timing %>% sum
    }) 
    route_time[[which.max(route_time)]]
  })
  if (best > neigbour_results[[which.min(neigbour_results)]]) {
    arc_allocation = neighbourhood[[which.min(neigbour_results)]]
    best = neigbour_results[[which.min(neigbour_results)]]
  }
}
Sys.time() - tic 

