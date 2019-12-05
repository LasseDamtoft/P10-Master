trials = expand.grid(rep(list(1L:2L), nrow(arc_allocation)))
tic = Sys.time()
brute = lapply(1:nrow(trials), function(arc_allocation2){
  # browser()
  arc_allocation$vehicle = t(trials[arc_allocation2,]) %>% as.numeric()
  print(arc_allocation)
  routes = route_finding(arc_allocation, example_vehicles, example_graph, N = 0)
  route_time = lapply(routes, function(route){
    route$Timing %>% sum
  })
  route_time[[which.max(route_time)]]
}) %>% do.call(rbind,.)
Sys.time() - tic
