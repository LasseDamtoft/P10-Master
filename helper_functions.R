evaluate_routes = function(routes){
  route_time = lapply(routes, function(route){
    route$Timing %>% sum
  })
  return(route_time[[which.max(route_time)]])
}