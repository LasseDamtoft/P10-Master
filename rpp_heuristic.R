rpp_heuristic = function(arcs, N = 1000){
  arcs_test = arcs %>% sample_n(nrow(arcs))
  best_route_value = arcs_to_route(arcs_test) %>% summarise(sum(Lenght))
  best_route = arcs_to_route(arcs_test)
  for (i in 1:N) {
    # print(i)
    arcs_test = arcs %>% sample_n(nrow(arcs))
    current_route = arcs_test  %>% 
      arcs_to_route()
    current_route_value = current_route  %>% summarise(sum(Lenght))
    if (current_route_value < best_route_value) {
      best_route_value = current_route_value
      best_route = current_route
    }
  }
  return(best_route)
}