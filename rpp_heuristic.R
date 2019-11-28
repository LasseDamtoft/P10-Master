rpp_heuristic = function(arcs, N = 100){
  browser()
  # if (connected_rpp(arcs)) {
  #   connected_rpp_solver()
  # }
  arcs_test = arcs
  best_route = arcs_to_route(arcs_test)
  best_route_value = best_route %>%  summarise(sum(Lenght))

  dist = lapply(1:nrow(arcs_test), function(arc){
    c(s.paths[1,arcs_test[arc,1]],s.paths[1,arcs_test[arc,2]])
  }) %>% do.call(rbind,.)
  arcs_rules = cbind(arcs_test,dist, rowMeans(dist))
  names(arcs_rules)[5:7] = c('first_node', 'second_node', 'node_mean')
  r1 = arcs_test[order(arcs_rules$first_node),] %>% arcs_to_route() 
  if (r1 %>%  summarise(sum(Lenght)) < best_route_value) {
    best_route = r1
    best_route_value = best_route %>%  summarise(sum(Lenght))
  }
  r2 = arcs_test[order(arcs_rules$second_node),] %>% arcs_to_route()
  if (r2 %>%  summarise(sum(Lenght)) < best_route_value) {
    best_route = r2
    best_route_value = best_route %>%  summarise(sum(Lenght))
  }
  r3 = arcs_test[order(arcs_rules$node_mean),] %>% arcs_to_route() 
  if (r3 %>%  summarise(sum(Lenght)) < best_route_value) {
    best_route = r3
    best_route_value = best_route %>%  summarise(sum(Lenght))
  }
  r4 = arcs_test[sort(arcs_rules$first_node, decreasing = T),] %>% arcs_to_route()
  if (r4 %>%  summarise(sum(Lenght)) < best_route_value) {
    best_route = r4
    best_route_value = best_route %>%  summarise(sum(Lenght))
  }
  r5 = arcs_test[order(arcs_rules$second_node, decreasing = T),] %>% arcs_to_route()
  if (r5 %>%  summarise(sum(Lenght)) < best_route_value) {
    best_route = r5
    best_route_value = best_route %>%  summarise(sum(Lenght))
  }
  r6 = arcs_test[order(arcs_rules$node_mean, decreasing = T),] %>% arcs_to_route() 
  if (r6 %>%  summarise(sum(Lenght)) < best_route_value) {
    best_route = r6
    best_route_value = best_route %>%  summarise(sum(Lenght))
  }
  
  i = 1
  while(i<N) {
    # print(i)
    # browser()
    arcs_test = arcs %>% sample_n(nrow(arcs))
    print(arcs_test)
    current_route = arcs_test  %>% 
      arcs_to_route()
    current_route_value = current_route  %>% summarise(sum(Lenght))
    if (current_route_value < best_route_value) {
      best_route_value = current_route_value
      best_route = current_route
      i = 1
    }else{
      i = i + 1
    }
  }
  rownames(best_route) = NULL
  return(best_route)
}