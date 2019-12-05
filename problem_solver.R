problem_solver = function(example_vehicles, example_graph, pop_size = 3, N_mutations = 3){
  # browser()
  best_allocation = data.frame(arc =example_graph$EdgeNumber[example_graph$service == 1],
                               vehicle = round(runif(length(example_graph$EdgeNumber[example_graph$service == 1]),
                                                     min = min(example_vehicles$ID)-.5,
                                                     max = max(example_vehicles$ID)+.5),0))
  # best_allocation = arc_allocation = data.frame(arc = example_graph$EdgeNumber[example_graph$service == 1],
  #                                               vehicle = c(1,2,1,2,2,1,2,1,1,2,1,2,1,2))
  
  population = best_allocation$vehicle %>% rbind()
  population = population[c(rep(1,pop_size)),]
  routes = route_finding(best_allocation, example_vehicles, example_graph, N = 0)
  best = evaluate_routes(routes)
  population_values = rep(best,pop_size)
  old = best+1
  # browser()
  while (best < old) {
    old = best 
    neighbourhood = lapply(1:nrow(population), function(k){
      arc_allocation = best_allocation
      arc_allocation$vehicle = population[k,]
      lapply(1:nrow(best_allocation), function(i){
        arc_allocation$vehicle[i] = example_vehicles$ID[which(example_vehicles$ID != arc_allocation$vehicle[i])] 
        if (feasibility(arc_allocation, example_vehicles, example_graph) &
            fixed_cost(arc_allocation, example_vehicles, example_graph)<best &
            fixed_cost2(arc_allocation, example_vehicles, example_graph)<best) {
          arc_allocation$vehicle
        }else{
          NULL
        }
      }) %>% do.call(rbind,.)
    }) %>% do.call(rbind,.)
      
    # browser()
    for(bla in 1:N_mutations) {
      # browser()
      addition = round(runif(nrow(best_allocation),
                             min = min(best_allocation$vehicle)-.5,
                             max = max(best_allocation$vehicle)+.5),0)
      temp = best_allocation
      temp$vehicle = addition
      if (feasibility(temp, vehicles = example_vehicles, graph = example_graph)&
          fixed_cost(temp, vehicles = example_vehicles, graph = example_graph)) {
        neighbourhood = rbind(neighbourhood, as.numeric(addition))
      }
    }
    # browser()
    neighbourhood = neighbourhood[!duplicated(neighbourhood),]

    neigbour_results = lapply(1:nrow(neighbourhood), function(j){
      arc_allocation2 = best_allocation
      arc_allocation2$vehicle = neighbourhood[j,]
      routes = route_finding(arc_allocation2, example_vehicles, example_graph, N = 0)
      evaluate_routes(routes)
    }) %>% do.call(rbind,.)
    if (any(population_values > neigbour_results[order(neigbour_results)[1:pop_size]])) {
      # browser()
      pop_temp = rbind(population,neighbourhood[order(neigbour_results)[1:pop_size],])
      pop_temp_values = c(population_values,neigbour_results[order(neigbour_results)[1:pop_size]] )
      population = pop_temp[order(pop_temp_values)[1:pop_size],]
      population_values = pop_temp_values[order(pop_temp_values)[1:pop_size]]
      if (best > min(population_values)) {
        best_allocation$vehicle = population[which.min(population_values),]
        best = population_values[which.min(population_values)]
      }
    }
  }
  return(list(best_allocation, best))
}