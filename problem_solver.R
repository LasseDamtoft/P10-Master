problem_solver = function(example_vehicles, example_graph, pop_size = 3, N_mutations = 3){
  # browser()
  # set.seed(13)
  salt_cap = sum(example_vehicles$capacity_m3*1200)
  salt_usage = sum(example_graph$Lenght*example_graph$Width) * 17/1000
  if(salt_usage>salt_cap) {
    best_allocation = data.frame(arc =example_graph$EdgeNumber[example_graph$service == 1],
                                 vehicle = 0)
    return(list(best_allocation, best = 0))
  }
  # browser()
  best_allocation = data.frame(arc =example_graph$EdgeNumber[example_graph$service == 1],
                               vehicle = sample(example_vehicles$ID,size = length(example_graph$EdgeNumber[example_graph$service == 1]),replace = T))
  # best_allocation = arc_allocation = data.frame(arc = example_graph$EdgeNumber[example_graph$service == 1],
  #                                               vehicle = c(1,2,1,2,2,1,2,1,1,2,1,2,1,2))
  # browser()
  # population = best_allocation$vehicle %>% rbind()
  # browser()
  
  population = lapply(1:N_mutations, function(i){
    vehicle = sample(example_vehicles$ID,size = length(example_graph$EdgeNumber[example_graph$service == 1]),replace = T)
  }) %>% do.call(rbind,.)
  population = population %>% rbind(best_allocation$vehicle)
  population_values = lapply(1:nrow(population), function(j){
    arc_allocation2 = best_allocation
    arc_allocation2$vehicle = population[j,]
    routes = route_finding(arc_allocation2, example_vehicles, example_graph, N = 0)
    evaluate_routes(routes)
  }) %>% do.call(rbind,.)
  # browser()
  best = min(population_values)
  best_allocation$vehicle = population[which.min(population_values),]
  print(c(best_allocation$vehicle, best))
  old = best+1
  # browser()
  while (best < old) {
    old = best
    # browser()
    if (nrow(population) == 1) {
      arc_allocation = best_allocation
      arc_allocation$vehicle = population[1,]
      neighbourhood = lapply(1:nrow(best_allocation), function(arc_number){
        lapply(example_vehicles$ID[which(example_vehicles$ID != arc_allocation$vehicle[arc_number])], function(vehicle_number){
          # browser()
          arc_allocation$vehicle[arc_number] = vehicle_number
          
          if (feasibility(arc_allocation, example_vehicles, example_graph)){
            if (fixed_cost(arc_allocation, example_vehicles, example_graph)<best) {
              if (sub_graphs(arc_allocation, example_vehicles, example_graph) < .5) {
                if (fixed_cost2(arc_allocation, example_vehicles, example_graph)<best) {
                  arc_allocation$vehicle
                }else{NULL}
              }else{NULL}
            }else{NULL}
          }else{NULL}
        })%>% do.call(rbind,.)
      }) %>% do.call(rbind,.)
    }else{
      neighbourhood = lapply(1:nrow(population), function(k){
        arc_allocation = best_allocation
        arc_allocation$vehicle = population[k,]
        lapply(1:nrow(best_allocation), function(arc_number){
          lapply(example_vehicles$ID[which(example_vehicles$ID != arc_allocation$vehicle[arc_number])], function(vehicle_number){
            # browser()
            arc_allocation$vehicle[arc_number] = vehicle_number
            
            if (feasibility(arc_allocation, example_vehicles, example_graph)){
              if (fixed_cost(arc_allocation, example_vehicles, example_graph)<best) {
                if (sub_graphs(arc_allocation, example_vehicles, example_graph) < .5) {
                  if (fixed_cost2(arc_allocation, example_vehicles, example_graph)<best) {
                    arc_allocation$vehicle
                  }else{NULL}
                }else{NULL}
              }else{NULL}
            }else{NULL}
          })%>% do.call(rbind,.)
        }) %>% do.call(rbind,.)
      }) %>% do.call(rbind,.)
    }
    # browser()
    for(bla in 1:N_mutations) {
      # browser()
      addition = sample(example_vehicles$ID,size = length(example_graph$EdgeNumber[example_graph$service == 1]),replace = T)
      temp = best_allocation
      temp$vehicle = addition
      if (feasibility(temp, vehicles = example_vehicles, graph = example_graph)) {
        if (fixed_cost(temp, example_vehicles, example_graph)<best) {
          if (sub_graphs(temp, example_vehicles, example_graph) < .5) {
            if (fixed_cost2(temp, example_vehicles, example_graph)<best) {
              neighbourhood = rbind(neighbourhood, as.numeric(addition))
            }
          }
        }
      }
    }
    # browser()
    neighbourhood = neighbourhood[!duplicated(neighbourhood),] %>% rbind()
    # browser()
    if (length(nrow(neighbourhood))== 0) {
      return(list(best_allocation, best))
    }
    if (nrow(neighbourhood) < 2) {
      arc_allocation2 = best_allocation
      arc_allocation2$vehicle = neighbourhood[1,]
      routes = route_finding(arc_allocation2, example_vehicles, example_graph, N = 0)
      neigbour_results = evaluate_routes(routes) %>% rbind()
    }else{
      neigbour_results = lapply(1:nrow(neighbourhood), function(j){
        arc_allocation2 = best_allocation
        arc_allocation2$vehicle = neighbourhood[j,]
        routes = route_finding(arc_allocation2, example_vehicles, example_graph, N = 0)
        evaluate_routes(routes)
      }) %>% do.call(rbind,.)
    }# browser()
    if (max(population_values) > min(neigbour_results[order(neigbour_results)[1:pop_size]], na.rm = T)) {
      # browser()
      pop_temp = rbind(population,neighbourhood[order(neigbour_results)[1:pop_size],])
      pop_temp_values = c(population_values,neigbour_results[order(neigbour_results)[1:pop_size]] )
      population = pop_temp[order(pop_temp_values)[1:pop_size],] %>% rbind()
      population_values = pop_temp_values[order(pop_temp_values)[1:pop_size]]
      if (best > min(population_values)) {
        best_allocation$vehicle = population[which.min(population_values),]
        best = population_values[which.min(population_values)]
        print(c(best_allocation$vehicle, best))
      }
    }
  }
  return(list(best_allocation, best))
}