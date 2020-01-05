library(dplyr)  
library(graph)
library(eulerian)
library(mcpm)
source('graph_loader.R')
source('vehicle_loader.R')
source('short_path.R')
source('example.R')
source('arcs_to_route.R')
source('direction_search.R')
source('rpp_heuristic.R')
source('route_finding.R')
source("connected_rpp.R")
source("rules.R")
source('euleran_path_from_connected.R')
source("add_eulerian_need_paths.R")
source("add_eulerian_need_cycle.R")
source('euleran_cycle_from_connected.R')
source('make_arcs_connected.R')
source('feasibility.R')
source('fixed_cost.R')
source('helper_functions.R')
source('problem_solver.R')
source('sub_graphs2.R')
source('sub_graphs.R')
source('fixed_cost2.R')
file_names = list.files('CARP graphs/')

test_instances = lapply(file_names, function(i){
  graph_loader(path = paste('CARP graphs/',i, sep =''), seed = round(runif(1,1,1000)))
})

sizes = lapply(test_instances, function(i){
  nrow(i)
}) %>% do.call(rbind,.) 

small = test_instances[order(sizes)[1:29]]
medium = test_instances[order(sizes)[30:59]]
large = test_instances[order(sizes)[60:88]]

results = mclapply(small, mc.cores = 50, FUN = function(i){
  s.paths = short_paths( i[i$service == 1,], distances = T)
  paths_list = short_paths( i[i$service == 1,], paths = T)
  
  tic = Sys.time()
  solution = problem_solver(example_vehicles, i, pop_size = 6, N_mutations = 5)
  cbind(arcs = t(solution[[1]]$vehicle),
        objective = solution[[2]],
        time = difftime(Sys.time(), tic, units = 'mins'))
}) %>% do.call(rbind,.)

vehicles_1 = example_vehicles[c(1,8),]
vehicles_2 = example_vehicles[c(1,5,8),]
vehicles_3 = example_vehicles[c(1,5,8,9),]
