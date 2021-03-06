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
s.paths = short_paths( example_graph[example_graph$service == 1,], distances = T)
paths_list = short_paths( example_graph[example_graph$service == 1,], paths = T)
arc_allocation = data.frame(arc = example_graph$EdgeNumber[example_graph$service == 1],
                            vehicle = c(1,8,8,1,1,1,1,1,8,8,8,8,1,1))

tt = route_finding(arc_allocation, example_vehicles[c(1,8),], example_graph)
evaluate_routes(tt)
# euleran_path_from_connected(example_graph[-1,c(2,3,4,7)], graph = example_graph)


# 
# test_graph = graph_loader()
# test_graph[,1:3] = test_graph[,1:3]+1
# connected_rpp(test_graph)
# tic = Sys.time()
# s.paths_test = short_paths(test_graph, distances = T)
# paths_list_test = short_paths(test_graph, paths = T)
# Sys.time() - tic
# 
# 
# example_vehicles
# test_graph$EdgeNumber[example_graph$service == 1] %>% length()
# arc_allocation = data.frame(arc = test_graph$EdgeNumber[example_graph$service == 1],
#                             vehicle = sample(c(1,2), 1049, replace = T))
# 
# s.paths = s.paths_test
# paths_list = paths_list_test

tic= Sys.time()
solution = problem_solver(example_vehicles[c(1,8),], example_graph, pop_size = 5, N_mutations = 5)
Sys.time() - tic 

ss =solution[[1]] %>% route_finding(vehicles = example_vehicles[c(1,8),], example_graph)
evaluate_routes(ss)
