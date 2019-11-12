library(dplyr)

source('graph_loader.R')
source('vehicle_loader.R')
graph = graph_loader()
vehicles = vehicle_loader()

graph %>% head
vehicles %>% head

