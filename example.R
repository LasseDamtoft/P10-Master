source('graph_loader.R')
source('vehicle_loader.R')
graph = graph_loader()
vehicles = vehicle_loader()
example_graph = graph %>% 
  slice(1:15)
example_vehicles = vehicles %>%
  slice(c(1,13))

example_graph$service[2:15] = 1
example_graph$StartNodeNumber = c(0,1,1,1,2,3,3,4,5,5,6,6,7,7,8)
example_graph$EndNodeNumber = c(1,2,3,4,8,6,9,6,6,7,7,10,9,10,9)
example_graph$Lenght = c(0,2,1.5,3,4,7,5,1,0.5,0.25,1,1.5,3,0.5,6)
example_graph$Width = c(3,3,3,3,6,6,6,6,6,3,6,3,3,6,3)

example_vehicles$spreadwidth = c(6,3)
example_vehicles$ID = c(2,1)
example_vehicles$service_speed = c(30,15)
example_vehicles$deadhead_speed = c(60,30)
example_graph$EdgeNumber = example_graph %>% rownames() %>% as.numeric()
example_graph$EdgeNumber = example_graph$EdgeNumber-1
