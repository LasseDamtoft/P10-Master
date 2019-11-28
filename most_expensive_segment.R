
temp = route_finding[[which.max(route_time)]][route_finding[[which.max(route_time)]]$service == 1,]

vehicle_service = example_graph

temp2 = vehicle_service[which(vehicle_service$EdgeNumber %in% 
                                which(arc_allocation$vehicle == which.max(route_time))),] 
r_match = rbind(row.match(temp[,c(2,1)],temp2[,2:3], nomatch = F),
                row.match(temp[,1:2],temp2[,2:3], nomatch = F)) %>%
  as.data.frame() %>% 
  sapply(max) %>% 
  as.numeric()

temp$arc_ID = temp2[r_match,1]


arc_times = lapply(unique(temp$arc_ID), function(arc_inde){
  sum(temp$timeing[temp$arc_ID == arc_inde])
}) %>% do.call(rbind,.) %>% cbind(unique(temp$arc_ID))

most_expensive_edge = arc_times[which.max(arc_times[,1]),2]

temp3 = sort(example_vehicles$ID)
arc_allocation[most_expensive_edge,2] =temp3[temp3 !=arc_allocation[most_expensive_edge,2]]
