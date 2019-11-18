source('arcs_to_route.R')
library(dplyr)
direction_search = function(arcs){
  arcs_test_new = aa = arcs_test = arcs
  current_route_value = arcs_to_route(arcs_test)  %>% summarise(sum(Lenght))
  j=1
  while (j <= nrow(arcs_test_new)) {
    arcs_test_new[j,c(1,2)] = arcs_test[j,c(2,1)] %>% as.numeric()
    new_value = arcs_to_route(arcs_test_new) %>% summarise(sum(Lenght)) %>% as.numeric()
    # print(new_value < current_route_value)
    if (new_value < current_route_value) {
      arcs_test = arcs_test_new
      current_route_value = new_value
      j = 1
    }else{
      arcs_test_new = arcs_test
      j = j + 1
    }
  }
  return(arcs_test)
}
