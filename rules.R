## Rules
flip = function(arcs){
  arcs[nrow(arcs):1,] %>% return()
}
swap_half = function(arcs){
  arcs[c((ceiling(nrow(arcs)/2)+1):nrow(arcs),1:ceiling(nrow(arcs)/2)),] %>% return()
}
swap_half_flip = function(arcs){
  arcs[c((ceiling(nrow(arcs)/2)+1):nrow(arcs),1:ceiling(nrow(arcs)/2)),] %>% flip %>% return()
}
swap_half_flip_first = function(arcs){
  temp = arcs[c((ceiling(nrow(arcs)/2)+1):nrow(arcs),1:ceiling(nrow(arcs)/2)),]
  temp[1:ceiling(nrow(temp)/2),] = temp[1:ceiling(nrow(temp)/2),] %>% flip 
  temp %>% return()
}
swap_half_flip_second = function(arcs){
  temp = arcs[c((ceiling(nrow(arcs)/2)+1):nrow(arcs),1:ceiling(nrow(arcs)/2)),]
  temp[(ceiling(nrow(arcs)/2)+1):nrow(arcs),] = temp[(ceiling(nrow(arcs)/2)+1):nrow(arcs),] %>% flip 
  temp %>% return()
}
rule = function(arcs, rule){
  if (rule == 1) {
    # browser()
    temp = arcs %>% flip()
  }
  if (rule == 2) {
    temp = arcs %>% swap_half() 
  }
  if (rule == 3) {
    temp = arcs %>% swap_half_flip() 
  }
  if (rule == 4) {
    temp = arcs %>% swap_half_flip_first() 
  }
  if (rule == 5) {
    temp = arcs %>% swap_half_flip_second() 
  }
  return(temp)
}

try_rule = function(best_route){
  for (i in 1:5) {
    temp = rule(best_route, i) %>% arcs_to_route()
    temp2 = best_route %>% arcs_to_route()
    if ((temp$Timing %>% sum)<(best_route$Timing %>% sum)) {
      best_route = temp
    }
  }
  return(best_route)
}













