arcs_to_route = function(arcs){
  arcs_new = lapply(1:(nrow(arcs)-1), function(i){
    if (i == 1) {
      arcs_new  =arcs[1,]
    }
    if (arcs[i,2] == arcs[i+1,1]) {
      arcs_new= rbind(arcs_new[i+1])
      next
    }else{
      inserted_path = paths_list[[arcs[i,2]]][[arcs[i+1,1]]]
      arcs_part= lapply(1:(length(inserted_path)-1), function(j){
        c(inserted_path[j],inserted_path[j+1], s.paths[j,j+1])
      }) %>% do.call(rbind,.) %>% as.data.frame()
      names(arcs_part) = names(arcs)
      arcs_new= rbind(arcs[i,],arcs_part)
    }
    arcs_new
  }) %>% do.call(rbind,.)
  return(arcs_new)
}