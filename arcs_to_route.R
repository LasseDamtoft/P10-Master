library(dplyr)
library(prodlim)
arcs_to_route = function(arcs){

  # browser()

  if (arcs[1,1] != 1) {
    # browser()
    inserted_path = paths_list[[1]][[arcs[1,1]]]
    arcs_part= lapply(1:(length(inserted_path)-1), function(j){
      c(inserted_path[j],inserted_path[j+1], s.paths[inserted_path[j],inserted_path[j+1]])
    }) %>% do.call(rbind,.) %>% as.data.frame()
    arcs_part$service = 0
    names(arcs_part) = names(arcs)
    r_match = rbind(row.match(arcs_part[,c(2,1,3)],arcs[,1:3], nomatch = F),
                    row.match(arcs_part[,1:3],arcs[,1:3], nomatch = F)) %>%
      as.data.frame() %>% 
      sapply(max) %>% 
      as.numeric()
    
    if (any(r_match != 0)) {
      arcs_part$service[which(r_match > 0)] = 1
      r_match_int = r_match[which(r_match != 0)]
      a_2 = arcs[-(r_match_int),]
      arcs = rbind(arcs_part,
                   a_2)
    }else{
      arcs= rbind(arcs_part, arcs)
    }
  }
  arcs_new = lapply(1:(nrow(arcs)-1), function(i){
    # browser()
    if (i == 1) {
      arcs_new  =arcs[1,]
      
    }
    # browser()
    if (arcs[i,2] == arcs[i+1,1]) {
      arcs_new= arcs[i,]
    }else{
      # browser()
      inserted_path = paths_list[[arcs[i,2]]][[arcs[i+1,1]]]
      arcs_part= lapply(1:(length(inserted_path)-1), function(j){
        c(inserted_path[j],inserted_path[j+1], s.paths[inserted_path[j],inserted_path[j+1]])
      }) %>% do.call(rbind,.) %>% as.data.frame()
      arcs_part$service = 0
      names(arcs_part) = names(arcs)
      
      r_match = rbind(row.match(arcs_part[,c(2,1,3)],arcs[(i+1):nrow(arcs),1:3], nomatch = F),
                      row.match(arcs_part[,1:3],arcs[(i+1):nrow(arcs),1:3], nomatch = F)) %>%
        as.data.frame() %>% 
        sapply(max) %>% 
        as.numeric()
      if (any(r_match != 0)) {
        arcs_part$service[which(r_match > 0)] = 1
        r_match_int = r_match[which(r_match != 0)]
        a_2 = arcs[-(i+r_match_int),]
        if (i >= nrow(a_2)) {
          arcs <<- rbind(a_2[1:(i),],
                         arcs_part[which(r_match != 0),])
        }else{
          arcs <<- rbind(a_2[1:(i),],
                         arcs_part[which(r_match != 0),],
                         a_2[(i+1):nrow(a_2),])
        }
      }
      arcs_new= rbind(arcs[i,],arcs_part[which(cumsum(r_match) == 0),])
    }
    if ((i+1) ==nrow(arcs)) {
      arcs_new = rbind(arcs_new, arcs[i+1,])
    }
    arcs_new
  }) %>% do.call(rbind,.)
  return(arcs_new)
}
