sub_graphs2 = function(arcs, graph){
  
  arcs2  = arcs
  connected_subs = list()
  i=1
  # browser()
  while(nrow(arcs2) != 0){
    rest = arcs2[,c("StartNodeNumber", "EndNodeNumber")] 
    rest2 = arcs
    current = arcs2[1,c("StartNodeNumber", "EndNodeNumber")]
    current2 = arcs[1,]
    row_include = 1
    while(length(row_include) != 0 ){
      rest = rest[-row_include,]
      rest2 = rest2[-row_include,]
      row_include = (which(as.matrix(rest) %in% as.matrix(current)) %% nrow(rest))
      row_include[row_include == 0] = nrow(rest)
      row_include = row_include %>% unique()
      current = rbind(current, rest[row_include,])
      current2 = rbind(current2, rest2[row_include,])
      
    } 
    connected_subs[[i]] = current2
    i=i+1
    arcs2 = rest
    arcs = rest2
  }
  
  # browser()
  return(connected_subs)
}