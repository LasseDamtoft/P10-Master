connected_rpp = function(arcs){
  # browser()
  rest = arcs[,c("StartNodeNumber", "EndNodeNumber")] 
  current = arcs[1,c("StartNodeNumber", "EndNodeNumber")]
  row_include = 1
  while(length(row_include) != 0 ){
    rest = rest[-row_include,]
    row_include = (which(as.matrix(rest) %in% as.matrix(current)) %% nrow(rest))
    row_include[row_include == 0] = nrow(rest)
    row_include = row_include %>% unique()
    current = rbind(current, rest[row_include,])
  } 
  return(all(as.matrix(arcs[,c("StartNodeNumber", "EndNodeNumber")])  %in% as.matrix(current)))
}