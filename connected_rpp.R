connected_rpp = function(arcs){
  # browser()
  rest = arcs[-1,c("StartNodeNumber", "EndNodeNumber")] 
  current = arcs[1,c("StartNodeNumber", "EndNodeNumber")]
  row_include = 1
  while(length(row_include)){
    row_include = ceiling(which(as.matrix(rest) %in% as.matrix(current))/2)
    current = rbind(current, rest[row_include,])
    rest = rest[-row_include,]
  }
  return(all(as.matrix(arcs[,c("StartNodeNumber", "EndNodeNumber")])  %in% as.matrix(current)))
}