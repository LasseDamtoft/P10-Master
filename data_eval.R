load("~/Gitprojects/P10-Master/rstudio-export (4)/res_small2_1.RData")
load("~/Gitprojects/P10-Master/rstudio-export (4)/res_small2_2.RData")
load("~/Gitprojects/P10-Master/rstudio-export (4)/res_small2_3.RData")

lapply(results_2, function(x){
  c(min(x[,ncol(x)-1]),median(x[,ncol(x)-1]),max(x[,ncol(x)-1]))
})



file_names = list.files('CARP graphs/')

test_instances = lapply(file_names, function(i){
  graph_loader(path = paste('CARP graphs/',i, sep =''), seed = round(runif(1,1,1000)))
})

sizes = lapply(test_instances, function(i){
  nrow(i)
}) %>% do.call(rbind,.) 
small = test_instances[order(sizes)[1:4]]

lapply(small, function(x){
  c(edges = nrow(x), 
    nodes = sum(!duplicated(c(x$StartNodeNumber,x$EndNodeNumber))),
    service = sum(x$service), 
    length_service = sum(x$service%*%x$Lenght)
    )
})%>% do.call(rbind,.) 
