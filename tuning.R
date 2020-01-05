load('results2.RData')
load('results3.RData')
library(dplyr)

data_obj = lapply(results2, function(pop){
  lapply(pop, function(x){
    median(x[,15])
  })%>% do.call(cbind,.)
}) %>% do.call(rbind,.) %>% unlist %>% matrix(10,10)

data_time = lapply(results2, function(pop){
  lapply(pop, function(x){
    median(x[,16])
  })%>% do.call(cbind,.)
}) %>% do.call(rbind,.) %>% unlist %>% matrix(10,10)



data %>% str()
plotly::plot_ly(z = data_obj, type = 'heatmap')
plotly::plot_ly(z = data_time, type = 'heatmap')
