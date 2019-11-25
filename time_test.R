plot_value = NULL
plot_time = NULL
for (heuristic_N in 1:200) {
  sample_size = 100
  times = rep(0,sample_size)
  values = rep(100,sample_size)
  print(heuristic_N)
  for (i in 1:sample_size) {
    # print(i)
    t = Sys.time()
    best_route = rpp_heuristic(arcs, heuristic_N)
    values[i] = best_route %>% summarise(sum(Lenght)) %>% as.numeric()
    times[i] = Sys.time() - t
  }
  plot_value = c(plot_value, sum(values == 25)/sample_size)
  plot_time = c(plot_time, mean(times))
  p = plotly::plot_ly(y =~plot_value,
                      type = 'scatter',
                      mode = 'lines+markers',
                      name = 'Percentage of outputs optimal') %>% 
    plotly::add_trace(y =~ plot_time,
                      name = 'mean time per heuristic (Secs)')
  print(p)
}