monte_carlo = function(f, a = 0, b = 5, n, ...) {
  opt = optimize(f, interval = c(a, b), maximum = TRUE)
  
  bot = 0
  top = opt$objective
  rect_area = (b - a)*(top - bot)
  
  x = runif(n, a, b)
  y = runif(n, bot, top)
  x_eval = sapply(x, f)
  hit = (y <= x_eval) & (x_eval >= 0)
  
  return(rect_area * mean(hit))
}


f = function(x) sqrt(x) * sin(x) * cos(x) + 1

N = 100

library(batchtools)

reg = makeRegistry(file.dir = NA, seed = 1)

batchMap(fun = monte_carlo, n = rep(10^6, 100), more.args = list(f = f))

# submitJobs()
# 
# getStatus()
# res = reduceResultsList()
# 

