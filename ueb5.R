library(smoof)
rosenbrock = makeRosenbrockFunction(dimensions = 2L)
rosenbrock = addLoggingWrapper(rosenbrock, logg.x = TRUE, logg.y = TRUE)

booth = makeBoothFunction()
booth = addLoggingWrapper(booth, logg.x = TRUE, logg.y = TRUE)


set.seed(1)
maxit = 10^4
control = list(maxit = maxit, reltol = .Machine$double.xmin)
p = function() runif(1, min = -5, max = 5)
optim.nelder = function(data, ...) {
  optim(c(p(), p()), fn = data, method = "Nelder-Mead", control = control)
  return(getLoggedValues(data))
}
optim.bfgs = function(data, ...) {
  optim(c(p(), p()), fn = data, method = "BFGS", control = control)
  return(getLoggedValues(data))
}
optim.sann = function(data, ...) {
  optim(c(p(), p()), fn = data, method = "SANN", control = control)
  return(getLoggedValues(data))
}

library(batchtools)
unlink("registry", recursive = TRUE, force = TRUE)
reg = makeExperimentRegistry()

batchtools::addProblem(name = "rosenbrock", data = rosenbrock)
batchtools::addProblem(name = "booth", data = booth)

batchtools::addAlgorithm("nelder", fun = function(job, data, instance) optim.nelder(data))
batchtools::addAlgorithm("bfgs", fun = function(job, data, instance) optim.bfgs(data))
batchtools::addAlgorithm("sann", fun = function(job, data, instance) optim.sann(data))

addExperiments(repls = 30)


