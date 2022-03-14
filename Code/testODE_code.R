library(deSolve)
library(scatterplot3d)

testODE <- function(time_space, initial_contamination, parameters){
  with(
    as.list(c(initial_contamination, parameters)),{
      dContamination <- -d*exp(-g*time_space)*Contamination
      return(list(dContamination))
    }
  )
}

parameters <- c(C = -8/3, d = -10, g =  28)
Y=c(y=1200)
times <- seq(0, 6, by = 0.01)
initial_contamination=c(Contamination=1200) 
out <- ode(initial_contamination, times, testODE, parameters, method = "radau",atol = 1e-4, rtol = 1e-4)

plot(out)


library(deSolve)
library(scatterplot3d)
dyn.load("Code/testODE.so")

Y <-c(y1=initial_contamination) ;
out <- ode(1200, times, func = "derivs", parms = parameters,
           dllname = "testODE", initfunc = "initmod")


plot(out)



library(deSolve)

testODE <- function(t, y, parameters){
  with(
    as.list(c(y, parameters)),{
      dContamination <- (1-Contamination/C)*r -d * exp(-g * t) * Contamination
      return(list(dContamination))
    }
  )
}

system("R CMD SHLIB Code/testODE.c")
dyn.load("Code/testODE.so")

parameters <- c(C = 2000, d = 100, g =  50, r=50)
initial_contamination<- c(Contamination = 1200)
times      <- seq(0, 6, by = 0.01)

out1 <- ode(initial_contamination, times, testODE,
            parms = parameters, method = "radau", atol = 1e-4, rtol = 1e-4)
out2 <- ode(initial_contamination, times, func = "derivs", dllname = "testODE", initfunc = "initmod",
            parms = parameters, method = "radau", atol = 1e-4, rtol = 1e-4)

plot(out1, out2)          # no visible difference
summary(out1 - out2)      # differences should be (close to) zero

dyn.unload("Code/testODE.so") # always unload before editing .c file !!



