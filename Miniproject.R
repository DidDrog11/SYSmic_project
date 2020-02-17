library("deSolve") ##Package to numerically solve differential equations
library("tidyverse")
library("patchwork")

##Setting model parameters
R_0 <- 12 ##The number of infections caused by a single introduction of a case into a susceptible population
N <- 10000 ##The population at the beginning of the model
I <- 12  ##The number of introduced infections
tau <- 9 ##The number of days an individual is infectious for

gamma = gamma_val(tau) ##The rate of recovery from infection per unit time is termed gamma
beta = beta_val(R_0, N, gamma) ##The effective infectious contact rate per unit time is termed beta

par_1 <- c(
  gamma,
  beta
)

init <- c(
  S = N-I,  ##The number of susceptible individuals at time point t=0 is S
  I =   I,  ##The number of infectious individuals introduced at time point t=0 is I
  R =   0   ##The number of recovered individuals at time point t=0 is R
)

##These models can be expanded upon dependent on the disease of interest. Other examples includes SEIR

time <- seq(0, 50) ##This represents the time step between T1 and T2

##First model
SIR_1 <- as.data.frame(ode(y = init,
                           times = time,
                           func = sir_equations,
                           parms = par_1
                           ))

plot_1 <- plot_graph(SIR_1)


##Adjusting parameters to upperlimit of R0
R_0 <- 18 ##This is the upper limit from the published literature
N <- 10000 ##No change
I <- 12  ##No change
tau <- 9 ##No change

gamma = gamma_val(tau) ##No change
beta = beta_val(R_0, N, gamma) ##This will change based on the value of R0

par_2 <- c(
  gamma,
  beta
)

time <- seq(0, 50) ##No change

##Running the model with these new parameters
SIR_2 <- as.data.frame(ode(y = init,
                           times = time,
                           func = sir_equations,
                           parms = par_2
))

plot_2 <- plot_graph(SIR_2)

##Adjusting the model to reflect a lower limit for infectious period
R_0 <- 12 ##This is the upper limit from the published literature
N <- 10000 ##No change
I <- 12  ##No change
tau <- 7 ##No change

gamma = gamma_val(tau) ##No change
beta = beta_val(R_0, N, gamma) ##This will change based on the value of R0

par_3 <- c(
  gamma,
  beta
)

time <- seq(0, 50) ##No change

##Running the model with these new parameters
SIR_3 <- as.data.frame(ode(y = init,
                           times = time,
                           func = sir_equations,
                           parms = par_3
))

plot_3 <- plot_graph(SIR_3)

##Adjusting the model to reflect an upper limit for infectious period
R_0 <- 12 ##This is the upper limit from the published literature
N <- 10000 ##No change
I <- 12  ##No change
tau <- 12  ##No change

gamma = gamma_val(tau) ##No change
beta = beta_val(R_0, N, gamma) ##This will change based on the value of R0

par_4 <- c(
  gamma,
  beta
)

time <- seq(0, 50) ##No change

##Running the model with these new parameters
SIR_4 <- as.data.frame(ode(y = init,
                           times = time,
                           func = sir_equations,
                           parms = par_3
))

plot_4 <- plot_graph(SIR_4)

##Comparing these plots
patchwork <- plot_1 + plot_2 + plot_3 + plot_4
patchwork + plot_annotation(title = "Measles infectious dynamics for a range of parameter values",
                            caption = "Green = Susceptible, Red = Infectious, Blue = Recovered")
