library("deSolve") ##Package to numerically solve differential equations
library("tidyverse")

##Setting model parameters
R_0 <- 12 ##The number of infections caused by a single introduction of a case into a susceptible population
N <- 10000 ##The population at the beginning of the model
I <- 12  ##The number of introduced infections
tau <- 11 ##The number of days an individual is infectious for

gamma = gamma_val(tau) ##The rate of recovery from infection per unit time is termed gamma
beta = beta_val(R_0, N, gamma) ##The effective infectious contact rate per unit time is termed beta

par <- c(
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

SIR_1 <- as.data.frame(ode(y = init,
                           times = time,
                           func = sir_equations,
                           parms = par
                           ))

ggplot(SIR_1, aes(x = time))+
  geom_line(aes(y = S), colour = "green")+
  geom_line(aes(y = I), colour = "red")+
  geom_line(aes(y = R), colour = "blue")+
  labs(title = paste("Measles infection dynamics for an R0 =",R_0, "and an infectious period of", tau, "days"))+
  xlab("Time in days")+
  ylab("Number of individuals (N=10,000)")

