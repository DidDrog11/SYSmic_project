library("deSolve") ##Package to numerically solve differential equations
library("tidyverse")
library("patchwork")
library("socialmixr")


# Part 1 ------------------------------------------------------------------
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
R_0 <- 12 ##No change
N <- 10000 ##No change
I <- 12  ##No change
tau <- 7 ##Lower limit of infectious period

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
R_0 <- 12 ##No change
N <- 10000 ##No change
I <- 12  ##No change
tau <- 12  ##Increased to maximum infectious period

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
                           parms = par_4
))

plot_4 <- plot_graph(SIR_4)

##Comparing these plots
patchwork <- plot_1 + plot_2 + plot_3 + plot_4
patchwork + plot_annotation(title = "Measles infectious dynamics for a range of parameter values",
                            caption = "Green = Susceptible, Red = Infectious, Blue = Recovered")

##We will now use the initial model of an R0 of 12 and tau of 9 days for different starting conditions
R_0 <- 12 ##The number of infections caused by a single introduction of a case into a susceptible population
N <- 10000 ##The population at the beginning of the model
I <- 12  ##The number of introduced infections
R <- 0 ##The number of previously immune individuals
tau <- 9 ##The number of days an individual is infectious for

gamma = gamma_val(tau) ##The rate of recovery from infection per unit time is termed gamma
beta = beta_val(R_0, N, gamma) ##The effective infectious contact rate per unit time is termed beta

par <- c(
  gamma,
  beta
)

init <- c(
  S = N-I,  ##The number of susceptible individuals at time point t=0 is S
  I =   I,  ##The number of infectious individuals introduced at time point t=0 is I
  R =   R   ##The number of recovered individuals at time point t=0 is R
)

time <- seq(0, 50) ##This represents the time step between T1 and T2

##First model
SIR_1 <- as.data.frame(ode(y = init,
                           times = time,
                           func = sir_equations,
                           parms = par
))

plot_5 <- plot_graph(SIR_1)+
  labs(title = paste("S=", init[1], "I=", I, "R=", R),
       caption = paste("R0 =",R_0, "Tau =", tau, "days"))

##Changing the starting conditions to a reduced number of initially infected individuals
N <- 10000
I <- 2
R <- 0

init <- c(
  S = N-(I+R),  ##The number of susceptible individuals at time point t=0 is S
  I =   I,  ##The number of infectious individuals introduced at time point t=0 is I
  R =   R   ##The number of recovered individuals at time point t=0 is R
)

##Second model
SIR_5 <- as.data.frame(ode(y = init,
                           times = time,
                           func = sir_equations,
                           parms = par
))

plot_6 <- plot_graph(SIR_5)+
  labs(title = paste("S=", init[1], "I=", I, "R=", R),
       caption = paste("R0 =",R_0, "Tau =", tau, "days"))

##Changing the starting conditions to an increased number of vaccinated individuals to current UK average (2 doses by 5) = 86.4
N <- 10000
I <- 12
R <- 8640

init <- c(
  S = N-(I+R),  ##The number of susceptible individuals at time point t=0 is S
  I =   I,  ##The number of infectious individuals introduced at time point t=0 is I
  R =   R   ##The number of recovered individuals at time point t=0 is R
)

##Third model
SIR_6 <- as.data.frame(ode(y = init,
                           times = time,
                           func = sir_equations,
                           parms = par
))

plot_7 <- plot_graph(SIR_6)+
  labs(title = paste("S=", init[1], "I=", I, "R=", R),
       caption = paste("R0 =",R_0, "Tau =", tau, "days"))

##Changing the starting conditions to an increased number of vaccinated individuals to level required for herd immunity = 98%
N <- 10000
I <- 12
R <- 9800

init <- c(
  S = N-(I+R),  ##The number of susceptible individuals at time point t=0 is S
  I =   I,  ##The number of infectious individuals introduced at time point t=0 is I
  R =   R   ##The number of recovered individuals at time point t=0 is R
)

##Fourth model
SIR_7 <- as.data.frame(ode(y = init,
                           times = time,
                           func = sir_equations,
                           parms = par
))

plot_8 <- plot_graph(SIR_7)+
  labs(title = paste("S=", init[1], "I=", I, "R=", R),
       caption = paste("R0 =",R_0, "Tau =", tau, "days"))

patchwork_2 <- plot_5 + plot_6 + plot_7 + plot_8
patchwork_2 + plot_annotation(title = "Measles infectious dynamics for a range of parameter values",
                            caption = "Green = Susceptible, Red = Infectious, Blue = Recovered")

# Part 2 ------------------------------------------------------------------
##The UK population in 1950 was 50380000
##Setting model parameters
R_0 <- 12 ##The number of infections caused by a single introduction of a case into a susceptible population
N <- 50380000 ##The population at the beginning of the model
I <- 3000/7  ##The number of introduced infections, this is the level of the lowest number of weekly cases/days
R <- ((50380000/100)*91.3)-I ##The number of resistant individuals in the population
tau <- 7 ##The number of days an individual is infectious for

gamma = gamma_val(tau) ##The rate of recovery from infection per unit time is termed gamma
beta = beta_val(R_0, N, gamma) ##The effective infectious contact rate per unit time is termed beta

par_5 <- c(
  gamma,
  beta
)

init <- c(
  S = N-(I+R),  ##The number of susceptible individuals at time point t=0 is S
  I =   I,  ##The number of infectious individuals introduced at time point t=0 is I
  R =   R   ##The number of recovered individuals at time point t=0 is R
)

time <- seq(0, 3650) ##This represents the time step between T1 and T2

##First model
SIR_8 <- as.data.frame(ode(y = init,
                           times = time,
                           func = sir_equations,
                           parms = par_5
))

plot_9 <- measles_outbreaks(SIR_8)

##This approximately reproduces the peak of cases from the data, however over a much greater timeline
max(SIR_8$I)

##The birth rate in the UK and 1950 is reported as 14.39 per 1000 individuals. Estimated as around 2,000 births per day
mu <- ((14.39/1000)/365)

par_6 <- c(
  gamma,
  beta,
  mu
)

##First model to generate an oscillating epidemic based on 
SIR_9 <- as.data.frame(ode(y = init,
                           times = time,
                           func = sir_birth_death,
                           parms = par_6
))

cyclical_inf <- measles_outbreaks(SIR_9)
cyclical_sus <- measles_sus(SIR_9)
cyclical_rec <- measles_rec(SIR_9)

cyclical_inf/cyclical_rec/cyclical_sus

# Part 3 ------------------------------------------------------------------
##Using social mixing data and an age structure model
UK_child_contact <- contact_matrix(survey = polymod, countries = c("United Kingdom"), age.limits = c(0,1,5,15), symmetric = T)
