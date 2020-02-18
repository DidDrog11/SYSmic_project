##Function to derive beta from R0
beta_val <- function(R_0, N, gamma) {
  beta <- ((R_0*gamma)/N)
}

##Function to dervive recovery rate from time of infection
gamma_val <- function(nu) {
  gamma <- 1/nu
}

##Function to solve the SIR differential equation
sir_equations <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), {
    dS <- -beta * I * S
    dI <-  beta * I * S - gamma * I
    dR <-  gamma * I
    return(list(c(dS, dI, dR)))
  })
}

##Function to plot the SIR outcomes for a range of R0 and nu
plot_graph <- function(plot_model){
  ggplot(plot_model, aes(x = time))+
    geom_line(aes(y = S), colour = "green")+
    geom_line(aes(y = I), colour = "red")+
    geom_line(aes(y = R), colour = "blue")+
    theme_minimal()+
    labs(title = paste("R0 =",R_0, "nu =", nu, "days"))+
    xlab("Time in days")+
    ylab(paste("N=", N))+
    theme(plot.title = element_text(size = 12))
}

##Function to plot the outbreak size
measles_outbreaks <- function(measles_cases){
  ggplot(measles_cases, aes(x = time))+
    geom_line(aes(y = I), colour = "red")+
    theme_minimal()+
    labs(title = paste("Infectious population R0 =",R_0, "nu =", nu, "days"))+
    xlab("Time in days")+
    ylab(paste("N=", N))+
    theme(plot.title = element_text(size = 12))
}

##Function to solve the SIR differential equation when births incorporated
sir_birth_death <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), {
    dS <- (-beta * I * S) + (mu*N) - (mu*S)
    dI <- (beta * I * S) - (gamma * I) - (mu*I)
    dR <- (gamma * I) - (mu*R)
    return(list(c(dS, dI, dR)))
  })
}

##Function to plot changing susceptible population size
measles_sus <- function(measles_cases){
  ggplot(measles_cases, aes(x = time))+
    geom_line(aes(y = S), colour = "green")+
    theme_minimal()+
    labs(title = paste("Susceptible population R0 =",R_0, "nu =", nu, "days"))+
    xlab("Time in days")+
    ylab(paste("N=", N))+
    theme(plot.title = element_text(size = 12))
}

##Function to plot changing susceptible population size
measles_rec <- function(measles_cases){
  ggplot(measles_cases, aes(x = time))+
    geom_line(aes(y = R), colour = "blue")+
    theme_minimal()+
    labs(title = paste("Recovered population R0 =",R_0, "nu =", nu, "days"))+
    xlab("Time in days")+
    ylab(paste("N=", N))+
    theme(plot.title = element_text(size = 12))
}
