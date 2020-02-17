##Function to derive beta from R0
beta_val <- function(R_0, N, gamma) {
  beta <- ((R_0*gamma)/N)
}

##Function to dervive recovery rate from time of infection
gamma_val <- function(tau) {
  gamma <- 1/tau
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
