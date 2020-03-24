library(deSolve)
# library(ggplot2)
require(gridExtra)
library(scales)



modelIreland <- function(time, stocks, auxs) {
  with(as.list(c(stocks, auxs)), {
    #convert the stocks vector to a matrix
    states <- matrix(stocks, nrow = NUM_COHORTS, ncol = 3)
    
    Susceptible <- states[, 1]
    Infected    <- states[, 2]
    Recovered   <- states[, 3]
    
    CE_MATRIX <- matrix(ce_func(diag(CE, NUM_COHORTS, NUM_COHORTS), CE, 1),
                        nrow = NUM_COHORTS,ncol = NUM_COHORTS, byrow = TRUE)
    
    CP <- countries[[countryNo]][[2]]
    
    beta <- CE_MATRIX / CP
    
    Lambda <- beta %*% Infected
    
    IR <- Lambda * Susceptible
    RR <- Infected / rep(RECOVERY_DELAY, NUM_COHORTS)
    
    dS_dt  <- -IR
    dI_dt  <- IR - RR
    dR_dt  <- RR
    
    return (list(
      c(dS_dt, dI_dt, dR_dt),
      IR,
      RR,
      Lambda,
      RecDelay = rep(RECOVERY_DELAY, NUM_COHORTS)
    ))
  })
  
}

modelFandP <- function(time, stocks, auxs) {
  with(as.list(c(stocks, auxs)), {
    #convert the stocks vector to a matrix
    states <- matrix(stocks, nrow = NUM_COHORTS, ncol = 3)
    
    Susceptible <- states[, 1]
    Infected    <- states[, 2]
    Recovered   <- states[, 3]
    
    CE_MATRIX <- matrix(ce_func(diag(CE, NUM_COHORTS, NUM_COHORTS), CE, 1),
                        nrow = NUM_COHORTS,ncol = NUM_COHORTS, byrow = TRUE)
    
    CP <- countries[[countryNo]][[2]]
    
    beta <- CE_MATRIX / CP
    
    Lambda <- beta %*% Infected
    
    IR <- Lambda * Susceptible
    RR <- Infected / rep(RECOVERY_DELAY, NUM_COHORTS)
    
    dS_dt  <- -IR
    dI_dt  <- IR - RR
    dR_dt  <- RR
    
    return (list(
      c(dS_dt, dI_dt, dR_dt),
      IR,
      RR,
      Lambda,
      RecDelay = rep(RECOVERY_DELAY, NUM_COHORTS)
    ))
  })
}

modelSpain <- function(time, stocks, auxs) {
  with(as.list(c(stocks, auxs)), {
    #convert the stocks vector to a matrix
    states <- matrix(stocks, nrow = NUM_COHORTS, ncol = 3)
    
    Susceptible <- states[, 1]
    Infected    <- states[, 2]
    Recovered   <- states[, 3]
    
    CE_MATRIX <- matrix(ce_func(diag(CE, NUM_COHORTS, NUM_COHORTS), CE, 1),
                        nrow = NUM_COHORTS,ncol = NUM_COHORTS, byrow = TRUE)
    
    CP <- countries[[countryNo]][[2]]
    
    beta <- CE_MATRIX / CP
    
    Lambda <- beta %*% Infected
    
    IR <- Lambda * Susceptible
    RR <- Infected / rep(RECOVERY_DELAY, NUM_COHORTS)
    
    dS_dt  <- -IR
    dI_dt  <- IR - RR
    dR_dt  <- RR
    
    return (list(c(dS_dt, dI_dt, dR_dt), IR, RR, Lambda, 
                 RecDelay = rep(RECOVERY_DELAY, NUM_COHORTS)
    ))
  })
}
  
  ce_func <- function(vm, ce, i) {
    for (value in vm) {
      if(value < ce) {
        vm[i] <- runif(1, 0, 0.1)
      }
      i <- i + 1
    }
    return(vm)
  }

