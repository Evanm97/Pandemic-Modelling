library(deSolve)
# library(ggplot2)
require(gridExtra)
library(scales)



modelIreland <- function(time, stocks, auxs) {
  with(as.list(c(stocks, auxs)), {

    states <- matrix(stocks, nrow = NUM_COHORTS, ncol = 3)

    Susceptible <- states[, 1]
    Infected    <- states[, 2]
    Recovered   <- states[, 3]

    CP <- countries[[countryNo]][[2]]

    beta <- CE_I / CP

    Lambda <- beta %*% Infected

    IR <- Lambda * Susceptible
    RR <- Infected / rep(RECOVERY_DELAY, NUM_COHORTS)

    QR <- (IR - RR) * QV_I * QF

    # fIR_PPE <- min(Infected/PPEDELAY, PPE) / NUM_COHORTS

    dS_dt  <- -IR
    dI_dt  <- IR - RR - QR
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

modelFrance <- function(time, stocks, auxs) {
  with(as.list(c(stocks, auxs)), {

    states <- matrix(stocks, nrow = NUM_COHORTS, ncol = 3)

    Susceptible <- states[, 1]
    Infected    <- states[, 2]
    Recovered   <- states[, 3]

    CP <- countries[[countryNo]][[2]]

    beta <- CE_F / CP

    Lambda <- beta %*% Infected

    IR <- Lambda * Susceptible
    RR <- Infected / rep(RECOVERY_DELAY, NUM_COHORTS)

    QR <- (IR - RR) * QV_F * QF

    dS_dt  <- -IR
    dI_dt  <- IR - RR - QR
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

modelPortugal <- function(time, stocks, auxs) {
  with(as.list(c(stocks, auxs)), {

    states <- matrix(stocks, nrow = NUM_COHORTS, ncol = 3)

    Susceptible <- states[, 1]
    Infected    <- states[, 2]
    Recovered   <- states[, 3]

    CP <- countries[[countryNo]][[2]]

    beta <- CE_P / CP

    Lambda <- beta %*% Infected

    IR <- Lambda * Susceptible
    RR <- Infected / rep(RECOVERY_DELAY, NUM_COHORTS)

    QR <- (IR - RR) * QV_F * QF

    dS_dt  <- -IR
    dI_dt  <- IR - RR - QR
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

    states <- matrix(stocks, nrow = NUM_COHORTS, ncol = 3)

    Susceptible <- states[, 1]
    Infected    <- states[, 2]
    Recovered   <- states[, 3]

    CP <- countries[[countryNo]][[2]]

    beta <- CE_S / CP

    Lambda <- beta %*% Infected

    IR <- Lambda * Susceptible
    RR <- Infected / rep(RECOVERY_DELAY, NUM_COHORTS)

    QR <- (IR - RR) * QV_S * QF

    dS_dt  <- -IR
    dI_dt  <- IR - RR - QR
    dR_dt  <- RR

    return (list(c(dS_dt, dI_dt, dR_dt), IR, RR, Lambda,
                 RecDelay = rep(RECOVERY_DELAY, NUM_COHORTS)
    ))
  })
}

model_d <- function(time, stocks, auxs){
  with(as.list(c(stocks, auxs)),{

    infectiousContacts <- sInfected * aContactRate
    infectiousContacts_PPE <- sInfected_PPE * aContactRate
    infectiousContacts_Q <- sInfected_Q * 0

    fIR <- (infectiousContacts * (sSusceptible/aTotalPopulation) * aInfectivity)
    + (infectiousContacts_PPE * (sSusceptible/aTotalPopulation) * aInfectivity_PPE)
    + (infectiousContacts_Q * (sSusceptible/aTotalPopulation) * aInfectivity)

    fRR <- sInfected / aDelay
    fRR_PPE <- sInfected_PPE / aDelay

    # Quaratine
    fQR <- (fIR - fRR) * quarantineF
    fQRR <- fQR / aDelay

    fIR_PPE <- min(sInfected/PPEDELAY, PPE) * PPEFLAG

    d_sSusceptible_dt  <- - fIR
    d_sInfected_dt     <- fIR - fRR - fQR - fIR_PPE
    d_sRecovered_dt    <- fRR + fQRR + fRR_PPE

    d_sInfected_Q_dt <- fQR - fQRR

    d_sPPE_dt <- - fIR_PPE
    d_sInfected_w_PPE <- fIR_PPE - fRR_PPE

    return (list(c(d_sSusceptible_dt,d_sInfected_dt,d_sRecovered_dt, d_sInfected_Q_dt, d_sPPE_dt, d_sInfected_w_PPE),
                 IR=fIR, RR=fRR, RecDelay=aDelay))
  })
}

  ce_func <- function(vm, ce, i) {
    for (value in vm) {
      if(value < ce) {
        vm[i] <- format(round(runif(1, 0, 0.1), 2), nsmall = 2)
      } else {
        vm[i] <- format(round(runif(1, 1, 10), 2), nsmall = 2)
      }
      i <- i + 1
    }
    return(vm)
  }

