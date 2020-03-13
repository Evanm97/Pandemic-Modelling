library(deSolve)
library(ggplot2)
require(gridExtra)
library(scales)

START<-0; FINISH<-5; STEP<-0.125;
NUM_COHORTS<-18; NUM_STATES<-3
simtime <- seq(START, FINISH, by=STEP)


CE  <- matrix(c(1.5, 0.25, 0.1, 0.1, 0.2, 0.03, 0.25, 0.1, 0.2, 0.2, 0.03, 0.25, 0.1, 0., 0.2, 0.1, 0.1, 0.1,
                0.03, 0.25, 0.1, 0.5, 0.2, 0.03, 0.25, 0.1, 0.5, 0.2, 0.03, 0.25, 0.1, 0.5, 0.2, 0.1, 0.1, 0.1,
                0.03, 0.25, 0.1, 0.5, 0.2, 0.03, 0.25, 0.1, 0.5, 0.2, 0.03, 0.25, 0.1, 0.5, 0.2, 0.1, 0.1, 0.1,
                0.03, 0.25, 0.1, 0.5, 0.2, 0.03, 0.25, 0.1, 0.5, 0.2, 0.03, 0.25, 0.1, 0.5, 0.2, 0.1, 0.1, 0.1,
                0.03, 0.25, 0.1, 0.5, 0.2, 0.03, 0.25, 0.1, 0.5, 0.2, 0.03, 0.25, 0.1, 0.5, 0.2, 0.1, 0.1, 0.1,
                0.03, 0.25, 0.1, 0.5, 0.2, 0.03, 0.25, 0.1, 0.5, 0.2, 0.03, 0.25, 0.1, 0.5, 0.2, 0.1, 0.1, 0.1,
                0.03, 0.25, 0.1, 0.5, 0.2, 0.03, 0.25, 0.1, 0.5, 0.2, 0.03, 0.25, 0.1, 0.5, 0.2, 0.1, 0.1, 0.1,
                0.03, 0.25, 0.1, 0.5, 0.2, 0.03, 0.25, 0.1, 0.5, 0.2, 0.03, 0.25, 0.1, 0.5, 0.2, 0.1, 0.1, 0.1,
                0.03, 0.25, 0.1, 0.5, 0.2, 0.03, 0.25, 0.1, 0.5, 0.2, 0.03, 0.25, 0.1, 0.5, 0.2, 0.1, 0.1, 0.1,
                0.03, 0.25, 0.1, 0.5, 0.2, 0.03, 0.25, 0.1, 0.5, 0.2, 0.03, 0.25, 0.1, 0.5, 0.2, 0.1, 0.1, 0.1,
                0.03, 0.25, 0.1, 0.5, 0.2, 0.03, 0.25, 0.1, 0.5, 0.2, 0.03, 0.25, 0.1, 0.5, 0.2, 0.1, 0.1, 0.1,
                0.03, 0.25, 0.1, 0.5, 0.2, 0.03, 0.25, 0.1, 0.5, 0.2, 0.03, 0.25, 0.1, 0.5, 0.2, 0.1, 0.1, 0.1,
                0.03, 0.25, 0.1, 0.5, 0.2, 0.03, 0.25, 0.1, 0.5, 0.2, 0.03, 0.25, 0.1, 0.5, 0.2, 0.1, 0.1, 0.1,
                0.03, 0.25, 0.1, 0.5, 0.2, 0.03, 0.25, 0.1, 0.5, 0.2, 0.03, 0.25, 0.1, 0.5, 0.2, 0.1, 0.1, 0.1,
                0.03, 0.25, 0.1, 0.5, 0.2, 0.03, 0.25, 0.1, 0.5, 0.2, 0.03, 0.25, 0.1, 0.5, 0.2, 0.1, 0.1, 0.1,
                0.03, 0.25, 0.1, 0.5, 0.2, 0.03, 0.25, 0.1, 0.5, 0.2, 0.03, 0.25, 0.1, 0.5, 0.2, 0.1, 0.1, 0.1,
                0.03, 0.25, 0.1, 0.5, 0.2, 0.03, 0.25, 0.1, 0.5, 0.2, 0.03, 0.25, 0.1, 0.5, 0.2, 0.1, 0.1, 0.1,
                0.03, 0.25, 0.1, 0.5, 0.2, 0.03, 0.25, 0.1, 0.5, 0.2, 0.03, 0.25, 0.1, 0.5, 0.2, 0.1, 0.1, 0.1),
              nrow=18,ncol=18,byrow=TRUE)

CohortPopulatons<- c(france$population)

beta <- CE/CohortPopulatons

#Setup the model variables
stocks <- c(c(france$population[1]-1, france$population[2:18]),  
            c(1, rep(0, 17)),
            rep(0, 18))

delays <- c(rep(2, 18))

stocks

auxs <-NULL

model <- function(time, stocks, auxs){
  with(as.list(c(stocks, auxs)),{ 
    #convert the stocks vector to a matrix
    states<-matrix(stocks,nrow=NUM_COHORTS,ncol=NUM_STATES)
    
    Susceptible <- states[,1]
    Susceptible
    Infected    <- states[,2]
    Recovered   <- states[,3]
    
    Lambda      <- beta %*% Infected
    
    IR         <- Lambda * Susceptible
    RR         <- Infected / delays
    
    dS_dt  <- -IR
    dI_dt  <- IR - RR
    dR_dt  <- RR
    
    return (list(c(dS_dt, dI_dt, dR_dt)))  
  })
}

oFR<-data.frame(ode(y=stocks, times=simtime, func = model, 
                   parms=auxs, method="euler"))

oFR
oFR$X19

# o2$TotalInfected<-o2$InfectedD+o2$InfectedG+o2$InfectedL+o2$InfectedC+o2$InfectedW

p2<-ggplot()+
  geom_line(data=oFR,size=1,aes(time,oFR$X19,color="1. Île-de-France"))+
  # geom_line(data=o2,size=1,aes(time,o2$TotalInfected,color="6. Total"))+
  scale_y_continuous(labels = comma)+
  ylab("Infected")+
  xlab("Month") +
  labs(color="")+
  theme(legend.position="bottom")
