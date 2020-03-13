library(deSolve)
library(ggplot2)
require(gridExtra)
library(scales)

START<-0; FINISH<-20; STEP<-0.125;
NUM_COHORTS<-32; NUM_STATES<-3
simtime <- seq(START, FINISH, by=STEP)


CE  <- matrix(c(3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
                3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
                3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
                3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
                3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
                3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
                3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
                3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
                3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
                3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
                3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
                3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
                3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
                3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
                3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
                3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
                3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
                3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
                3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
                3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
                3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
                3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
                3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
                3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
                3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
                3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
                3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
                3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
                3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
                3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
                3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
                3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,3.0,2.0,1.0,1.5,1.2,1.0,1.0,1.0,1.0,1.0,1.0,1.0),
              nrow=32,ncol=32,byrow=TRUE)

CohortPopulatons<- c(618108,174792,56932,76176,118817,542868,247132,159192,532665,1347359,61170,258058,147707,222504,99232,84697,32044,194899,40873,128884,130507,195044,61386,77961,64544,65535,159553,179000,116176,88770,149772,142425)

beta <- CE/CohortPopulatons

#Setup the model variables
stocks <- c(SusAntrim=618108, SusArmagh=174792, SusCW=56932, SusCN=76176, SusCE=118817, SusC=542868, SusDerry=247132, SusDL=159192,
            SusDown=531665, SusD=1347358, SusFermanagh=61170, SusG=258058, SusKY=147707, SusKE=222504, SusKK=99232, SusLS=84697,
            SusLM=32044, SusL=194899, SusLD=40873, SusLH=128884, SusMO=130507, SusMH=195044, SusMN=61386, SusOY=77961,
            SusRN=64544, SusSO=65535, SusT=159553, SusTyrone=179000, SusW=116176, SusWH=88770, SusWX=149722, SusWW=142425,
            
            InfAntrim=0, InfArmagh=0, InfCW=0, InfCN=0, InfCE=0, InfC=0, InfDerry=0, InfDL=0,
            InfDown=0, InfD=1, InfFermanagh=0, InfG=0, InfKY=0, InfKE=0, InfKK=0, InfLS=0,
            InfLM=0, InfL=0, InfLD=0, InfLH=0, InfMO=0, InfMH=0, InfMN=0, InfOY=0,
            InfRN=0, InfSO=0, InfT=0, InfTyrone=0, InfW=0, InfWH=0, InfWX=0, InfWW=0,
            
            RecAntrim=0, RecArmagh=0, RecCW=0, RecCN=0, RecCE=0, RecC=0, RecDerry=0, RecDL=0,
            RecDown=0, RecD=0, RecFermanagh=0, RecG=0, RecKY=0, RecKE=0, RecKK=0, RecLS=0,
            RecLM=0, RecL=0, RecLD=0, RecLH=0, RecMO=0, RecMH=0, RecMN=0, RecOY=0,
            RecRN=0, RecSO=0, RecT=0, RecTyrone=0, RecW=0, RecWH=0, RecWX=0, RecWW=0)

delays <- c(Antrim=2.0, Armagh=2.0, CW=2.0, CN=2.0, CE=2.0, C=2.0, Derry=2.0, Donegal=2.0,
            Down=2.0, D=2.0, Fermanagh=2.0, G=2.0, KY=2.0, KE=2.0, KK=2.0, LS=2.0,
            LM=2.0, L=2.0, LD=2.0, LH=2.0, MO=2.0, MH=2.0, MN=2.0, OY=2.0,
            RN=2.0, SO=2.0, T=2.0, Tyrone=2.0, W=2.0, WH=2.0, WX=2.0, WW=2.0)

auxs <-NULL

model <- function(time, stocks, auxs){
  with(as.list(c(stocks, auxs)),{ 
    #convert the stocks vector to a matrix
    states<-matrix(stocks,nrow=NUM_COHORTS,ncol=NUM_STATES)
    
    Susceptible <- states[,1]
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

o<-data.frame(ode(y=stocks, times=simtime, func = model, 
                  parms=auxs, method="euler"))

o$TotalInfected<-o$InfAntrim + o$InfArmagh + o$InfCW + o$InfCN + o$InfCE + o$InfC + o$InfDerry + o$InfDL
 + o$InfDown + o$InfD + o$InfFermanagh + o$InfG + o$InfKY + o$InfKE + o$InfKK + o$InfLS
 + o$InfLM + o$InfL + o$InfLD + o$InfLH + o$InfMO + o$InfMH + o$InfMN + o$InfOY + o$InfRN 
 + o$InfSO + o$InfT + o$InfTyrone + o$InfW + o$InfWH + o$InfWX + o$InfWW

p3<-ggplot()+
  geom_line(data=o,size=1,aes(time,o$InfAntrim,color="1. Antrim"))+
  geom_line(data=o,size=1,aes(time,o$InfArmagh,color="2. Armagh"))+
  geom_line(data=o,size=1,aes(time,o$InfCW,color="3. Carlow"))+
  geom_line(data=o,size=1,aes(time,o$InfCN,color="4. Cavan"))+
  geom_line(data=o,size=1,aes(time,o$InfCE,color="5. Clare"))+
  geom_line(data=o,size=1,aes(time,o$InfC,color="6. Cork"))+
  geom_line(data=o,size=1,aes(time,o$InfDerry,color="7. Derry"))+
  geom_line(data=o,size=1,aes(time,o$InfDL,color="8. Donegal"))+
  geom_line(data=o,size=1,aes(time,o$InfDown,color="9. Down"))+
  geom_line(data=o,size=1,aes(time,o$InfD,color="10. Dublin"))+
  geom_line(data=o,size=1,aes(time,o$InfFermanagh,color="11. Fermanagh"))+
  geom_line(data=o,size=1,aes(time,o$InfG,color="12. Galway"))+
  geom_line(data=o,size=1,aes(time,o$InfKY,color="13. Kerry"))+
  geom_line(data=o,size=1,aes(time,o$InfKE,color="14. Kildare"))+
  geom_line(data=o,size=1,aes(time,o$InfKK,color="15. Kilkenny"))+
  geom_line(data=o,size=1,aes(time,o$InfLS,color="16. Laois"))+
  geom_line(data=o,size=1,aes(time,o$InfLM,color="17. Leitrim"))+
  geom_line(data=o,size=1,aes(time,o$InfL,color="18. Limerick"))+
  geom_line(data=o,size=1,aes(time,o$InfLD,color="19. Longford"))+
  geom_line(data=o,size=1,aes(time,o$InfLH,color="20. Louth"))+
  geom_line(data=o,size=1,aes(time,o$InfMO,color="21. Mayo"))+
  geom_line(data=o,size=1,aes(time,o$InfMH,color="22. Meath"))+
  geom_line(data=o,size=1,aes(time,o$InfMN,color="23. Monaghan"))+
  geom_line(data=o,size=1,aes(time,o$InfOY,color="24. Offaly"))+
  geom_line(data=o,size=1,aes(time,o$InfRN,color="25. Roscommon"))+
  geom_line(data=o,size=1,aes(time,o$InfSO,color="26. Sligo"))+
  geom_line(data=o,size=1,aes(time,o$InfT,color="27. Tipperary"))+
  geom_line(data=o,size=1,aes(time,o$InfTyrone,color="28. Tyrone"))+
  geom_line(data=o,size=1,aes(time,o$InfW,color="29. Waterford"))+
  geom_line(data=o,size=1,aes(time,o$InfWH,color="30. Westmeath"))+
  geom_line(data=o,size=1,aes(time,o$InfWX,color="31. Wexford"))+
  geom_line(data=o,size=1,aes(time,o$InfWW,color="32. Wicklow"))+
  #geom_line(data=o,size=1,aes(time,o$TotalInfected,color="33. Total"))+
  scale_y_continuous(labels = comma)+
  ylab("Infected")+
  xlab("Month") +
  labs(color="")+
  theme(legend.position="bottom")

p3
