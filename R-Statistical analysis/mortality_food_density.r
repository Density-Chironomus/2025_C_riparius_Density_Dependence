# mortality vs food and density
mort1b<-read.table('Mortality.csv',header=TRUE,sep=';')
dead<-mort1b$dead
initial<-mort1b$initial
alive<-initial-dead
food<-mort1b$food
food_level<-mort1b$food_level
ratio<-dead/initial
attach(mort1b)
mortrel<-mort/100
library('betareg')
fitbeta4<-betareg(mortrel~food*initial)
summary(fitbeta4)
fitbeta5<-betareg(mortrel~food_level*initial)
summary(fitbeta5)
fitbeta7<-betareg(ratio~food_level*initial)
summary(fitbeta7)
fitbeta8<-betareg(mortrel~food+initial)
summary(fitbeta8)
fitbeta9<-betareg(mortrel~food_level+initial)
summary(fitbeta9)
fitbeta11<-betareg(ratio~food_level+initial)
summary(fitbeta11)

