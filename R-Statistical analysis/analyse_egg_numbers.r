# Egg numbers vs food and density
egg_number<-read.table('Egg_number.csv',header=TRUE,sep=';')
egg<-egg_number$eggs
fl<-egg_number$food_level
dens<-egg_number$density
model1<-glm(egg~fl*dens,family = poisson(link = "log"))
summary(model1)
model2<-glm(egg~fl+dens,family=poisson(link="log"))
summary(model2)
library('AER')
dispersiontest(model2)
model3<-glm(egg~fl+dens,family=quasipoisson)
summary(model3)

