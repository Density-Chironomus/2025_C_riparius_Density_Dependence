# EmT50 vs food and density
ec50data<-read.table('EmT50.csv',header=TRUE,sep=';')
EmT50<-ec50data$EmT50
fl<-ec50data$food_level
dens<-ec50data$density
shapiro.test(EmT50)
bartlett.test(EmT50~fl)
bartlett.test(EmT50~dens)
cor1<-lm(EmT50~fl*dens,method='spearman')
summary(cor1)
cor2<-lm(EmT50~fl+dens,method='spearman')
summary(cor2)

