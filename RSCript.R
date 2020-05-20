bike<-read.csv('bikeshare.csv')
head(bike)
#Exploratory Data Analysis

library(ggplot2)
pl<-ggplot(bike,aes(temp,count))+geom_point(aes(color=temp),alpha=.5)
print(pl)

bike$datetime<-as.POSIXct(bike$datetime)
pl2<-ggplot(bike,aes(datetime,count))+geom_point(aes(color=temp),alpha=.5)
print(pl2)                                                 

a<-cor(bike$temp,bike$count)
print(a)

pl3<-ggplot(bike,aes(factor(season),count))+geom_boxplot(aes(color=factor(season)))
print(pl3)
bike$hour <- sapply(bike$datetime,function(x){format(x,"%H")})
head(bike)


library(dplyr)
#plot for working days
bike1<-filter(bike,workingday==1)
print(bike1)
pl4<-ggplot(bike1,aes(hour,count))+geom_point(aes(color=temp))
print(pl4)

#plot for non working day
bike2<-filter(bike,workingday==0)
head(bike2)
pl5<-ggplot(bike1,aes(hour,count))+geom_point(aes(color=temp))
print(pl5)


#lm model
temp.model<-lm(count~temp,bike)
summary(temp.model)

#predict
a<-data.frame(temp=c(25))
result<-predict(temp.model,a)
print(result)


bike$hour <- sapply(bike$hour,as.numeric)
