library(foreign)
dataset<-read.arff("reduced-concrete-data.arff")

#function to find correlation
correl<-function(x,y){
  (x.meanvalue<-mean(x,na.rm = TRUE))
  (y.meanvalue<-mean(y,na.rm = TRUE))
  (x.deviation<-sd(x,na.rm = TRUE))
  (y.deviation<-sd(y,na.rm = TRUE))
  n<-1
  l<-length(y)
  sum.of.products<-0
  while(n<=l){
    sum.of.products<-(x[n]-x.meanvalue)*(y[n]-y.meanvalue)+sum.of.products
    n<-n+1
  }
  resultant<-(sum.of.products/(x.deviation*y.deviation))/(length(x)-1)
  return(resultant) 
}

#user-defined function
correl(dataset$Cement,dataset$Concrete_compressive_strength)
correl(dataset$Fly_Ash,dataset$Concrete_compressive_strength)
cor(dataset$Water,dataset$Concrete_compressive_strength)
correl(dataset$Age,dataset$Concrete_compressive_strength)

#Built-in functions
cor(dataset$Cement,dataset$Concrete_compressive_strength)
cor(dataset$Fly_Ash,dataset$Concrete_compressive_strength)
correl(dataset$Water,dataset$Concrete_compressive_strength)
cor(dataset$Age,dataset$Concrete_compressive_strength)
