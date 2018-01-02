x<-c(17, 19, 26, 29, 31, 32, 34, 45, 47, 51, 52, 59, 60, 62, 63)
#min-max normalization
min_max.new_value<-function(x,value){
  min.A<-0
  max.A<-1
  result<-((value-min(x))/(max(x)-min(x)))*(max.A-min.A)+ min.A
  return (result)
}
#z-score normalization
z_score.new_value<-function(x,value){
  result<-(value-mean(x))/sd(x)
  return(result)
}
#Problem-1(3)
min_max.new_value(x,26)
min_max.new_value(x,45)
#Problem-1(4)
z_score.new_value(x,26)
z_score.new_value(x,45)
