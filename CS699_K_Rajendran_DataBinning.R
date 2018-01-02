x<-c(17, 19, 26, 29, 31, 32, 34, 45, 47, 51, 52, 59, 60, 62, 63)

#Depth binning data values for 3 bins
depthbinning<-function(x,y){
  size<-as.integer(length(x)/y)
  bin1<-rep(0,size)
  bin2<-rep(0,size)
  bin3<-rep(0,size)
  n<-1
  while(n<=size){
    bin1[n]<-x[n]
    bin2[n]<-x[size+n]
    bin3[n]<-x[2*size+n]
    n<-n+1
  }
  return (list(bin1,bin2,bin3))
}
#smoothing by bin means for depth binning
bin.mean<-function(x){
  bin1<-x[[1]]
  bin2<-x[[2]]
  bin3<-x[[3]]
  bin.size<-length(bin1)
  
  bin1.mean<-mean(bin1)
  bin2.mean<-mean(bin2)
  bin3.mean<-mean(bin3)
  
  bin1<-rep(bin1.mean,bin.size)
  bin2<-rep(bin2.mean,bin.size)
  bin3<-rep(bin3.mean,bin.size)
  
  return( list( bin1,bin2,bin3))
}

#smoothing by bin medians for depth binning
bin.median<-function(x){
  bin1<-x[[1]]
  bin2<-x[[2]]
  bin3<-x[[3]]
  bin.size<-length(bin1)
  bin1<-rep(median(bin1),bin.size)
  bin2<-rep(median(bin2),bin.size)
  bin3<-rep(median(bin3),bin.size)
  
  return( list( bin1,bin2,bin3))
}

#smoothing by bin boundaries for depth binning
bin.boundary<-function(x){
  bin1<-x[[1]]
  bin2<-x[[2]]
  bin3<-x[[3]]
  bin.size<-length(bin1)
  n=2
  while(n<bin.size){
    if((bin1[n]-min(bin1))<(max(bin1)-bin1[n])){
      bin1[n]<-min(bin1)
    }
    else{
      bin1[n]<-max(bin1)
    }
    n<-n+1
  }
  n=2
  while(n<bin.size){
   if((bin2[n]-min(bin2))<(max(bin2)-bin2[n])){
      bin2[n]<-min(bin2)
    }
    else{
      bin2[n]<-max(bin2)
    }
    n<-n+1
  }
  n=2
  while(n<bin.size){
    if((bin3[n]-min(bin3))<(max(bin3)-bin3[n])){
      bin3[n]<-min(bin3)
    }
    else{
      bin3[n]<-max(bin3)
    }
    n<-n+1
  }
  return(list(bin1,bin2,bin3))
}

#Width binning data values for 3 bins
widthbinning<-function(x,y){
  interval<-(max(x)-min(x))/y
  bin1<-c(min(x),min(x)+interval)
  bin2<-c(min(x)+interval,max(x)-interval)
  bin3<-c(max(x)-interval,max(x))
  n<-1
  k1=1
  k2=1
  k3=1
  bin1.values<-0
  bin2.values<-0
  bin3.values<-0
  while(n<=length(x)){
    if(x[n]>=bin1[1] && x[n]<bin1[2]){
      bin1.values[k1]<-x[n]
      k1<-k1+1
    }
    else if(x[n]>=bin2[1] && x[n]<bin2[2]){
      bin2.values[k2]<-x[n]
      k2<-k2+1
    }
    else if(x[n]>=bin3[1] && x[n]<=bin3[2]){
      bin3.values[k3]<-x[n]
      k3<-k3+1
    }
    n<-n+1
  }
  return (list(bin1.values,bin2.values,bin3.values))
}

#smoothing by bin means for width binning
bin.width.mean<-function(x){
  bin1<-x[[1]]
  bin2<-x[[2]]
  bin3<-x[[3]]
  
  bin1.mean<-mean(bin1)
  bin2.mean<-mean(bin2)
  bin3.mean<-mean(bin3)
  
  bin1<-rep(bin1.mean,length(bin1))
  bin2<-rep(bin2.mean,length(bin2))
  bin3<-rep(bin3.mean,length(bin3))
  
  return( list( bin1,bin2,bin3))
}

#smoothing by bin medians for width binning
bin.width.median<-function(x){
  bin1<-x[[1]]
  bin2<-x[[2]]
  bin3<-x[[3]]
  
  bin1<-rep(median(bin1),length(bin1))
  bin2<-rep(median(bin2),length(bin2))
  bin3<-rep(median(bin3),length(bin3))
  
  return( list( bin1,bin2,bin3))
}

#smoothing by bin boundaries for width binning
bin.width.boundary<-function(x){
  bin1<-x[[1]]
  bin2<-x[[2]]
  bin3<-x[[3]]
  n=2
  while(n<length(bin1)){
    if((bin1[n]-min(bin1))<(max(bin1)-bin1[n])){
      bin1[n]<-min(bin1)
    }
    else{
      bin1[n]<-max(bin1)
    }
    n<-n+1
  }
  n=2
  while(n<length(bin2)){
    if((bin2[n]-min(bin2))<(max(bin2)-bin2[n])){
      bin2[n]<-min(bin2)
    }
    else{
      bin2[n]<-max(bin2)
    }
    n<-n+1
  }
  n=2
  while(n<length(bin3)){
    if((bin3[n]-min(bin3))<(max(bin3)-bin3[n])){
      bin3[n]<-min(bin3)
    }
    else{
      bin3[n]<-max(bin3)
    }
    n<-n+1
  }
  return(list(bin1,bin2,bin3))
}

#main calculations 
#Equal width data binning
(bin.width.values<-widthbinning(x,3))
bin.width.mean(bin.width.values)
bin.width.median(bin.width.values)
bin.width.boundary(bin.width.values)
#Equal depth data binning
(bins.values<-depthbinning(x,3))
bin.mean(bins.values)
bin.median(bins.values)
bin.boundary(bins.values)

