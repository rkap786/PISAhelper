integrate<-function(rt,caf) {
  library(kdensity)
  kd<-kdensity(rt)
  rt<-seq(min(rt),max(rt),length.out=10000)
  den<-kd(rt)
  del<-mean(diff(rt))
  y<-numeric()
  for (i in 1:length(rt)) {
    ii<-which.min(abs(rt[i]-caf$t))
    y[i]<-caf$yhat[ii]
  }
  sum(den*del*y)
}