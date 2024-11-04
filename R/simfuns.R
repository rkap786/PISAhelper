simdata<-function(speed.offset,th.offset,N=1000,rho=0,b.time,nitems=45) {
  library(MASS) 
  z1<-mvrnorm(N,c(0,0),matrix(c(1,rho,rho,1),2,2))
  z2<-mvrnorm(N,c(th.offset,speed.offset),matrix(c(1,rho,rho,1),2,2))
  z<-rbind(cbind(z1,1),cbind(z2,2))
  th<-z[,1]
  tau<-z[,2]
  gr<-z[,3]
  diff<-rnorm(nitems,mean=0,sd=.5)
  L<-list()
  for (i in 1:length(diff)) L[[i]]<-cbind(1:length(th),i,th,tau,diff[i],gr)
  x<-data.frame(do.call("rbind",L))
  names(x)<-c("id","item","th","tau","diff","group")
  f1<-function(del) exp(rnorm(nrow(x),mean=.1+.3*(del),sd=.5))
  x$rt<-f1(-1*x$tau)
  f2<-function(th,t,diff,b.time) 1/(1+exp(-(th+b.time*t-diff)))
  f<-Vectorize(f2,vectorize.args=c("th","t","diff"))
  x$pv<-f(th=x$th,t=x$rt-mean(x$rt),diff=x$diff,b.time=b.time)
  x$resp<-rbinom(nrow(x),1,x$pv)
  x
}


# 
# simdata.rho<-function(speed.offset,th.offset,N=1000,rho1=0,rho2=0,b.time,nitems=45) {
#     library(MASS) 
#     z1<-mvrnorm(N,c(0,0),matrix(c(1,rho1,rho1,1),2,2))
#     z2<-mvrnorm(N,c(th.offset,speed.offset),matrix(c(1,rho2,rho2,1),2,2))
#     z<-rbind(cbind(z1,1),cbind(z2,2))
#     th<-z[,1]
#     tau<-z[,2]
#     gr<-z[,3]
#     diff<-rnorm(nitems,mean=0,sd=.5)
#     L<-list()
#     for (i in 1:length(diff)) L[[i]]<-cbind(1:length(th),i,th,tau,diff[i],gr)
#     x<-data.frame(do.call("rbind",L))
#     names(x)<-c("id","item","th","tau","diff","group")
#     f1<-function(del) exp(rnorm(nrow(x),mean=.1+.3*(del),sd=.5))
#       x$rt<-f1(-1*x$tau)
#     f2<-function(th,t,diff,b.time) 1/(1+exp(-(th+b.time*t-diff)))
#     f<-Vectorize(f2,vectorize.args=c("th","t","diff"))
#     x$pv<-f(th=x$th,t=x$rt-mean(x$rt),diff=x$diff,b.time=b.time)
#     x$resp<-rbinom(nrow(x),1,x$pv)
#     x
# }


integrate<-function(rt,caf) {
    ## library(kdensity)
    ## if (length(rt)>5000) rt<-sample(rt,5000)
    ## kd<-kdensity(rt)
    ## den<-kd(rt)

    den<-density(rt)
    rt0<-seq(min(rt),max(rt),length.out=1000)
    den<-approx(den$x,den$y,xout=rt0)$y
        
    del<-mean(diff(rt0))
    y<-numeric()
    ##
    ff<-function(t,caf) {
        ii<-which.min(abs(t-caf$t))
        caf$yhat[ii]
    }
    ff<-Vectorize(ff,"t")
    y<-ff(rt0,caf)
    sum(den*del*y)
}
