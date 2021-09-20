simdata<-function(speed.offset,th.offset,N=1000,rho=0,b.time) {
    library(MASS)
    z1<-mvrnorm(N,c(0,0),matrix(c(1,rho,rho,1),2,2))
    z2<-mvrnorm(N,c(th.offset,speed.offset),matrix(c(1,rho,rho,1),2,2))
    z<-rbind(cbind(z1,1),cbind(z2,2))
    th<-z[,1]
    tau<-z[,2]
    gr<-z[,3]
    diff<-rnorm(45,mean=0,sd=.5)
    L<-list()
    for (i in 1:length(diff)) L[[i]]<-cbind(1:length(th),i,th,tau,diff[i],gr)
    x<-data.frame(do.call("rbind",L))
    names(x)<-c("id","item","th","tau","diff","group")
    x$delta<-rnorm(nrow(x),mean=0,sd=.5)
    ##
    f1<-function(tau,delta) 2/(1+exp(-1*(tau-delta)))
    tau<-sort(seq(-4,4,length.out=1000))
    #plot(tau,f1(tau,1),type='l'); lines(tau,f1(tau,0),col='red')
    f<-Vectorize(f1)
    x$rt<-f1(x$tau,x$delta)
    #by(x$rt,x$group,mean)
    #plot(density(x$rt[x$group==1])); lines(density(x$rt[x$group==2]),col='red')
    ##
    f2<-function(th,t,diff,b.time) 1/(1+exp(-(th+b.time*t-diff)))
    f<-Vectorize(f2,vectorize.args=c("th","t","diff"))
    x$pv<-f(th=x$th,t=x$rt-mean(x$rt),diff=x$diff,b.time=b.time)
    #plot(density(x$pv[x$group==1])); lines(density(x$pv[x$group==2]),col='red')
    #by(x$pv,x$group,mean)
    x$resp<-rbinom(nrow(x),1,x$pv)
    #cor(x$pv,x$resp)
                                        #by(x$resp,x$group,mean)
    #print(summary(lm(resp~rt+group,x)))
    x
}

getcaf<-function(x) {
    library(splines)
    tmp.bs<-bs(x$rt)
     
    for (i in 1:ncol(tmp.bs)) x[[paste("bs",i,sep='')]]<-tmp.bs[,i]
    caf<-list()
    library(fixest) 
    ran<-range(x$rt)
    for (i in 1:2) {
        xx<-x[x$group==i,]
        mod<-feols(resp~bs1+bs2+bs3|item+id,xx)
        #print(mod)
                                        #p0<-mean(xx$pv.est)
        t<-seq(ran[1],ran[2],length.out=1000)
        z<-predict(tmp.bs,t)
        fe<-fixest::fixef(mod)
        if (i==1) {
            ii<-which.min(abs(fe$item))
            item<-names(fe$item)[ii]
        }
        ii<-which.min(abs(fe$id))
        id<-names(fe$id)[ii]
        z<-data.frame(bs1=z[,1],bs2=z[,2],bs3=z[,3],item=item,id=id)
        y0<-predict(mod,z)
        caf[[i]]<-data.frame(t=t,z,yhat=y0-mean(y0))
    }
    ##need spot to anchor
    y<-x[abs(x$rt-mean(x$rt))<.01,]
    M<-by(y$resp,y$group,mean)
    caf[[1]]<-cbind(caf[[1]]$t,M[[1]]+caf[[1]]$yhat)
    caf[[2]]<-cbind(caf[[2]]$t,M[[2]]+caf[[2]]$yhat)
    caf
}

integrate<-function(rt,caf) {
    library(kdensity)
    if (length(rt)>5000) rt<-sample(rt,5000)
    kd<-kdensity(rt)
    rt<-seq(min(rt),max(rt),length.out=1000)
    den<-kd(rt)
    del<-mean(diff(rt))
    y<-numeric()
    ##
    ff<-function(t,caf) {
        ii<-which.min(abs(t-caf$t))
        caf$yhat[ii]
    }
    ff<-Vectorize(ff,"t")
    y<-ff(rt,caf)
    ## for (i in 1:length(rt)) {
    ##     ii<-which.min(abs(rt[i]-caf$t))
    ##     y[i]<-caf$yhat[ii]
    ## }
    sum(den*del*y)
}



