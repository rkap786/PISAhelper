
interplay<-function(x,std.time.in.item=FALSE,nspl=4,fe.terms='item+id',nboot=NULL) {
  ##x needs to have columns:
  ## item [item id]
  ## id [person id]
  ## diff [item difficulty]
  ## th [person theta]
  ## pv [irt-based p-value]
  ## rt [response time in metric you want to analyze]
  ## resp [item response]
  #####################################################################
  nms<-c("item","id","diff","th","pv","rt")
  if (!(all(nms %in% names(x)))) stop("need more columns")
  ##standardizd item times within item
  if (std.time.in.item) {
    L<-split(x,x$item)
    std<-function(z) (z-mean(z,na.rm=TRUE))/sd(z,na.rm=TRUE)
    for (i in 1:length(L)) {
      L[[i]]->y
      y$rt<-std(y$rt)
      L[[i]]<-y
    }
    x<-data.frame(do.call("rbind",L))
  }
  tmp<-x[,nms]
  x<-x[rowSums(is.na(tmp))==0,]
  #############################################################################
  library(splines)
  x$pv.center<-x$pv-mean(x$pv,na.rm=TRUE)
  library(splines)
  bs(x$rt,df=nspl)->spl
  for (i in 1:ncol(spl)) spl[,i]->x[[paste("spl",i,sep='')]]
  ##########################################################################
  ##now model accuracy
  modfun<-function(x,xv) {
    library(fixest) ##won't work on ozzy 
    fm.spl<-paste(paste("spl",1:nspl,sep=""),collapse="+")
    fm<-paste("resp~1+pv.center+(",fm.spl,")",sep='')
    fm.fe<-paste(fm,"|",fe.terms,sep="")
    feols(formula(fm.fe),x)->m
    ##fitted accuracy
    fe<-fixest::fixef(m)
    M<-mean(fe$id)
    index<-which.min(abs(fe$id-M))
    id<-names(fe$id)[index]
    M<-mean(fe$item)
    index<-which.min(abs(fe$item-M))
    item<-names(fe$item)[index]
    ##fitted values
    pv<-0
    predict(spl,xv)->tmp
    for (i in 1:ncol(tmp)) colnames(tmp)[i]<-paste("spl",i,sep="")
    ##
    z<-expand.grid(pv.center=pv,rt.num=1:nrow(tmp))
    tmp<-data.frame(rt.num=1:100,tmp)
    z<-merge(z,tmp)
    z<-merge(z,data.frame(rt.num=1:100,rt=xv))
    z$item<-item
    z$id<-id
    z$resp<-predict(m,z,"response")
    z$resp<-z$resp-mean(z$resp)
    pts<-cbind(rt=z$rt,resp=z$resp)
  }
  rt.lims<-quantile(x$rt,c(.05,.95),na.rm=TRUE)
  xv<-seq(rt.lims[1],rt.lims[2],length.out=100)
  pts<-modfun(x,xv)
  ##bootstrap?
  if (!is.null(nboot)) {
    pb<-list()
    for (i in 1:nboot) {
      boot.index<-sample(1:nrow(x),nrow(x),replace=TRUE)
      pb[[i]]<-modfun(x[boot.index,],xv=xv)[,2]
    }
    pb<-do.call("cbind",pb)
    cil<-apply(pb,1,quantile,.025)
    cih<-apply(pb,1,quantile,.975)
    pts<-cbind(pts,cil,cih)
  }
  ##densities
  xcenter<-mean(rt.lims)
  c(-.2,.2)->rt.lims
  dens<-list()
  for (resp in 0:1) {
    den<-density(x$rt[x$resp==resp])
    scale.factor<-.25
    m<-min(den$y)
    dy<-den$y-m
    M<-max(den$y)
    dy<-dy/M
    dy<-rt.lims[1]+scale.factor*dy*(rt.lims[2]-rt.lims[1])
    dens[[as.character(resp)]]<-cbind(den$x,dy)
  }
  list(pts=pts,dens=dens)
}
