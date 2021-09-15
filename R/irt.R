# ######### IRT
#  
#?ranef
# x=pisa_gender %>%
#   filter(gender==1, subject=="read")
irt<-function(x,lmer.flag=FALSE ,model="2PL") {
  if (lmer.flag) { ##lmer
    library(lme4)
    m<-glmer(resp~0+(1|item)+(1|id),x,family="binomial")
    #m<-lmer(resp~0+(1|item)+(1|id),x)
    ranef(m)$item->fe
    item<-data.frame(item=rownames(fe),diff=-1*fe[,1])
    re<-ranef(m)$id
    stud<-data.frame(id=rownames(re),th=re[,1])
    x<-merge(x,item)
    x<-merge(x,stud)
  } else { ##mirt
    ##muck with item names
    
    nms<-unique(x$item)
    if (all(nms %in% 1:length(nms))) x$item<-paste("item_",x$item,sep='')
    ##make response matrix
    id<-unique(x$id)
    L<-split(x,x$item)
    out<-list()
    for (i in 1:length(L)) {
      z<-L[[i]]
      index<-match(z$id,id)
      resp<-rep(NA,length(id))
      resp[index]<-z$resp
      out[[i]]<-resp
    }
    resp<-do.call("cbind",out)
    resp<-data.frame(resp)
    names(resp)<-names(L)
    resp$id<-id
    nr<-apply(resp,2,function(x) length(table(x)))
    resp<-resp[,nr>1]
    ##
    library(mirt)
    index<-grep('id',names(resp))
    
    # x %>%
    #   group_by(item) %>%
    #   summarise(mean=mean(resp, na.rm = T)) %>%
    #   filter(mean>0.98)
    # 
    #problematic - 
    s <- 'F = 1-100
      PRIOR = (1-100, a1, lnorm, 0, 2), ((1-100, d, norm, 0, 2))'
    model=mirt.model(s)
    
        m<-mirt(resp[,-index],1,itemtype = "2PL",
            model=model,
            technical = list(NCYCLES = 200)
            )
    
    
    
    co<-coef(m)
    co<-do.call("rbind",co[-length(co)])
    item<-data.frame(item=names(resp)[-index],diff=-1*co[,2])
    ##
    th<-fscores(m)
    stud<-data.frame(id=resp$id,th=th[,1])
    ##
    x<-merge(x,stud)
    x<-merge(x,item)
  }
  ##
  kk<-x$th-x$diff
  kk<-exp(kk)
  x$pv<-kk/(1+kk)
  ##
  return(x)
  
 }
