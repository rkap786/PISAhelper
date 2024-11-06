## takes in dataset and degrees for the b spline
## Returns score difference: Group 1 - Group2
## Adjsted diff A_c: Group 1 - Group 2
## Adjsted diff A_s: Group 1 - Group 2
# x should contain: rt (time), group (group name), resp (score)


scorediff = function(x, deg=2) {
  
  library(PISAhelper)
  # source("/Users/radhika/Google Drive Stanford/PISA gender/Final code files/Code/PISAhelper-additional/R/getcaf_deg.R")
  # source("/Users/radhika/Google Drive Stanford/PISA gender/Final code files/Code/PISAhelper-additional/R/getcaf0_deg.R")
  
  ##### estimate same CAF, density for group 1
  
  #x$group= ifelse(x$gender==1,2,1)
  x = x |>
    filter(!is.na(rt),
           !is.na(group)) 
  # |>
  #   group_by(item) |>
  #   mutate(rt = scale(rt), na.rm=T) |>
  #   ungroup()


  
  x1= x[x$group==1,]
  
  x2=x[x$group==2,]
  # summary(x1$rt)
  # summary(x2$rt)
  M<-by(x$resp,x$group,mean)
  ##### hold time density the same, different group CAFs
  # caf.tmp<-data.frame(t=caf[[2]][,1],yhat=caf[[2]][,2])
  # intdiff<-integrate(x$rt,caf.tmp)
  #print(M)
  #print(intdiff) #0.4766381
  
  
  caf = getcaf_deg(x, deg=2)
  caf0= getcaf0_deg(x,deg=2)
  #caf= getcafspline(x)
  #caf0= getcafspline0(x)
  
  x1= x1 |>filter(!is.na(rt))
  x2= x2 |>filter(!is.na(rt))
  x= x |>filter(!is.na(rt))
  
  ##### time density of group 1, CAF for group 2 (t1h2) 
  # caf.tmp<-data.frame(t=caf[[2]][,1],yhat=caf[[2]][,2])
  # intdiff1<-integrate(x1$rt,caf.tmp)
  # intdiff_t1h2=intdiff1
  # # 
  ##### time density of group 2, CAF for group 2 (t2h2)
  caf.tmp<-data.frame(t=caf[[2]][,1],yhat=caf[[2]][,2])
  intdiff2<-integrate(x2$rt,caf.tmp)
  intdiff_t2h2=intdiff2
  # 
  
  ##### merged time density, CAF for group 2 (th2)  
  caf.tmp<-data.frame(t=caf[[2]][,1],yhat=caf[[2]][,2])
  intdiff3<-integrate(x$rt,caf.tmp)
  intdiff_th2=intdiff3
  
  ##### merged time density, CAF for group 1 (th1)
  caf.tmp<-data.frame(t=caf[[1]][,1],yhat=caf[[1]][,2])
  intdiff4<-integrate(x$rt,caf.tmp)
  intdiff_th1= intdiff4
  
  # ##### time density of group 1, CAF for group 1 (t1h1)
  caf.tmp<-data.frame(t=caf[[1]][,1],yhat=caf[[1]][,2])
  intdiff5<-integrate(x1$rt,caf.tmp)
  intdiff_t1h1= intdiff5
  # # 
  # 
  ##### time density of group 2, CAF for group 1 (t2h1)
  # caf.tmp<-data.frame(t=caf[[1]][,1],yhat=caf[[1]][,2])
  # intdiff6<-integrate(x2$rt,caf.tmp)
  # intdiff_t2h1=intdiff6
  # 
  #### time density of group 1, merged CAF (t1h)
  caf.tmp<-data.frame(t=caf0[,1],yhat=caf0[,2])
  intdiff7<-integrate(x1$rt,caf.tmp)
  intdiff_t1h=intdiff7
  
  #### time density of group 2, merged CAF (t2h)
  caf.tmp<-data.frame(t=caf0[,1],yhat=caf0[,2])
  intdiff8<-integrate(x2$rt,caf.tmp)
  intdiff_t2h=intdiff8
  
  #a1 = intdiff_t1h2 - intdiff_t1h1  
  #a2 = intdiff_t2h1 - intdiff_t1h1
  a3c = intdiff_th2 - intdiff_th1
  a4s= intdiff_t2h - intdiff_t1h
  score_diff = M[[2]]-M[[1]]
  #est_diff = intdiff_t2h2 - intdiff_t1h1
  
  return(c("actualdiff"=score_diff, "a_c"=a3c, "a_s"=a4s))
}

