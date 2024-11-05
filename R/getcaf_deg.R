getcaf_deg= function (x, deg=2) 
{
  list.of.packages=c('fixest', 'splines')
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  library(splines)
  tmp.bs <- bs(x$rt, degree = deg)
  for (i in 1:ncol(tmp.bs)) x[[paste("bs", i, sep = "")]] <- tmp.bs[, 
                                                                    i]
  expr= ""
  for (i in 1:deg) expr = paste0(expr, "bs", i, " +")
  expr = substr(expr,1,nchar(expr)-2)
  caf <- list()
  library(fixest)
  #ran <- quantile(x$rt, c(0.01, 0.99), na.rm = TRUE)
  ran= c(min(x$rt), max(x$rt))
  for (i in 1:2) {
    xx <- x[x$group == i, ]
    regexp = as.formula(paste("resp ~", expr, "| item+id"))
    mod= feols(regexp, xx)
    #mod <- feols(resp ~ bs1 + bs2 + bs3 + bs4 | item + id, xx)
    t <- seq(ran[1], ran[2], length.out = 1000)
    z <- predict(tmp.bs, t)
    fe <- fixest::fixef(mod)
    z=data.frame(z)
    #for (i in 1:ncol(tmp.bs)) z[[paste("bs", i, sep = "")]] <- z[,i] 
    #z <- data.frame(bs1 = z[, 1], bs2= z[, 2], bs3= z[, 3], bs4= z[, 4])
    co <- coef(mod)
    yhat <- 0
    for (j in 1:length(co)) yhat <- yhat + co[j] * z[, j]
    yhat <- yhat + mean(fe$item) + mean(fe$id)
    caf[[i]] <- data.frame(t = t, z, yhat = yhat)
  }
  caf[[1]] <- cbind(caf[[1]]$t, caf[[1]]$yhat)
  caf[[2]] <- cbind(caf[[2]]$t, caf[[2]]$yhat)
  caf
}

