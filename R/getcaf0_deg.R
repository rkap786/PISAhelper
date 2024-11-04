
getcaf0_deg= function (x, deg=2) 
{
  library(splines)
  tmp.bs <- bs(x$rt, degree=deg)
  for (i in 1:ncol(tmp.bs)) x[[paste("bs", i, sep = "")]] <- tmp.bs[, 
                                                                    i]
  expr= ""
  for (i in 1:deg) expr = paste0(expr, "bs", i, " +")
  expr = substr(expr,1,nchar(expr)-2)
  caf <- list()
  library(fixest)
  ran <- quantile(x$rt, c(0.01, 0.99), na.rm = TRUE)
  xx <- x
  regexp = as.formula(paste("resp ~", expr, "| item+id"))
  mod= feols(regexp, xx)
  #mod <- feols(resp ~ bs1 + bs2 + bs3 + bs4 | item + id, xx)
  t <- seq(ran[1], ran[2], length.out = 1000)
  z <- predict(tmp.bs, t)
  fe <- fixest::fixef(mod)
  z=data.frame(z)
  #z <- data.frame(bs1 = z[, 1], bs2= z[, 2], bs3= z[, 3], bs4= z[, 4])
  co <- coef(mod)
  yhat <- 0
  for (j in 1:length(co)) yhat <- yhat + co[j] * z[, j]
  yhat <- yhat + mean(fe$item) + mean(fe$id)
  caf <- data.frame(t = t, z, yhat = yhat)
  caf <- cbind(caf$t, caf$yhat)
  caf
  # 
  # yhat <- as.matrix(z) %*% matrix(co, ncol = 1)
  # yhat <- yhat[, 1] - mean(yhat[, 1])
  # data.frame(t = t, yhat = yhat)
}