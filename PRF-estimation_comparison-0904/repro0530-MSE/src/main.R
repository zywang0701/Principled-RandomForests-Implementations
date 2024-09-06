library(rpart)
library(rpart.plot)
library(MASS)
library(intervals)
library(ranger)
library(intervals)
source('src/sources.R')

fname <- file('stdin')
open(fname)
n = as.numeric(readLines(fname, n=1)) # {500,1000,1500,2000,3000,4000,5000,8000}
simround = as.numeric(readLines(fname, n=1)) # {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20}
close(fname)
filename = paste0('repro-MSE-p10-n',n,'-simround',simround,'.rds')
print(filename)

nsim = 10
M = 1000
Y0.fit.list = rep(list(NA), nsim)
for(isim in 1:nsim){
  print(isim)
  set.seed(isim+(simround-1)*nsim)
  # Generate Data -----------------------------------------------------------
  L = 3
  get.group = function(x){
    if(x[1]<=-0.5) return(1)
    if(x[1]>-0.5 & x[2]<=0) return(2)
    if(x[1]>-0.5 & x[2]>0) return(3)
  }
  group_mean = c(2, 0.2, -0.2)
  
  # training data
  n = n
  p = 10
  X = matrix(runif(n*p, min=-1, max=1), nrow=n)
  colnames(X) = paste0('X',1:p,seq="")
  group = apply(X[1:n,], 1, get.group)
  Ystar = group_mean[group]
  eps = rnorm(n)
  Y = Ystar + eps
  
  n0 = 200
  X0 = rbind(# middle point in each subgroup
             c(-0.75, 0, runif(p-2, min=-1, max=1)), # 1st
             c(0.25, -0.5, runif(p-2, min=-1, max=1)), # 2nd
             c(0.25, 0.5, runif(p-2, min=-1, max=1)), # 3rd
             # boundary point of subgroup
             c(-0.55, -0.5, runif(p-2, min=-1, max=1)), # 1st & 2nd 
             c(-0.45, -0.5, runif(p-2, min=-1, max=1)), # 1st & 2nd
             c(0.25, 0.05, runif(p-2, min=-1, max=1)), # 2nd & 3rd
             c(0.25, -0.05, runif(p-2, min=-1, max=1)), # 2nd & 3rd
             c(-0.55, 0, runif(p-2, min=-1, max=1)), # 1st & 2nd & 3rd
             c(-0.45, -0.05, runif(p-2, min=-1, max=1)), # 1st & 2nd & 3rd
             c(-0.45, 0.05, runif(p-2, min=-1, max=1)), # 1st & 2nd & 3rd
             # far boundary point of subgroup
             c(-0.60, -0.5, runif(p-2, min=-1, max=1)), # 1st & 2nd
             c(-0.40, -0.5, runif(p-2, min=-1, max=1)), # 1st & 2nd
             c(0.25, 0.10, runif(p-2, min=-1, max=1)), # 2nd & 3rd
             c(0.25, -0.10, runif(p-2, min=-1, max=1)), # 2nd & 3rd
             c(-0.60, 0, runif(p-2, min=-1, max=1)), # 1st & 2nd & 3rd
             c(-0.40, -0.10, runif(p-2, min=-1, max=1)), # 1st & 2nd & 3rd
             c(-0.40, 0.10, runif(p-2, min=-1, max=1)), # 1st & 2nd & 3rd
             matrix(runif((n0-17)*p, min=-1, max=1), nrow=n0-17))
  colnames(X0)=paste0('X',1:p,seq="")
  group0 = apply(X0,1 ,get.group)
  Y0star = group_mean[group0]
  set.seed(NULL)
  
  ## Post ----
  print('-----> Post Procedure ----->')
  result.post = post.procedure(X, Y, X0)
  Y0.fit.post = result.post$Y0.fit.val
  
  ## Honest ----
  print('-----> Honest Procedure ----->')
  result.hon = honest.procedure(X, Y, X0)
  Y0.fit.hon = result.hon$Y0.fit.val
  
  ## PRF ----
  print('-----> PRF Procedure ----->')
  t1 = Sys.time()
  out.PRF = trainPRF(X, Y, M=M)
  criterions = quantile(out.PRF$cor.vec, probs = c(0, 0.7, 0.8, 0.9, 0.95))
  pass.ind0 = (1:M)[out.PRF$cor.vec >= criterions[1]]
  pass.ind7 = (1:M)[out.PRF$cor.vec >= criterions[2]]
  pass.ind8 = (1:M)[out.PRF$cor.vec >= criterions[3]]
  pass.ind9 = (1:M)[out.PRF$cor.vec >= criterions[4]]
  pass.ind95 = (1:M)[out.PRF$cor.vec >= criterions[5]]
  result.PRF0 = testPRF(X0, out.PRF$trees.list, out.PRF$trees.info, pass.ind0)
  result.PRF7 = testPRF(X0, out.PRF$trees.list, out.PRF$trees.info, pass.ind7)
  result.PRF8 = testPRF(X0, out.PRF$trees.list, out.PRF$trees.info, pass.ind8)
  result.PRF9 = testPRF(X0, out.PRF$trees.list, out.PRF$trees.info, pass.ind9)
  result.PRF95 = testPRF(X0, out.PRF$trees.list, out.PRF$trees.info, pass.ind95)
  
  Y0.fit.PRF9 = apply(result.PRF9$Y0.fit.val, MARGIN=1, FUN=mean)
  Y0.fit.PRF95 = apply(result.PRF95$Y0.fit.val, MARGIN=1, FUN=mean)
  t2 = Sys.time()
  print(t2-t1)
  
  ## ranger ----
  print('-----> rf Procedure ----->')
  data = as.data.frame(cbind(X, Y))
  fmla = as.formula(paste('Y~', paste(colnames(X), collapse = '+')))
  rf <- ranger(fmla,data=data,num.trees = M, keep.inbag = TRUE)
  pred = predict(rf, data=as.data.frame(X0), type='se', se.method='infjack')
  Y0.fit.RF = pred$predictions
  
  ## summary ----
  Y0.fit.list[[isim]] = cbind(Y0.fit.post, Y0.fit.hon, Y0.fit.PRF9, Y0.fit.PRF95, Y0.fit.RF)
}

result <- list(Y0.fit.list)
saveRDS(result, filename)
