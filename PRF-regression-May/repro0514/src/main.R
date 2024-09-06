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
filename = paste0('repro-illus-p10-n',n,'-simround',simround,'.rds')
print(filename)

nsim = 10
M = 1000
cov0.mat.list = len0.mat.list = rep(list(NA), nsim)
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
  cov0.post = (result.post$CI0[,1] <= Y0star)*(result.post$CI0[,2] >= Y0star)
  len0.post = result.post$CI0[,2] - result.post$CI0[,1]
  names(cov0.post) = names(len0.post) = NULL
  
  ## Honest ----
  print('-----> Honest Procedure ----->')
  result.hon = honest.procedure(X, Y, X0)
  cov0.hon = (result.hon$CI0[,1] <= Y0star)*(result.hon$CI0[,2] >= Y0star)
  len0.hon = result.hon$CI0[,2] - result.hon$CI0[,1]
  names(cov0.hon) = names(len0.hon) = NULL
  
  ## PRF ----
  print('-----> PRF Procedure ----->')
  t1 = Sys.time()
  out.PRF = trainPRF(X, Y, M=M)
  criterions = quantile(out.PRF$cor.vec, probs = c(0, 0.7, 0.8, 0.9))
  pass.ind0 = (1:M)[out.PRF$cor.vec >= criterions[1]]
  pass.ind8 = (1:M)[out.PRF$cor.vec >= criterions[2]]
  pass.ind9 = (1:M)[out.PRF$cor.vec >= criterions[3]]
  pass.ind95 = (1:M)[out.PRF$cor.vec >= criterions[3]]
  result.PRF0 = testPRF(X0, out.PRF$trees.list, out.PRF$trees.info, pass.ind0)
  result.PRF8 = testPRF(X0, out.PRF$trees.list, out.PRF$trees.info, pass.ind8)
  result.PRF9 = testPRF(X0, out.PRF$trees.list, out.PRF$trees.info, pass.ind9)
  result.PRF95 = testPRF(X0, out.PRF$trees.list, out.PRF$trees.info, pass.ind9)
  
  cov0.PRF0 = len0.PRF0 = cov0.PRF8 = len0.PRF8 = cov0.PRF9 = len0.PRF9 = cov0.PRF95 = len0.PRF95 = rep(NA, n0)
  for(i0 in 1:n0){
    cov0.PRF0[i0] = sum((result.PRF0$CI0[[i0]][,1] <= Y0star[i0])*(result.PRF0$CI0[[i0]][,2] >= Y0star[i0]))
    len0.PRF0[i0] = sum(result.PRF0$CI0[[i0]][,2] - result.PRF0$CI0[[i0]][,1])
    cov0.PRF8[i0] = sum((result.PRF8$CI0[[i0]][,1] <= Y0star[i0])*(result.PRF8$CI0[[i0]][,2] >= Y0star[i0]))
    len0.PRF8[i0] = sum(result.PRF8$CI0[[i0]][,2] - result.PRF8$CI0[[i0]][,1])
    cov0.PRF9[i0] = sum((result.PRF9$CI0[[i0]][,1] <= Y0star[i0])*(result.PRF9$CI0[[i0]][,2] >= Y0star[i0]))
    len0.PRF9[i0] = sum(result.PRF9$CI0[[i0]][,2] - result.PRF9$CI0[[i0]][,1])
    cov0.PRF95[i0] = sum((result.PRF95$CI0[[i0]][,1] <= Y0star[i0])*(result.PRF95$CI0[[i0]][,2] >= Y0star[i0]))
    len0.PRF95[i0] = sum(result.PRF95$CI0[[i0]][,2] - result.PRF95$CI0[[i0]][,1])
  }
  t2 = Sys.time()
  print(t2-t1)
  
  ## ranger ----
  print('-----> rf Procedure ----->')
  data = as.data.frame(cbind(X, Y))
  fmla = as.formula(paste('Y~', paste(colnames(X), collapse = '+')))
  rf <- ranger(fmla,data=data,num.trees = M, keep.inbag = TRUE)
  pred = predict(rf, data=as.data.frame(X0), type='se', se.method='infjack')
  ci0.rf = cbind(pred$predictions-qnorm(0.975)*pred$se,
                 pred$predictions+qnorm(0.975)*pred$se)
  cov0.rf = (ci0.rf[,1] <= Y0star)*(ci0.rf[,2] >= Y0star)
  len0.rf = ci0.rf[,2] - ci0.rf[,1]
  
  ## summary ----
  cov0.mat = cbind(cov0.post, cov0.hon, cov0.PRF0, cov0.PRF8, cov0.PRF9, cov0.PRF95, cov0.rf)
  len0.mat = cbind(len0.post, len0.hon, len0.PRF0, len0.PRF8, len0.PRF9, len0.PRF95, len0.rf)
  colnames(cov0.mat) = colnames(len0.mat) = c('post','honest','PRF0', 'PRF8', 'PRF9', 'PRF95', 'rf')
  
  cov0.mat.list[[isim]] = cov0.mat
  len0.mat.list[[isim]] = len0.mat
}

result <- list(cov0.mat.list = cov0.mat.list,
               len0.mat.list = len0.mat.list)
saveRDS(result, filename)
