library(MASS)
library(rpart)
library(glmnet)
library(rpart.plot)
library(intervals)
library(xgboost)
library(ranger)


fname <- file('stdin')
open(fname)
p = 10
n = as.numeric(readLines(fname, n=1)) # {500, 1000,2000,3000,4000,5000,6000,7000,8000}
simround = as.numeric(readLines(fname, n=1)) # {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20}
nsim = 10
close(fname)
filename = paste0('PRF-treatment_effect-p10-n',n,'-simround',simround,'.rds')
print(filename)

# Source functions ------
honest.procedure <- function(X, Y, X0){
  n = nrow(X)
  ind.A = sample(1:n, size=round(n/2), replace = F)
  ind.B = setdiff(1:n, ind.A)
  # Building Trees on A
  XA = X[ind.A,]
  YA = Y[ind.A]
  data = as.data.frame(cbind(XA, YA))
  colnames(data) = c(colnames(X), "Y")
  fmla = as.formula(paste('Y~', paste(colnames(X), collapse = '+')))
  fit = rpart(fmla, data=data, control=rpart.control(cp=0.0001, xval=20))
  cptable = fit$cptable
  cp.opt.ind = which.min(cptable[,4])
  cp.opt = cptable[cp.opt.ind,1]
  fit = prune(fit, cp=cp.opt)
  # Get inference on B
  XB = X[ind.B,]
  YB = Y[ind.B]
  colnames(XB) = colnames(X)
  fit.nodes = rpart.predict(fit, data.frame(XB), nn=TRUE)$nn
  term.nodes = unique(fit.nodes)
  mean.vec = sd.vec = rep(NA, length(term.nodes))
  names(mean.vec) = names(sd.vec) = term.nodes
  for(node in term.nodes){
    map = (1:nrow(XB))[fit.nodes== node]
    y = YB[map]
    y.mean = mean(y)
    y.sd = sd(y)/sqrt(length(y))
    mean.vec[as.character(node)] = y.mean
    sd.vec[as.character(node)] = y.sd
  }
  # Evaluation on Target
  Y0.fit = rpart.predict(fit, data.frame(X0), nn=TRUE)
  Y0.fit.val = (sapply(Y0.fit$nn, FUN=function(x) mean.vec[as.character(x)]))
  Y0.fit.sd = (sapply(Y0.fit$nn, FUN=function(x) sd.vec[as.character(x)]))
  CI0 = cbind(Y0.fit.val-qnorm(0.975)*Y0.fit.sd,
              Y0.fit.val+qnorm(0.975)*Y0.fit.sd)
  return(list(CI0=CI0,
              cp.opt=cp.opt))
}

# Main Content ------
cov0.mat.list = len0.mat.list = rep(list(NA), nsim)
for(isim in 1:nsim){
  print(isim)
  set.seed(isim+(simround-1)*nsim)
  # Number of partitions
  L = 3
  # Treat-structured CATE
  get.group = function(x){
    if(x[1]<=-0.5) return(1)
    if(x[1]>-0.5 & x[2]<=0) return(2)
    if(x[1]>-0.5 & x[2]>0) return(3)
  }
  group_mean = c(2, 0.2, -0.2)
  # Basis model
  eta <- function(X){
    0.5*X[,1] + X[,2] + 0.2*X[,3]^2
  }
  # Generate W via binomial distribution
  gamma = c(c(-2,-1,1,2)/5, rep(0,6))
  prob_cal <- function(X, b){
    val = exp(X%*%b)/(1+exp(X%*%b))
    return(as.vector(val))
  }
  
  # Training data
  X = matrix(runif(n*p, min=-1, max=1), nrow=n)
  colnames(X) = paste0('X',1:p)
  group = apply(X, 1, get.group)
  tau = group_mean[group]
  probX = prob_cal(X, gamma)
  W = rbinom(n, size=1, probX)
  Y = W * tau + eta(X) + rnorm(n)
  
  # Test data
  set.seed(0)
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
  tau0 = group_mean[group0]
  set.seed(NULL)
  
  
  ## Split data in half
  index_a = sample(1:n, size=round(n/2), replace=F)
  index_b = setdiff(1:n, index_a)
  Xa = X[index_a, ]; Wa = W[index_a]; Ya = Y[index_a]
  Xb = X[index_b, ]; Wb = W[index_b]; Yb = Y[index_b]
  
  # On dataset b, we estimate models mu0, mu1, pi
  logistic_model <- glm(Wb ~ . -1, data = as.data.frame(Xb), family = binomial)
  
  Xb_sub_1 = Xb[Wb == 1, ]; Yb_sub_1 = Yb[Wb == 1]
  Xb_sub_0 = Xb[Wb == 0, ]; Yb_sub_0 = Yb[Wb == 0]
  dtrain_1 <- xgb.DMatrix(data = Xb_sub_1, label = Yb_sub_1)
  dtrain_0 <- xgb.DMatrix(data = Xb_sub_0, label = Yb_sub_0)
  
  # Set XGBoost parameters
  params <- list(
    objective = "reg:squarederror",  # For regression tasks
    max_depth = 6,                   # Depth of trees
    eta = 0.1,                       # Learning rate
    eval_metric = "rmse"             # Use RMSE as evaluation metric
  )
  # Cross-validation for Wb == 1
  cv_results_1 <- xgb.cv(
    params = params,
    data = dtrain_1,
    nrounds = 100,
    nfold = 5,
    verbose = 1,
    early_stopping_rounds = 10,
    maximize = FALSE
  )
  best_nrounds_1 <- cv_results_1$best_iteration
  xgb_model_1 <- xgboost(
    params = params,
    data = dtrain_1,
    nrounds = best_nrounds_1,
    verbose = 0
  )
  # Cross-validation for Wb == 0
  cv_results_0 <- xgb.cv(
    params = params,
    data = dtrain_0,
    nrounds = 100,
    nfold = 5,
    verbose = 1,
    early_stopping_rounds = 10,
    maximize = FALSE
  )
  best_nrounds_0 <- cv_results_0$best_iteration
  xgb_model_0 <- xgboost(
    params = params,
    data = dtrain_0,
    nrounds = best_nrounds_0,
    verbose = 0
  )
  
  # Predict on Xa using both models
  piXa <- predict(logistic_model, newdata = as.data.frame(Xa), type = "response")
  mu1Xa <- predict(xgb_model_1, as.matrix(Xa))
  mu0Xa <- predict(xgb_model_0, as.matrix(Xa))
  
  # calculate RMSE, which will be set as noise level for generating synthetic noises
  calculate_rmse <- function(actual, predicted) {
    sqrt(mean((actual - predicted)^2))
  }
  pred_train_1 <- predict(xgb_model_1, as.matrix(Xb_sub_1))
  pred_train_0 <- predict(xgb_model_0, as.matrix(Xb_sub_0))
  rmse_1 <- calculate_rmse(Yb_sub_1, pred_train_1)
  rmse_0 <- calculate_rmse(Yb_sub_0, pred_train_0)
  sigma.hat = (rmse_1 + rmse_0) / 2
  
  # AIPW adjustment -----------
  Ya_AIPW = Wa/piXa*(Ya-mu1Xa) - (1-Wa)/(1-piXa)*(Ya-mu0Xa) + (mu1Xa - mu0Xa)
  
  # PRF ------
  cor.vec = rep(NA, M)
  trees.list = trees.info = rep(list(NA), M)
  for(m in 1:M){
    if(m%%50==1) print(m)
    eps.m = rnorm(round(n/2), sd = sigma.hat)
    Ya.m = Ya - eps.m
    Ya_AIPW.m = Wa/piXa*(Ya.m-mu1Xa) - (1-Wa)/(1-piXa)*(Ya.m-mu0Xa) + (mu1Xa - mu0Xa)
    data.m = as.data.frame(cbind(Xa, Ya_AIPW.m))
    colnames(data.m) = c(colnames(X),'Y')
    fmla = as.formula(paste('Y~', paste(colnames(Xa), collapse = '+')))
    fit.m = rpart(fmla, data=data.m, control=rpart.control(cp=0.0001))
    cp.m.opt = fit.m$cptable[which.min(fit.m$cptable[,4]),1]
    fit.m.prune = prune(fit.m, cp=cp.m.opt)
    pred = rpart.predict(fit.m.prune)
    cor.vec[m] = abs(sum((Ya_AIPW - pred)*eps.m))/sqrt(sum((Ya_AIPW-pred)^2)*sum(eps.m^2))
    
    fit.nodes = rpart.predict(fit.m.prune, nn=TRUE)$nn
    term.nodes = unique(fit.nodes)
    mean.vec = sd.vec = rep(NA, length(term.nodes))
    names(mean.vec) = names(sd.vec) = term.nodes
    for(node in term.nodes){
      map = (1:round(n/2))[fit.nodes== node]
      y = Ya_AIPW.m[map]
      y.mean = mean(y)
      y.sd = sd(y)/sqrt(length(y))
      mean.vec[as.character(node)] = y.mean
      sd.vec[as.character(node)] = y.sd
    }
    
    trees.list[[m]] = fit.m.prune
    trees.info[[m]] = list(mean.vec = mean.vec, sd.vec = sd.vec)
  }
  
  criterion = quantile(cor.vec, probs = 0.9)
  pass.ind = (1:M)[cor.vec >= criterion]
  pass.count = length(pass.ind)
  Y0.fit.val = Y0.fit.sd = matrix(NA, nrow=n0, ncol=pass.count)
  for(ipass in 1:pass.count){
    m = pass.ind[ipass]
    fit.m = trees.list[[m]]
    mean.vec = trees.info[[m]]$mean.vec
    sd.vec = trees.info[[m]]$sd.vec
    Y0.fit = rpart.predict(fit.m, data.frame(X0), nn=TRUE)
    Y0.fit.val[,ipass] = sapply(Y0.fit$nn, FUN=function(x) mean.vec[as.character(x)])
    Y0.fit.sd[,ipass] = sapply(Y0.fit$nn, FUN=function(x) sd.vec[as.character(x)])
  }
  CI0 <- rep(list(NA), n0)
  for(i0 in 1:n0){
    CI.mat = matrix(NA, nrow=pass.count, ncol=2); colnames(CI.mat) = c('lower','upper')
    for(ipass in 1:pass.count){
      CI.mat[ipass,] = c(Y0.fit.val[i0, ipass] - qnorm(0.975)*Y0.fit.sd[i0, ipass],
                         Y0.fit.val[i0, ipass] + qnorm(0.975)*Y0.fit.sd[i0, ipass])
    }
    CI.union = as.data.frame(interval_union(Intervals(na.omit(CI.mat))))
    CI0[[i0]] = CI.union
  }
  cov0.PRF = len0.PRF = rep(NA, n0)
  for(i0 in 1:n0){
    cov0.PRF[i0] = sum((CI0[[i0]][,1] <= tau0[i0])*(CI0[[i0]][,2] >= tau0[i0]))
    len0.PRF[i0] = sum(CI0[[i0]][,2] - CI0[[i0]][,1])
  }
  
  # Post-selection method -----
  result.hon = honest.procedure(Xa, Ya_AIPW, X0)
  cov0.hon = (result.hon$CI0[,1] <= tau0)*(result.hon$CI0[,2] >= tau0)
  len0.hon = result.hon$CI0[,2] - result.hon$CI0[,1]
  names(cov0.hon) = names(len0.hon) = NULL
  
  # grf method -----
  tau.forest <- causal_forest(X, Y, W, num.trees = M)
  tau.hat <- predict(tau.forest, X0, estimate.variance = TRUE)
  sigma.hat <- sqrt(tau.hat$variance.estimates)
  CI0.grf = cbind(tau.hat - qnorm(0.975) * sigma.hat, tau.hat + qnorm(0.975) * sigma.hat)
  cov0.grf = (CI0.grf[,1] <= tau0)*(CI0.grf[,2] >= tau0)
  len0.grf = CI0.grf[,2] - CI0.grf[,1]
  
  cov0.mat = cbind(cov0.hon, cov0.PRF, cov0.grf)
  len0.mat = cbind(len0.hon, len0.PRF, len0.grf)
  colnames(cov0.mat) = colnames(len0.mat) = c('post','PRF', 'GRF')
  cov0.mat.list[[isim]] = cov0.mat
  len0.mat.list[[isim]] = len0.mat
}

result <- list(cov0.mat.list = cov0.mat.list,
               len0.mat.list = len0.mat.list)
saveRDS(result, filename)