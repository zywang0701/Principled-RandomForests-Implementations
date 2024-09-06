
post.procedure <- function(X, Y, X0){
  data = as.data.frame(cbind(X, Y))
  fmla = as.formula(paste('Y~', paste(colnames(X), collapse = '+')))
  fit = rpart(fmla, data=data, control=rpart.control(cp=0.0001))
  cptable = fit$cptable
  cp.opt.ind = which.min(cptable[,4])
  cp.opt = cptable[cp.opt.ind,1]
  fit = prune(fit, cp=cp.opt)
  fit.nodes = rpart.predict(fit, nn=TRUE)$nn
  term.nodes = unique(fit.nodes)
  mean.vec = sd.vec = rep(NA, length(term.nodes))
  names(mean.vec) = names(sd.vec) = term.nodes
  for(node in term.nodes){
    map = (1:n)[fit.nodes== node]
    y = Y[map]
    y.mean = mean(y)
    y.sd = sd(y)/sqrt(length(y))
    mean.vec[as.character(node)] = y.mean
    sd.vec[as.character(node)] = y.sd
  }
  
  Y0.fit = rpart.predict(fit, data.frame(X0), nn=TRUE)
  Y0.fit.val = (sapply(Y0.fit$nn, FUN=function(x) mean.vec[as.character(x)]))
  Y0.fit.sd = (sapply(Y0.fit$nn, FUN=function(x) sd.vec[as.character(x)]))
  CI0 = cbind(Y0.fit.val-qnorm(0.975)*Y0.fit.sd,
              Y0.fit.val+qnorm(0.975)*Y0.fit.sd)
  return(list(CI0=CI0,
              cp.opt=cp.opt))
}

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

# PRF.procedure <- function(X, Y, X0, M=1000){
#   
#   out = trainPRF(X, Y, M=M)
#   criterions = quantile(out$cor.vec, probs=c(0,0.8,0.9))
#   result1 = testPRF.step2(X0, out$trees.list, out$trees.info, out$pass.ind, filt=TRUE)
#   result2 = testPRF.step2(X0, out$trees.list, out$trees.info, out$pass.ind, filt=FALSE)
#   return(list(CI0.fil0=result1$CI0,
#               CI0.fil8=result2$CI0,
#               CI0.fil9=
#               pass.ind=out$pass.ind,
#               cp.opt=cp.opt))
# }

trainPRF <- function(X, Y, M=500){
  #### The function is aimed to construct individual trees, and test whether it pass filtering.
  
  ## specify colnames
  n = nrow(X)
  p = ncol(X)
  colnames(X) = paste0('X',1:p)
  
  cor.vec = rep(NA, M)
  trees.list = trees.info = rep(list(NA), M)
  for(m in 1:M){
    epsm = rnorm(n)
    Ym = Y - epsm
    data.m = as.data.frame(cbind(X, Y=Ym))
    fmla = as.formula(paste('Y~', paste(colnames(X), collapse = '+')))
    fit.m = rpart(fmla, data=data.m, control=rpart.control(cp=0.001))
    cp.m.opt = fit.m$cptable[which.min(fit.m$cptable[,4]),1]
    fit.m.prune = prune(fit.m, cp=cp.m.opt)
    pred = rpart.predict(fit.m.prune)
    cor.vec[m] = abs(sum((Y - pred)*epsm))/sqrt(sum((Y-pred)^2)*sum(epsm^2))
    
    fit.nodes = rpart.predict(fit.m.prune, nn=TRUE)$nn
    term.nodes = unique(fit.nodes)
    mean.vec = sd.vec = rep(NA, length(term.nodes))
    names(mean.vec) = names(sd.vec) = term.nodes
    for(node in term.nodes){
      map = (1:n)[fit.nodes== node]
      y = Ym[map]
      y.mean = mean(y)
      y.sd = sd(y)/sqrt(length(y))
      mean.vec[as.character(node)] = y.mean
      sd.vec[as.character(node)] = y.sd
    }
    
    trees.list[[m]] = fit.m.prune
    trees.info[[m]] = list(mean.vec = mean.vec, sd.vec = sd.vec)
  }

  return(list(trees.list = trees.list,
              trees.info = trees.info,
              cor.vec = cor.vec))
}

testPRF <- function(X0, trees.list, trees.info, pass.ind){
  n0 = nrow(X0)
  p = ncol(X0)
  colnames(X0) = paste0('X',1:p)
  M = length(trees.list)
  
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
  return(list(Y0.fit.val=Y0.fit.val, CI0 = CI0))
}
