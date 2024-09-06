library(ggplot2)
library(reshape2)
library(ggpubr)
library(latex2exp)

# good to show: 3,10,17
p = 10
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

n.vec=c(500, 1000,2000,3000,4000,5000,6000,7000,8000)
sim.round.vec = 1:20
cov.summ = len.summ = matrix(NA, nrow=length(n.vec), ncol=8)
colnames(cov.summ) = colnames(len.summ) = c('n','post','honest','PRF0','PRF7','PRF8','PRF9','rfci')

show_cov <- function(pick){
  for(i.n in 1:length(n.vec)){
    countfile = 0
    cov.summ.per = len.summ.per = matrix(NA, nrow=200, ncol=7)
    colnames(cov.summ.per) = colnames(len.summ.per) = c('post','honest','PRF0','PRF7','PRF8','PRF9','rfci')
    for(sim.round in sim.round.vec){
      n = n.vec[i.n]
      filename = paste0('repro0514/repro-illus-p10-n',n,'-simround',sim.round,'.rds')
      if(file.exists(filename)){
        countfile = countfile + 1
        result = readRDS(filename)
        
        for(isim in 1:10){
          simidx = (sim.round-1)*10 + isim
          cov.summ.per[simidx,] = result$cov0.mat.list[[isim]][pick,]
          len.summ.per[simidx,] = result$len0.mat.list[[isim]][pick,]
        }
      }
    }
    cov.summ[i.n,] = c(n, colMeans(na.omit(cov.summ.per)))
    len.summ[i.n,] = c(n, colMeans(na.omit(len.summ.per)))
  }
  return(list(cov=cov.summ, len=round(len.summ,4)))
}
show_cov(5)

# coverage and length -----------------------------------------------------
draw_plot <- function(pick){
  cov.summ = len.summ = matrix(NA, nrow=length(n.vec), ncol=5)
  colnames(cov.summ) = colnames(len.summ) = c('n','Post','PRF0','PRF9','RFci')
  for(i.n in 1:length(n.vec)){
    countfile = 0
    cov.summ.per = len.summ.per = matrix(NA, nrow=200, ncol=4)
    colnames(cov.summ.per) = colnames(len.summ.per) = c('Post','PRF0','PRF9','RFci')
    for(sim.round in sim.round.vec){
      n = n.vec[i.n]
      filename = paste0('repro0514/repro-illus-p10-n',n,'-simround',sim.round,'.rds')
      if(file.exists(filename)){
        countfile = countfile + 1
        result = readRDS(filename)
        
        for(isim in 1:10){
          simidx = (sim.round-1)*10 + isim
          cov.summ.per[simidx,] = result$cov0.mat.list[[isim]][pick,c(2,3,6,7)]
          len.summ.per[simidx,] = result$len0.mat.list[[isim]][pick,c(2,3,6,7)]
        }
      }
    }
    cov.summ[i.n,] = c(n, colMeans(na.omit(cov.summ.per)))
    len.summ[i.n,] = c(n, colMeans(na.omit(len.summ.per)))
  }
  
  
  df.cov = melt(data.frame(cov.summ),id.vars = 'n')
  p.cov = ggplot(df.cov, aes(x=n, y=value))+
    geom_point(aes(shape=factor(variable, levels=c('PRF9','PRF0','Post','RFci')),
                   color=factor(variable, levels=c('PRF9','PRF0','Post','RFci'))), size=2.5)+
    geom_line(aes(color=variable))+
    geom_hline(yintercept = 0.95, linetype="dashed", color = "black", linewidth=1)+
    labs(x="n: sample size", y="Empirical Coverage", title='')+
    scale_color_brewer(palette = 'Set1')+
    theme_minimal()+
    ylim(c(0.3, 1))+
    theme(legend.title=element_blank(),
          plot.title = element_blank(),#element_text(hjust = 0.5),
          text = element_text(size=10))
  
  df.len = melt(data.frame(len.summ),id.vars = 'n')
  p.len = ggplot(df.len, aes(x=n, y=value))+
    geom_point(aes(shape=factor(variable, levels=c('PRF9','PRF0','Post','RFci')),
                   color=factor(variable, levels=c('PRF9','PRF0','Post','RFci'))), size=2.5)+
    geom_line(aes(color=variable))+
    labs(x="n: sample size", y="Average Length", title='')+
    scale_color_brewer(palette = 'Set1')+
    #ylim(c(0, 5))+
    theme_minimal()+
    theme(legend.title=element_blank(),
          plot.title = element_blank(),#plot.title = element_text(hjust = 0.5),
          text = element_text(size=10))
  # point_entry = switch (pick,
  #   "3" = c(0.25,0.5),
  #   "10" = c(-0.45,0.05),
  #   "17" = c(-0.4,0.1)
  # )
  if(pick==3) point_entry=c(0.25, 0.5)
  if(pick==10) point_entry=c(-0.45, 0.05)
  if(pick==17) point_entry=c(-0.4,0.1)
  p = ggarrange(p.cov, p.len, ncol=1, common.legend = T, legend = 'bottom')
  pa = annotate_figure(p, top = text_grob(TeX(sprintf(r'($x_{0,1} = %.2f$ and $x_{0,2}=%.2f$)', point_entry[1], point_entry[2])), color = "black", face = "bold", size = 14))
  # if(pick==1){
  #   p = ggarrange(p.cov, p.len, nrow=1, common.legend = T, legend = 'none')
  #   pa = annotate_figure(p, top = text_grob(TeX('Test Data Point $X_0$ with first two entries $(0.5,0.6)$'), color = "#377EB8", face = "bold", size = 14))
  # }
  # else if(pick==7){
  #   p = ggarrange(p.cov, p.len, nrow=1, common.legend = T, legend = 'none')
  #   pa = annotate_figure(p, top = text_grob(TeX('Test Data Point $X_0$ with first two entries $(-0.5,-0.2)$'), color = "#377EB8", face = "bold", size = 14))
  # }
  # else if(pick==6){
  #   p = ggarrange(p.cov, p.len, nrow=1, common.legend = T, legend = 'bottom')
  #   pa = annotate_figure(p, top = text_grob(TeX('Test Data Point $X_0$ with first two entries $(0,0)$'), color = "#377EB8", face = "bold", size = 14))
  # }
  # else{
  #   p = ggarrange(p.cov, p.len, nrow=1, common.legend = T, legend = 'bottom')
  #   pa = p
  # }
  return(pa)
}

pa = draw_plot(pick=3)
pb = draw_plot(pick=17)
pc = draw_plot(pick=10)

ggarrange(pa,pb, pc, ncol=3)


