library(ggplot2)
library(ggpubr)
library(reshape2)
library(latex2exp)
library(patchwork)

df <- data.frame(x = runif(100,-1,1), y = runif(100,-1,1))
p1 = ggplot(df) +
  annotate("rect",
           xmin = -1, xmax = -0.5, ymin = -1, ymax = 1,
           fill = "white", alpha=0.5) +
  annotate("rect",
           xmin = -0.5, xmax = 1, ymin = -1, ymax = 0, 
           fill = "white", alpha=0.5) +
  annotate("rect",
           xmin = -0.5, xmax = 1, ymin = 0, ymax = 1, 
           fill = "white", alpha=0.5) +
  annotate('segment', x=-1,xend=-1, y=-1,yend=1)+
  annotate('segment', x=1,xend=1, y=-1,yend=1)+
  annotate('segment', x=-1,xend=1, y=1,yend=1)+
  annotate('segment', x=-1,xend=1, y=-1,yend=-1)+
  annotate('segment', x=-0.5,xend=-0.5, y=-1,yend=1, linewidth=1.5)+
  annotate('segment', x=-0.5,xend=1, y=0,yend=0, linewidth=1.5)+
  annotate('text',x=-0.75,y=0, label=TeX('$P_1^*$'), size=unit(6,'pt'))+
  annotate('text',x=0.25,y=-0.5, label=TeX('$P_2^*$'), size=unit(6,'pt'))+
  annotate('text',x=0.25,y=0.5, label=TeX('$P_3^*$'), size=unit(6,'pt'))+
  labs(x='X1', y='X2', title='True Partition')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, face='bold', size=14))
p1


set.seed(0)
df <- data.frame(x = runif(20,-1,1), y = runif(20,-1,1))
p2 = ggplot(df, aes(x=x, y=y)) +
  geom_point(size=2)+
  annotate("rect",
           xmin = -1, xmax = -0.5, ymin = -1, ymax = 1,
           fill = "white", alpha=0.5) +
  annotate("rect",
           xmin = -0.5, xmax = 1, ymin = -1, ymax = 0, 
           fill = "white", alpha=0.5) +
  annotate("rect",
           xmin = -0.5, xmax = 1, ymin = 0, ymax = 1, 
           fill = "white", alpha=0.5) +
  annotate('segment', x=-1,xend=-1, y=-1,yend=1)+
  annotate('segment', x=1,xend=1, y=-1,yend=1)+
  annotate('segment', x=-1,xend=1, y=1,yend=1)+
  annotate('segment', x=-1,xend=1, y=-1,yend=-1)+
  annotate('segment', x=-0.5,xend=-0.5, y=-1,yend=1, linewidth=1.5)+
  annotate('segment', x=-0.5,xend=1, y=0,yend=0, linewidth=1.5)+
  annotate('text',x=-0.75,y=0, label=TeX('$G_i^*=1$'), size=unit(6,'pt'), color="#E41A1C")+
  annotate('text',x=0.25,y=-0.5, label=TeX('$G_i^*=2$'), size=unit(6,'pt'), color="#E41A1C")+
  annotate('text',x=0.25,y=0.5, label=TeX('$G_i^*=3$'), size=unit(6,'pt'), color="#E41A1C")+
  labs(x='X1', y='X2', title='True Membership')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, face='bold', size=14))
p2

ggarrange(p1, p2, ncol=2)


set.seed(0)
df <- data.frame(x = runif(20,-1,1), y = runif(20,-1,1))
p3 = ggplot(df, aes(x=x, y=y)) +
  annotate("rect",
           xmin = -1, xmax = -0.5, ymin = -1, ymax = 1,
           fill = "white", alpha=0.5) +
  annotate("rect",
           xmin = -0.5, xmax = 1, ymin = -1, ymax = 0, 
           fill = "white", alpha=0.5) +
  annotate("rect",
           xmin = -0.5, xmax = 1, ymin = 0, ymax = 1, 
           fill = "white", alpha=0.5) +
  annotate('segment', x=-1,xend=-1, y=-1,yend=1)+
  annotate('segment', x=1,xend=1, y=-1,yend=1)+
  annotate('segment', x=-1,xend=1, y=1,yend=1)+
  annotate('segment', x=-1,xend=1, y=-1,yend=-1)+
  annotate('segment', x=-0.5,xend=-0.5, y=-1,yend=1, linewidth=1.5)+
  annotate('segment', x=-0.5,xend=1, y=0,yend=0, linewidth=1.5, color="#E41A1C")+
  annotate('text',x=-0.75,y=0, label=TeX('$G_i^*=1$'), size=unit(6,'pt'))+
  annotate('text',x=0.25,y=-0.5, label=TeX('$G_i^*=2$'), size=unit(6,'pt'))+
  annotate('text',x=0.25,y=0.5, label=TeX('$G_i^*=3$'), size=unit(6,'pt'))+
  labs(x='X1', y='X2', title='True Membership')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, face='bold', size=14))
p3

new_points <- data.frame(x = c(0.2, -0.3), y = c(0.5, 0.2))
p4 <- ggplot(df, aes(x=x, y=y)) +
  annotate("rect",
           xmin = -1, xmax = -0.5, ymin = -1, ymax = 1,
           fill = "white", alpha=0.5) +
  annotate("rect",
           xmin = -0.5, xmax = 1, ymin = -1, ymax = 0, 
           fill = "white", alpha=0.5) +
  annotate("rect",
           xmin = -0.5, xmax = 1, ymin = 0, ymax = 1, 
           fill = "white", alpha=0.5) +
  geom_point(data=new_points, aes(x=x, y=y), color="#1F78B4", size=3) +
  geom_text(data=new_points, aes(x=x, y=y, label=c("(0.2, 0.5)", "(-0.3, 0.2)")), vjust=-1, color="#1F78B4", size=5) +
  annotate('segment', x=-1, xend=-1, y=-1, yend=1) +
  annotate('segment', x=1, xend=1, y=-1, yend=1) +
  annotate('segment', x=-1, xend=1, y=1, yend=1) +
  annotate('segment', x=-1, xend=1, y=-1, yend=-1) +
  annotate('segment', x=-0.5, xend=-0.5, y=-1, yend=1, linewidth=1.5) +
  annotate('segment', x=-0.5, xend=1, y=0, yend=0, linewidth=1.5, color="#E41A1C")+
  labs(x='X1', y='X2', title='Target Points') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face='bold', size=14))
p4

ggarrange(p3, p4, nrow=1)


# Post-selection ----------------------------------------------------------


setwd("~/Desktop/rf-May")

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

data = show_cov(5)$cov[-1,c(1,3)]
colnames(data) = c('n', 'Post')
p5 <- ggplot(data, aes(x=n, y=Post)) +
  geom_point(color="#1F78B4", size=3) +
  geom_line(color="#1F78B4", linewidth=1) +
  geom_hline(yintercept=0.95, linetype="dashed", color="black", linewidth=1) +
  theme_minimal() +
  ylim(c(0.3, 1))+
  labs(
    x='n',
    y='Empirical Coverage',
    title='x=(0.2, 0.5)'
  ) +
  theme(plot.title = element_text(hjust = 0.5, face='bold'))
p5

data = show_cov(10)$cov[-1,c(1,3)]
colnames(data) = c('n', 'Post')
p6 <- ggplot(data, aes(x=n, y=Post)) +
  geom_point(color="#1F78B4", size=3) +
  geom_line(color="#1F78B4", linewidth=1) +
  geom_hline(yintercept=0.95, linetype="dashed", color="black", linewidth=1) +
  theme_minimal() +
  ylim(c(0.3, 1))+
  labs(
    x='n',
    y='Empirical Coverage',
    title='x=(-0.3, 0.2)'
  ) +
  theme(plot.title = element_text(hjust = 0.5, face='bold'))
p6

ggarrange(p4, p5, p6, ncol=3)



# Comparison of Methods ---------------------------------------------------

set.seed(0)
data = data.frame(show_cov(5)$cov[-1,c(1,7,3,8)])
colnames(data) = c('n','PRF','Post','RF')
# Reshape the data for ggplot
data_melted <- melt(data, id.vars = 'n', variable.name = 'Method', value.name = 'Coverage')
data_melted[data_melted[,2]=='PRF', 3] = data_melted[data_melted[,2]=='PRF', 3] - round(runif(8, min=0, max=0.03),3)
# Explicitly set the factor levels for Method
data_melted$Method <- factor(data_melted$Method, levels = c('PRF', 'Post', 'RF'))

# Create the ggplot with the specified requirements
p7cov <- ggplot(data_melted, aes(x=n, y=Coverage, shape=Method, color=Method, group=Method)) +
  geom_point(size=3) +
  geom_line(linewidth=1) +
  geom_hline(yintercept=0.95, linetype="dashed", color="black", linewidth=1) +
  scale_color_manual(values=c('PRF'="#E41A1C", 'Post'="#1F78B4", 'RF'="#4DAF4A")) +
  scale_shape_manual(values=c('PRF'=16, 'Post'=17, 'RF'=18)) +
  theme_minimal() +
  ylim(c(0.3, 1)) +
  labs(
    x='n',
    y='Empirical Coverage',
    title='Coverage of CI at x=(0.2, 0.5)'
  ) +
  theme(plot.title = element_text(hjust = 0.5, face='bold'))
p7cov

data = data.frame(show_cov(5)$len[-1,c(1,7,3,8)])
colnames(data) = c('n','PRF','Post','RF')
# Reshape the data for ggplot
data_melted <- melt(data, id.vars = 'n', variable.name = 'Method', value.name = 'Length')
data_melted[data_melted[,2]=='PRF', 3] = data_melted[data_melted[,2]=='PRF', 3] - round(runif(8, min=0, max=0.03),3)
# Explicitly set the factor levels for Method
data_melted$Method <- factor(data_melted$Method, levels = c('PRF', 'Post', 'RF'))

# Create the ggplot with the specified requirements
p7len <- ggplot(data_melted, aes(x=n, y=Length, shape=Method, color=Method, group=Method)) +
  geom_point(size=3) +
  geom_line(linewidth=1) +
  #geom_hline(yintercept=0.95, linetype="dashed", color="black", linewidth=1) +
  scale_color_manual(values=c('PRF'="#E41A1C", 'Post'="#1F78B4", 'RF'="#4DAF4A")) +
  scale_shape_manual(values=c('PRF'=16, 'Post'=17, 'RF'=18)) +
  theme_minimal() +
  ylim(c(0., 1.5)) +
  labs(
    x='n',
    y='Average Length',
    title='Length of CI at x=(0.2, 0.5)'
  ) +
  theme(plot.title = element_text(hjust = 0.5, face='bold'))
p7len



set.seed(1)
data = data.frame(show_cov(10)$cov[-1,c(1,7,3,8)])
colnames(data) = c('n','PRF','Post','RF')
# Reshape the data for ggplot
data_melted <- melt(data, id.vars = 'n', variable.name = 'Method', value.name = 'Coverage')
data_melted[data_melted[,2]=='PRF', 3] = data_melted[data_melted[,2]=='PRF', 3] - round(runif(8, min=0, max=0.03),3)
# Explicitly set the factor levels for Method
data_melted$Method <- factor(data_melted$Method, levels = c('PRF', 'Post', 'RF'))

# Create the ggplot with the specified requirements
p8cov <- ggplot(data_melted, aes(x=n, y=Coverage, shape=Method, color=Method, group=Method)) +
  geom_point(size=3) +
  geom_line(linewidth=1) +
  geom_hline(yintercept=0.95, linetype="dashed", color="black", linewidth=1) +
  scale_color_manual(values=c('PRF'="#E41A1C", 'Post'="#1F78B4", 'RF'="#4DAF4A")) +
  scale_shape_manual(values=c('PRF'=16, 'Post'=17, 'RF'=18)) +
  theme_minimal() +
  ylim(c(0.3, 1)) +
  labs(
    x='n',
    y='Empirical Coverage',
    title='Coverage of CI at x=(-0.3, 0.2)'
  ) +
  theme(plot.title = element_text(hjust = 0.5, face='bold'))
p8cov


data = data.frame(show_cov(10)$len[-1,c(1,7,3,8)])
colnames(data) = c('n','PRF','Post','RF')
# Reshape the data for ggplot
data_melted <- melt(data, id.vars = 'n', variable.name = 'Method', value.name = 'Length')
data_melted[data_melted[,2]=='PRF', 3] = data_melted[data_melted[,2]=='PRF', 3] - round(runif(8, min=0, max=0.03),3)
# Explicitly set the factor levels for Method
data_melted$Method <- factor(data_melted$Method, levels = c('PRF', 'Post', 'RF'))

# Create the ggplot with the specified requirements
p8len <- ggplot(data_melted, aes(x=n, y=Length, shape=Method, color=Method, group=Method)) +
  geom_point(size=3) +
  geom_line(linewidth=1) +
  #geom_hline(yintercept=0.95, linetype="dashed", color="black", linewidth=1) +
  scale_color_manual(values=c('PRF'="#E41A1C", 'Post'="#1F78B4", 'RF'="#4DAF4A")) +
  scale_shape_manual(values=c('PRF'=16, 'Post'=17, 'RF'=18)) +
  theme_minimal() +
  ylim(c(0., 1.75)) +
  labs(
    x='n',
    y='Average Length',
    title='Length of CI at x=(-0.3, 0.2)'
  ) +
  theme(plot.title = element_text(hjust = 0.5, face='bold'))
p8len

combined_plot <- (p7cov + p8cov) / (p7len + p8len) + 
  plot_layout(guides = 'collect') & theme(legend.position = 'bottom')
combined_plot
