library(ggplot2)
library(ggpubr)
library(latex2exp)

# ("#B2DF8A", "#6BAED6", "#2171B5", "#FB9A99")

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
  annotate('segment', x=-0.5,xend=1, y=0,yend=0, linewidth=1.5, color="#E41A1C")+
  annotate('text',x=-0.75,y=0, label=TeX('$G_i^*=1$'), size=unit(6,'pt'))+
  annotate('text',x=0.25,y=-0.5, label=TeX('$G_i^*=2$'), size=unit(6,'pt'))+
  annotate('text',x=0.25,y=0.5, label=TeX('$G_i^*=3$'), size=unit(6,'pt'))+
  labs(x='X1', y='X2', title='True Memberships')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, face='bold'))
p1

df <- data.frame(x = runif(100,-1,1), y = runif(100,-1,1))
p2 = ggplot(df) +
  annotate("rect",
           xmin = -1, xmax = -0.53, ymin = -1, ymax = 1,
           fill = "white", alpha=0.5) +
  annotate("rect",
           xmin = -0.53, xmax = 1, ymin = -1, ymax = 1, 
           fill = "white", alpha=0.5) +
  annotate('segment', x=-1,xend=-1, y=-1,yend=1)+
  annotate('segment', x=1,xend=1, y=-1,yend=1)+
  annotate('segment', x=-1,xend=1, y=1,yend=1)+
  annotate('segment', x=-1,xend=1, y=-1,yend=-1)+
  annotate('segment', x=-0.53,xend=-0.53, y=-1,yend=1, linewidth=1.5, linetype=2)+
  annotate('text',x=0.25,y=0, label=TeX('$\\tilde{G}_i=2$'), size=unit(6,'pt'))+
  annotate('text',x=-0.75,y=0, label=TeX('$\\tilde{G}_i=1$'), size=unit(6,'pt'))+
  labs(x='X1', y='X2', title='CART n=1000')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, face='bold'))
p2

df <- data.frame(x = runif(100,-1,1), y = runif(100,-1,1))
p3 = ggplot(df) +
  annotate("rect",
           xmin = -1, xmax = -0.49, ymin = -1, ymax = 1,
           fill = "white", alpha=0.5) +
  annotate("rect",
           xmin = -0.49, xmax = 1, ymin = -0.01, ymax = 1, 
           fill = "white", alpha=0.5) +
  annotate("rect",
           xmin = -0.49, xmax = 1, ymin = -1, ymax = -0.01, 
           fill = "white", alpha=0.5) +
  annotate('segment', x=-1,xend=-1, y=-1,yend=1)+
  annotate('segment', x=1,xend=1, y=-1,yend=1)+
  annotate('segment', x=-1,xend=1, y=1,yend=1)+
  annotate('segment', x=-1,xend=1, y=-1,yend=-1)+
  annotate('segment', x=-0.49,xend=-0.49, y=-1,yend=1, linewidth=1.5, linetype=2)+
  annotate('segment', x=-0.49,xend=1, y=0.05,yend=0.05, linewidth=1.5, linetype=2, color="#E41A1C")+
  annotate('text',x=0.25,y=0.5, label=TeX('$\\tilde{G}_i=3$'), size=unit(6,'pt'))+
  annotate('text',x=0.25,y=-0.5, label=TeX('$\\tilde{G}_i=2$'), size=unit(6,'pt'))+
  annotate('text',x=-0.75,y=0, label=TeX('$\\tilde{G}_i=1$'), size=unit(6,'pt'))+
  labs(x='X1', y='X2', title='CART n=4000')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, face='bold'))
p3

# df <- data.frame(x = runif(100,-1,1), y = runif(100,-1,1))
# df.highlight = data.frame(x = c(0.5, 0, -0.5), y=c(0.6, 0, 0.4))
# label.highlight = c('(0.5, 0.6)', '(0.0, 0.0)', '(-0.5, 0.4)')
# p3 = ggplot(df) +
#   annotate("rect",
#            xmin = -1, xmax = 0, ymin = -0.2, ymax = 1,
#            fill = "#2171B5", alpha=0.5) +
#   annotate("rect",
#            xmin = -1, xmax = 0, ymin = -1, ymax = -0.2,
#            fill = "#6BAED6", alpha=0.5) +
#   annotate("rect",
#            xmin = 0, xmax = 1, ymin = 0.2, ymax = 1,
#            fill = "#B2DF8A", alpha=0.5) +
#   annotate("rect",
#            xmin = 0, xmax = 1, ymin = -1, ymax = 0.2,
#            fill = "#FB9A99", alpha=0.5)+
#   # geom_point(aes(x, y), size = 1, color='gray')+
#   annotate('segment', x=0,xend=0, y=-1,yend=1)+
#   # annotate('segment', x=0,xend=-1, y=-0.2,yend=-0.2)+
#   annotate('segment', x=0,xend=-1, y=-0.2,yend=-0.2, linetype=2, linewidth=1.5, color="#E41A1C")+ #"#1F78B4"
#   annotate('segment', x=-0,xend=1, y=0.2,yend=0.2)+
#   geom_point(data=df.highlight, aes(x=x, y=y), color="#E41A1C", size=4, shape=23, fill="#E41A1C")+
#   geom_text(data=df.highlight, aes(x=x+0.3, y=y, label=label.highlight), size=5, color="#E41A1C", fontface='bold')+
#   labs(x='X1', y='X2', title='Three New Data Point')+
#   theme_minimal()+
#   theme(plot.title = element_text(hjust = 0.5, face='bold'))
# p3

ggarrange(p1,p2, p3, nrow=1)
