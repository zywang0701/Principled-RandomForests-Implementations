library(ggplot2)
library(reshape2)
library(ggpubr)
library(latex2exp)

n.vec=c(1000,2000,3000,4000,5000,6000,7000,8000)
sim.round.vec = 1:20
# coverage and length -----------------------------------------------------
draw_plot <- function(pick, show_y_axis=TRUE, show_legend=FALSE){
  cov.summ = len.summ = matrix(NA, nrow=length(n.vec), ncol=4)
  colnames(cov.summ) = colnames(len.summ) = c('n','Post','PRF','RFci')
  for(i.n in 1:length(n.vec)){
    cov.summ.per = len.summ.per = matrix(NA, nrow=200, ncol=3)
    colnames(cov.summ.per) = colnames(len.summ.per) = c('Post', 'PRF', 'RFci')
    for(sim.round in sim.round.vec){
      filename = paste0('repro0514/repro-illus-p10-n', n.vec[i.n], '-simround', sim.round, '.rds')
      if(file.exists(filename)){
        result = tryCatch(readRDS(filename), error = function(e) return(NULL))
        if (!is.null(result)){
          for(isim in 1:10){
            simidx = (sim.round-1)*10 + isim
            cov.summ.per[simidx,] = result$cov0.mat.list[[isim]][pick, c(2, 6, 7)]
            len.summ.per[simidx,] = result$len0.mat.list[[isim]][pick, c(2, 6, 7)]
          }
        }
      }
    }
    cov.summ[i.n,] = c(n.vec[i.n], colMeans(na.omit(cov.summ.per)))
    len.summ[i.n,] = c(n.vec[i.n], colMeans(na.omit(len.summ.per)))
  }
  # Generate plots
  ps = plot_coverage_and_length(cov.summ, len.summ, pick, show_y_axis, show_legend)
  return(ps)
}

plot_coverage_and_length <- function(cov.summ, len.summ, pick, show_y_axis=TRUE, show_legend=FALSE){
  df.cov = melt(data.frame(cov.summ), id.vars = 'n')
  df.len = melt(data.frame(len.summ), id.vars = 'n')
  
  point_entry = switch(as.character(pick),
                       '3' = c(0.25, 0.5),
                       '10' = c(-0.45, 0.05),
                       '17' = c(-0.4, 0.1))
  p.cov = generate_plot(df.cov, 'Empirical Coverage', point_entry, show_y_axis, show_legend)
  p.len = generate_plot(df.len, 'Average Length', point_entry, show_y_axis, show_legend)
  
  return(list(p.cov = p.cov,
              p.len = p.len))
}


generate_plot <- function(df, ylabel, point_entry, show_y_axis=TRUE, show_legend=FALSE){
  title_expression <- TeX(sprintf("Point $x_0$ = (%.2f, %.2f)", point_entry[1], point_entry[2]))
  legend_position <- ifelse(!show_legend, 'none', 'bottom')
  if(ylabel == 'Empirical Coverage'){
    p = ggplot(df, aes(x=n, y=value, color=variable, shape=variable)) +
      geom_point(size=2.5) +
      geom_line() +
      geom_hline(yintercept = 0.95, linetype="dashed", color = "black", linewidth=1) +
      labs(x="", y="", title = title_expression) +
      scale_color_brewer(palette = 'Set1') +
      theme_minimal() +
      ylim(c(0.3, 1)) +
      theme(legend.title = element_blank(),
            legend.position = legend_position,
            axis.text.y = element_text(color = if (show_y_axis) "black" else "transparent"),
            axis.title.y = element_blank(),
            plot.title = element_text(hjust = 0.5, size=13))
  }
  if(ylabel == "Average Length"){
    p = ggplot(df, aes(x=n, y=value, color=variable, shape=variable)) +
      geom_point(size=2.5) +
      geom_line() +
      labs(x="", y="") +
      scale_color_brewer(palette = 'Set1') +
      theme_minimal() +
      ylim(c(0, 1.75)) +
      theme(legend.title = element_blank(),
            legend.position = legend_position,
            axis.text.y = element_text(color = if (show_y_axis) "black" else "transparent"),
            axis.title.y = element_blank())
  }
  return(p)
}

pa = draw_plot(pick=3, show_y_axis=TRUE)  # Show y-axis labels only on the first plot
pb = draw_plot(pick=17, show_y_axis=FALSE) # No y-axis labels or legend
pc = draw_plot(pick=10, show_y_axis=FALSE)  # Show legend only on the last plot

covs = ggarrange(pa$p.cov, pb$p.cov, pc$p.cov, nrow=1, ncol=3, common.legend = TRUE, legend = 'none')
covs_an = annotate_figure(covs, left = text_grob("Empirical Coverage", color = "#377EB8", rot = 90, face='bold',size=13))
lens = ggarrange(pa$p.len, pb$p.len, pc$p.len, nrow=1, ncol=3, common.legend = TRUE, legend = 'none')
lens_an = annotate_figure(lens, left = text_grob("Average Length", color = "#377EB8", rot = 90, face='bold',size=13))

# Extract the legend. Returns a gtable
leg <- get_legend(draw_plot(pick=3, show_y_axis=TRUE, show_legend=TRUE)$p.cov)

# Convert to a ggplot and print
as_ggplot(leg)

ggarrange(covs_an, lens_an, as_ggplot(leg), nrow=3, ncol=1, heights=c(0.55, 0.5, 0.05))




