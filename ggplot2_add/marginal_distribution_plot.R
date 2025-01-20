gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

summary_plot <- function(x, xintercept){
  x.density = density(x)
  x.df = data.frame(x = x.density$x,
                    pdf = x.density$y,
                    cdf = ecdf(x)(x.density$x))

  ggplot(x.df, aes(x = x)) +
    theme_bw() +
    geom_line(aes(y = pdf), color = "blue") +
    geom_line(aes(y = cdf*base::max(x.density$y)), color = 'red') +
    scale_y_continuous(
      name = "Density",
      sec.axis = sec_axis(~./base::max(x.density$y), name = "Cumulative Probability")
    ) +
    theme(
      axis.title.y.right = element_text(color = "red"), 
      axis.title.y.left = element_text(color = "blue")
    ) +
    geom_vline(xintercept = xintercept, linetype = 'dashed')
  
  # ggplot(data.frame(x = x), aes(x = x)) +
  #   theme_bw() +
  #   geom_density(color = "blue") +
  #   stat_ecdf(aes(y = after_stat(y)*base::max(x.density$y)), color = "red") +
  #   scale_y_continuous(
  #     name = "Density",
  #     sec.axis = sec_axis(~./base::max(x.density$y), name = "Cumulative Probability")
  #   ) +
  #   theme(
  #     axis.title.y.right = element_text(color = "red"), 
  #     axis.title.y.left = element_text(color = "blue")
  #   ) +
  #   geom_vline(xintercept = xintercept, linetype = 'dashed')
  
}

marginal_dp <- function(X, summary,
                        summary_type = 'y',
                        summary_name = 's',
                        marginal_plot = plot_dist){
  # summary is either a function to compute summary statistics or a numeric vector
  # or pre-computed summary statistics
  # summary_type is one of 'y', 'x', or a numeric vector of indices to display
  
  p = ncol(X)
  if(is.numeric(summary)){
    x = summary_fun
  }else{
    x = apply(X, 2, summary)
  }
  x.sorted = sort(x, index.return = T)
  
  if(index_type == 'even'){
    
  }
  if(index_type == 'percentile'){
    idx_cut = floor(quantile(1:p, probs = (0:14)/14))
  }
  idx = x.sorted$ix[idx_cut]
  val = x.sorted$x[idx_cut]
  
  g_summary = summary_plot(x, val) +
    theme(text = element_text(size = 8))
  g.ls = vector(mode = 'list', length = 15)
  for(i in 1:15){
    alpha.vec = rep(0,nrow(X))
    alpha.vec[sample(1:nrow(X),100)] = 1
    g = ggplot(data = data.frame(x = X[,idx[i]]), aes(x = x)) +
      geom_density(color = "blue", fill = "blue", alpha = 0.3) +
      theme_bw() +
      labs(y = NULL, x = paste0('ID = ', idx[i],', s = ', round(x[idx[i]], 3))) +
      theme(text = element_text(size = 8)) +
      geom_point(aes(x = x,
                     y = runif(length(x),0.4*max(density(x)$y),0.6*max(density(x)$y))),
                 alpha = alpha.vec,
                 shape = 1)
    g.ls[[i]] = g
  }
  
  plot_grid(plotlist = c(list(g_summary), g.ls),
            nrow = 4, ncol = 4, byrow = T)
  
}


marginal_dp <- function(X, summary_fun, x.xlab){
  
  p = ncol(X)
  if(is.null(colnames(X))){
    colnames(X) = paste0('V',1:p)
  }
  
  if(is.numeric(summary_fun)){
    x = summary_fun
  }else{
    x = apply(X, 2, summary_fun)
  }
  x.sorted = sort(x, index.return = T)
  
  idx_cut = floor(quantile(1:p, probs = (0:14)/14))
  idx = x.sorted$ix[idx_cut]
  val = x.sorted$x[idx_cut]
  
  g_summary = summary_plot(x, val, x.xlab) +
    theme(text = element_text(size = 8))
  g.ls = vector(mode = 'list', length = 15)
  for(i in 1:15){
    alpha.vec = rep(0,nrow(X))
    alpha.vec[sample(1:nrow(X),100)] = 1
    g = ggplot(data = data.frame(x = X[,idx[i]]), aes(x = x)) +
      geom_density(color = "blue", fill = "blue", alpha = 0.3) +
      theme_bw() +
      labs(y = NULL, x = paste0('ID = ', idx[i],', s = ', round(x[idx[i]], 3))) +
      theme(text = element_text(size = 8)) +
      geom_point(aes(x = x,
                     y = runif(length(x),0.4*max(density(x)$y),0.6*max(density(x)$y))),
                 alpha = alpha.vec,
                 shape = 1)
    g.ls[[i]] = g
  }
  
  plot_grid(plotlist = c(list(g_summary), g.ls),
            nrow = 4, ncol = 4, byrow = T)
  
}



plot_dist_single <- function(x){
  x.density = density(x)
  ggplot(data.frame(x = x), aes(x = x)) +
    theme_bw() +
    geom_density() +
    geom_point(y = runif(length(x), 0, base::max(x.density$y)/3),
               shape = 1, col = 3) +
    theme(plot.title = element_text(hjust = 0.5))
}

plot_dist <- function(X){
  if(is.null(dim(X))){
    plot_dist_single(x)
  }
  
  n_col = ceiling(sqrt(ncol(X)))
  plot.list = lapply(1:ncol(X), function(i){
    plot_dist_single(X[,i]) +
      ggtitle(colnames(X)[i]) +
      labs(x = NULL, y = NULL)})
  plot_grid(plotlist = plot.list, ncol = n_col)
}
