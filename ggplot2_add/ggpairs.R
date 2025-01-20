ggplot_point <- function(data, mapping){
  ggplot(data = data, mapping = mapping) +
    geom_point()
}  

ggplot_density <- function(data, mapping){
  ggplot(data = data, mapping = mapping) +
    geom_density()
}

ggplot_path <- function(data, mapping){
  ggplot(data = data, mapping = mapping) +
    geom_path()
}

gpairs_lower <- function(g){
  g$plots <- g$plots[-(1:g$nrow)]
  g$yAxisLabels <- g$yAxisLabels[-1]
  g$nrow <- g$nrow -1
  
  g$plots <- g$plots[-(seq(g$ncol, length(g$plots), by = g$ncol))]
  g$xAxisLabels <- g$xAxisLabels[-g$ncol]
  g$ncol <- g$ncol - 1
  
  g
}

## example
# ggpairs(df, mapping = aes(col = col, shape = shape, fill = shape),
#         columns = 1:4,
#         upper = NULL,
#         lower = list(continuous = function(...) ggplot_point(...) +
#                        scale_x_continuous(lims = c(0,1))),
#         diag = list(continuous = function(...) ggplot_density(...) +
#                       aes(alpha = 0.5))) +
#   scale_color_manual(values = 1:5) +
#   scale_shape_manual(values = 1:5) +
#   scale_fill_manual(values = 1:5) +
#   theme_bw()