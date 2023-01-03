# Plot a nice map

ggmap <- function(dummie){
  p00 <- ggplot(dummie) +
    geom_raster(aes(x = x, y = y, fill = value)) +
    borders(colour = "black") +
    coord_cartesian(xlim = c(min(dummie$x), max(dummie$x)), 
                    ylim = c(min(dummie$y), max(dummie$y))) +  
    labs(x = "Lon", y = "Lat", fill = prec_name_short) +
    scale_fill_gradient2(low ="red", mid = "white", high = "blue", midpoint = 0) +
    theme_bw() +
    theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE,
          panel.grid = element_line(color = "black"))
  y_labs <- ggplot_build(p00)$layout$panel_params[[1]]$y$get_labels()
  x_labs <- ggplot_build(p00)$layout$panel_params[[1]]$x$get_labels()
  p01 <- p00 + scale_x_continuous(labels = paste0(x_labs, "\u00b0")) +
    scale_y_continuous(labels = paste0(y_labs, "\u00b0"))
  return(p01)
}