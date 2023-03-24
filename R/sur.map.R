# sur.map----
#' @title sur.map
#' @description Displays the height map of a .SUR file
#' @param sur A surface file of the .SUR file format
#' @param col.levels The number of colors that will be used to describe height variation
#' @param col.range A vector of colors to be used on the map; default is NULL.
#' @param col.type A pre-defined pattern of colors, either "grayscale", "temperature" (default) or "colorblind"
#' @param is3d A boolean, should the result be displayed as a 3d plot in an rgl window
#' @return A data frame or a matrix of the 3d coordinates of the surface
#' @export
sur.map <- function(sur, col.levels = 255, col.range = NULL, col.type = "temperature", is3d = FALSE) {
  #Read sur file
  Mydf <- trident::sur.read(sur)
  #Define colors
  if (!is.null(col.range)) col.range <- col.range
  else if (col.type == "grayscale") col.range <- c("black","white")
  else if (col.type == "temperature") col.range <- c("royalblue4","royalblue","lightseagreen","olivedrab3","yellow1","orange","orangered","red","firebrick4")
  else if (col.type == "colorblind") col.range <- c("lightgreen","goldenrod1","yellow1","white","lightskyblue","dodgerblue4","royalblue")

  #Display map
  if(is3d)
  {
    col.palette <- grDevices::colorRampPalette(col.range)(col.levels)
    z.levels <- seq(min(Mydf$z), max(Mydf$z), length.out = col.levels + 1)
    col.tmp <- c(1:length(Mydf$z))
    for (i in c(1:col.levels)){
      col.tmp[Mydf$z >= z.levels[i]] <- col.palette[i]
    }
    rgl::open3d()
    rgl::par3d(windowRect = c(20, 30, 1000, 800))
    rgl::plot3d(x = Mydf$x, y = Mydf$y, z = Mydf$z, col = col.tmp,
                xlab = "X", ylab = "Y", zlab = "Z",
                aspect = c(1, 1, 0.25), box = FALSE, axes = FALSE,
                xlim = c(0, max(Mydf$x)), ylim = c(0, max(Mydf$y)))
    # Legend:
    plotx <- c(3, rep(0, col.levels * 100))
    ploty <- c(min(z.levels), seq(min(z.levels), max(z.levels), length.out = col.levels * 100))
    rgl::bgplot3d(graphics::plot(x = plotx,
                                 y = ploty,
                                 col = c("transparent", rep(col.palette, each = 100)),
                                 cex = 1,
                                 cex.axis = 1,
                                 cex.lab = 2,
                                 pch = 95,
                                 bty = "n",
                                 xaxs = "r",
                                 xaxt = "n",
                                 xlab = "",
                                 ylab = "height (µm)",
                                 ps = 2,
                                 family = "serif"))
    # Occlusal view:
    rgl::view3d(theta = 0, phi = 0)
    # Set the parallax to 0:
    rgl::par3d(FOV = 0)
  }

  else if (!is3d)
  {
    col.legends <- c("black", rep("transparent", col.levels - 2), "black")
    Plot <- ggplot2::ggplot(data = Mydf, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_raster(ggplot2::aes(fill = z)) +
      ggplot2::scale_fill_stepsn(name = "height (µm)",
                                 colours = col.range,
                                 n.breaks = col.levels,
                                 labels = function(breaks){
                                   Labels <- rep("", length(breaks))
                                   Labels[1] <- round(min(Mydf$z), 4)
                                   Labels[length(breaks)] <- round(max(Mydf$z), 4)
                                   return(Labels)
                                 }) +
      ggplot2::labs(x = "X", y = "Y") +
      ggplot2::guides(size = "none") +
      ggplot2::theme(text = ggplot2::element_text(family = "serif"), legend.text = ggplot2::element_text(colour = "black", size = 12, face = "bold"),
                     legend.position = "right", legend.title = ggplot2::element_text(size = 14),
                     axis.text.x = ggplot2::element_text(size = 12, angle = 00, hjust = 0.5, vjust = 0.5, face = "plain"),
                     axis.text.y = ggplot2::element_text(size = 12, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain"),
                     panel.background = ggplot2::element_rect(fill = "#ffffff", colour = "#000000", linetype = "solid"),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     panel.ontop = FALSE,
                     axis.title.x = ggplot2::element_text(size = 14, angle = 00, face = "plain"),
                     axis.title.y = ggplot2::element_text(size = 14, angle = 90, face = "plain")) +
      ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 2)))
    return(Plot)
  }
}
