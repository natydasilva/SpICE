#' Plot map including geographical location of observations colored by groups using ggmap
#' 
#' @param data data.frame with the data to be ploted having the variables longitude and latitude, named long and lat
#' @param gg name of the group id variable. 
#' @param gr vector with the selected groups to be ploted.
#' @param sz line widht in the plot.
#' @param aa numeric value for transparency in the plot.
#' @param fct logical value to inicate facets in the plot.
#' @param zz a zoom level for map
#' @param long variable name with the longitude data.
#' @param lat variable name with latitude data.
#' @param region 	a bounding box in the format c(lowerleftlon, lowerleftlat, upperrightlon, upperrightlat), see example
#' @returns A ggplot2 object.
#' @examples
#'\dontrun{
#' montevideo <- c(left = -56.286532, bottom = -34.95, right = -56.004532, top =-34.801112 )
#' plot_clcoord(data = coordDT, gg = grlab, gr = 1:4, region = montevideo)
#' }
#' @export
plot_clcoord <- function( data = NULL, gg, gr = NULL, sz = 0.5, aa = 0.7, fct = TRUE, zz = 13, long, lat, region = NULL) { 
  if(!ggmap::has_stadiamaps_key()){
    print('Key for stadiamaps is needed, see help(register_stadiamaps)')
  }else{
    
    fc <- paste("~", rlang::as_name(rlang::quo( {{gg}} )) ) |> stats::as.formula()
    colores <- grDevices::palette.colors(palette = "Set1")[gr]
    
    pp <- ggmap::get_stadiamap(bbox = region, zoom = zz) |> 
      ggmap::ggmap() + 
      ggplot2::geom_point(data= dplyr::filter(data, {{gg}} %in% gr ) ,
                          ggplot2::aes(x = long, y = lat, color = {{gg}}), alpha = 0.7,  size = sz)+
      ggplot2::scale_color_manual(values = colores) +
      ggmap::theme_nothing()
    
    if (fct) {
      pp <- pp + ggplot2::facet_wrap(fc) 
    }
    return(pp)
  }
}
