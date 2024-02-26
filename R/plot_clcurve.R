#' Plot clustered ICE curves using ggplot2
#' 
#' @param data data.frame with the data to be ploted
#' @param icevalue Name of ice curve variable.
#' @param gg Name of the group id variable. 
#' @param gr vector with the selected groups to be ploted.
#' @param yhat Name of each ice curve id.
#' @param sz line widht in the plot.
#' @param aa numeric value for transparency in the plot.
#' @param xlab string with x axis label.
#' @param ylab string with y axis label. 
#' @param fct logical value to inicate facets in the plot.
#' @param xvalue variable name with grid values ploted in x axis.
#' @returns A ggplot2 object
#' @examples
#'\dontrun{
#' plot_clcurve(data = curveDT, icevalue = ice, gg = grlab, yhat = yhat.id, 
#'gr = 1:4, xvalue = grid.val, aa =1/20)
#' }
#' @export
plot_clcurve <- function(data = NULL, icevalue= NULL, gg,  yhat, gr = NULL, sz = 0.5, aa = 1/100, xlab = NULL, ylab = NULL,
                         fct = TRUE, xvalue = NULL) { 
  
  fc <- paste("~", rlang::as_name(rlang::quo( {{gg}} )) ) |> stats::as.formula()
  colores <- grDevices::palette.colors(palette = "Set1")[gr]
  
  pp <- data |>
    dplyr::filter( {{gg}} %in% gr ) |>
    ggplot2::ggplot( ggplot2::aes(x = {{xvalue}}, y = {{icevalue}}, group = {{yhat}}, color = {{gg}} )) +
    ggplot2::geom_line(linewidth = sz, alpha = aa ) +
    ggplot2::labs(x = xlab, y = ylab) +
    ggplot2::scale_color_manual(values = colores) +
    ggplot2::theme(legend.position = 'none')
  
  if (fct) {
    pp <- pp + ggplot2::facet_wrap(fc) 
  }
  return(pp)
}
