#' Cluster ICE curves, transforming the ICE curves first and use for the clusters geographical information
#' 
#' @param DD0 data.frame with ICE curve information. 
#' @param DD1 data.frame with coordinate information asociated to ice curves values named lat and long.
#' @param alp numeric value between 0 and 1 which represents the weight fo D0 (ICE curve information) respect to location value.
#' @param yhat Name of each ice curve id.
#' @param grid Name of the grid value variable.
#' @param icevalue  Name of ice curve variable.
#' @returns data.frame
#' @examples
#'\dontrun{
#' cl_sobcurve(DD0 = curveDT, DD1 = coordDT, yhat = yhat.id, grid = grid.val, ice = ice)
#' }
#' @export

cl_sobcurve <- function( DD0, DD1, alp = 0.5, yhat = NULL, grid, icevalue ) { 
  zz <- DD0 |>   split( f = dplyr::pull(DD0, {{yhat}}) )
  
  ice.smooth <- lapply(zz, FUN = function(crv) {
    xs <-  dplyr::pull(crv, {{grid}})
    ys <- dplyr::pull(crv, {{icevalue}})
    ll <- KernSmooth::locpoly(x = xs, y = ys, gridsize = nrow(crv), bandwidth = diff(xs,1)[1])
    ll$y |> dplyr::as_tibble() |> t()
  })  |>  do.call(rbind, args =_)
  
  ice.deriv <- lapply(zz, FUN = function(crv) {
    xs <-  dplyr::pull(crv, {{grid}})
    ys <- dplyr::pull(crv, {{icevalue}})
    ll <- KernSmooth::locpoly(x = xs, y = ys, gridsize = nrow(crv), bandwidth = diff(xs,1)[1] , drv = 1)
    ll$y |> dplyr::as_tibble() |> t()
  })  |>  do.call(rbind, args=_)
  
  
  D1 <- DD1 |> 
    dplyr::arrange({{yhat}}) |> 
    sf::st_as_sf( coords = c("lat", "long"), crs = 4326) |> 
    sf::st_distance() |> 
    stats::as.dist()
  
  D0 <- sqrt( stats::dist(ice.smooth)^2 + stats::dist(ice.deriv)^2 )
  
  
  ClustGeo::hclustgeo(D0 = D0, D1 = D1, alpha = alp) 
  
}

