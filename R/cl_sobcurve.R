#' Cluster ICE curves, transforming the ICE curves first and use for the clusters geographical information
#' 
#' @param DD0 data.frame with ICE curve information. 
#' @param DD1 data.frame with coordinate information asociated to ice curves values.
#' @param yhat Name of each ice curve id
#' @param alp numeric value between 0 and 1 which represents the weight fo D0 (ICE curve information) respect to location value.
#' @returns data.frame
#' @examples
#'\dontrun{
#'cl_sobcurve(DD0 = curveDT, DD1 = coordDT, yhat = 'yhat.id')
#' }
#' @export

cl_sobcurve <- function( DD0, DD1, alp = 0.5, yhat = NULL ) { 
  zz <- DD0 |>   split( f = DD0[, yhat])
  
  ice.smooth <- lapply(zz, FUN = function(crv) {
    ll <- KernSmooth::locpoly(x = crv$grid.val, y = crv$ice, gridsize = nrow(crv), bandwidth = diff(crv$grid.val)[1] )
    ll$y |> dplyr::as_tibble() |> t()
  })  |>  do.call(rbind, args =_)
  
  ice.deriv <- lapply(zz, FUN = function(crv) {
    ll <- KernSmooth::locpoly(x = crv$grid.val, y = crv$ice, gridsize = nrow(crv), bandwidth = diff(crv$grid.val)[1] , drv = 1)
    ll$y |> dplyr::as_tibble() |> t()
  })  |>  do.call(rbind, args=_)
  
 
  D1 <- DD1 |> 
    dplyr::arrange(yhat) |> 
    sf::st_as_sf( coords = c("lat", "long"), crs = 4326) |> 
    sf::st_distance() |> 
    stats::as.dist()
  
  D0 <- sqrt( stats::dist(ice.smooth)^2 + stats::dist(ice.deriv)^2 )
  

  ClustGeo::hclustgeo(D0 = D0, D1 = D1, alpha = alp) 

}

