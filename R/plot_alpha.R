#' Function to compute and visualize optimum alpha cluster values
#' 
#' @param DD0 data.frame with ICE curve information. 
#' @param DD1 data.frame with coordinate information asociated to ice curves values named lat and long.
#' @param resp response vector to perform stratified sample
#' @param nfrac sampling fraction
#' @param yhat Name of each ice curve id
#' @param icevalue  Name of ice curve variable.
#' @param grid Name of the grid value variable.
#' @param clRange Numeric vector with posible values of number of cluster.
#' @param alpRange Numeric vector with posible values of alpha parameter.
#' @returns A list with cluster metric for each value of alpRange and clRange, and a visualization of the intertia decomposition. 
#' @examples
#'\dontrun{
#' plot_alpha( DD0 = curveDT, DD1= coordDT, yhat = yhat.id, icevalue=ice, grid=grid.val)
#' }
#' @export
plot_alpha <- function(DD1, DD0, resp = NULL, nfrac = .2, yhat, icevalue, grid,
                       clRange = 3:5, alpRange = seq(0, 1, .2)) {
  # sample of dataset
  if( is.null(resp) ) {
    id.smp <- DD1 |> dplyr::mutate(rr = 1:nrow(DD1) ) |> dplyr::sample_frac(nfrac) |> dplyr::pull(rr)
  } else {
    qs = stats::quantile(resp, probs=seq(0,1,.1))
    id.smp <- DD1 |> dplyr::mutate(qq = cut(resp, breaks = qs, include.lowest = TRUE),
                            rr = 1:nrow(df) ) |>
      dplyr::group_by(qq) |> dplyr::sample_frac(nfrac) |> dplyr::pull(rr)
  }
  #-------------
  
  # distance matrices
  
  DD1 <- DD1 |> dplyr::arrange({{yhat}})
  zz <- DD0 |>  split( f = dplyr::pull(DD0, {{yhat}}) )
  
  ice.smooth <- lapply(zz, FUN = function(crv) {
    xs <-  dplyr::pull(crv, {{grid}})
    ys <- dplyr::pull(crv, {{icevalue}})
    ll <- KernSmooth::locpoly(x = xs, y = ys, gridsize = nrow(crv), bandwidth = diff(xs,1)[1])
    ll$y |> dplyr::as_tibble() |> t()
  })  |>  do.call(rbind, args =_)
  
  ice.deriv <- lapply(zz, FUN = function(crv) {
    xs <- dplyr::pull(crv, {{grid}})
    ys <- dplyr::pull(crv, {{icevalue}})
    ll <- KernSmooth::locpoly(x = xs, y = ys, gridsize = nrow(crv), bandwidth = diff(xs,1)[1] , drv = 1)
    ll$y |> dplyr::as_tibble() |> t()
  })  |>  do.call(rbind, args=_)
  
  
  
  
  D1.smp <- DD1[id.smp, c("long","lat")] |> 
    sf::st_as_sf( coords = c("lat", "long"), crs = 4326) |> 
    sf::st_distance() |> 
    stats::as.dist()
  
  D0.smp <- sqrt( stats::dist(ice.smooth[id.smp,])^2 + stats::dist(ice.deriv[id.smp,])^2 )
  #-------------
  
  # compute inertia accordin to alpha values
  alpha.info <- NULL
  for(kk in clRange) {
    alp.opt <-   ClustGeo::choicealpha(D0 = D0.smp, D1 = D1.smp, 
                           range.alpha = alpRange, K = kk, 
                           scale = TRUE, graph = FALSE)
    alpha.info <- rbind(alpha.info ,
                        cbind(k = alp.opt$K, alpha = alp.opt$range.alpha, alp.opt$Q,  alp.opt$Qnorm)
    )
  }
  
  pl <- dplyr::as_tibble(alpha.info) |> 
  dplyr::select(-Q0norm, -Q1norm) |> 
    #select(-Q0, -Q1) |> 
    tidyr::pivot_longer(cols = 3:4) |>  
    ggplot2::ggplot( aes(x=alpha, y=value, color=name) ) + 
    ggplot2::geom_point() + ggplot2::geom_line() + 
    ggplot2::labs(y = 'Cluster homogeneity', color = '') + 
    ggplot2::facet_wrap(~k) + 
    ggplot2::scale_color_brewer(palette = 'Dark2') + 
    ggplot2::theme_bw() + ggplot2::theme(aspect.ratio = 1)
  
  out = list(alpha.info = alpha.info, alpha_plot = pl)
  
  return(out)
}
