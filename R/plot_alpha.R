# funcion para plotear los alphas
# args: 
# DD1: data with coordinate information
# DD0: data with curves information
# resp: response vector to perform stratified sample
# nfrac: sampling fraction
# clRange: vector with posible values of number of cluster

plot_alpha <- function(DD1, DD0, resp=NULL, nfrac=0.2, 
                       clRange=2:6, alpRange=seq(0, 1, .1)) {
  # sample of dataset
  if( is.null(resp) ) {
    id.smp <- DD1 |> mutate(rr=1:nrow(DD1) ) |> sample_frac(nfrac) |> pull(rr)
  } else {
    qs = quantile(resp, probs=seq(0,1,.1))
    id.smp <- DD1 |> mutate(qq = cut(lpreciom2, breaks = qs, include.lowest = TRUE),
                            rr = 1:nrow(df) ) |>
      group_by(qq) |> sample_frac(nfrac) |> pull(rr)
  }
  #-------------
  
  # distance matrices
  zz <- DD0 |>   split( f = DD0$yhat) 
  DD1 <- DD1 |> dplyr::arrange(yhat)
  
  ice.smooth <- lapply(zz, FUN = function(crv) {
    ll <- KernSmooth::locpoly(x = crv$grid.val, y = crv$ice, gridsize = nrow(crv), bandwidth = diff(crv$grid.val)[1] )
    ll$y |> dplyr::as_tibble() |> t()
  })  |>  do.call(rbind, args =_)
  
  ice.deriv <- lapply(zz, FUN = function(crv) {
    ll <- KernSmooth::locpoly(x = crv$grid.val, y = crv$ice, gridsize = nrow(crv), bandwidth = diff(crv$grid.val)[1] , drv = 1)
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
  for(kk in 2:6) {
    alp.opt <- choicealpha(D0=DD0.smp, D1=DD1.smp, 
                           range.alpha = alpRange, K = kk, 
                           scale = TRUE, graph = FALSE)
    alpha.info <- rbind(alpha.info ,
                        cbind(k=alp.opt$K, alpha=alp.opt$range.alpha, alp.opt$Q,  alp.opt$Qnorm)
    )
  }
  
  pl <- as_tibble(alpha.info) |> filter(k %in% 3:5) |> 
    select(-Q0norm, -Q1norm) |> 
    #select(-Q0, -Q1) |> 
    pivot_longer(cols = 3:4) |>  
    ggplot( aes(x=alpha, y=value, color=name) ) + 
    geom_point() + geom_line() + 
    #geom_hline(yintercept = 0.75, color='grey40', linewidth=1) +
    labs(y = 'Cluster homogeneity', color = '') + 
    facet_wrap(~k) + 
    #scale_x_continuous(breaks = seq(0,1, .1), limits = c(.4, 1)) + 
    #scale_y_continuous( limits = c(.4, 1.1)) + 
    scale_color_brewer(palette = 'Dark2') + 
    theme_bw() + theme(aspect.ratio = 1)
  
  # ggsave(file= paste0(ffpath, 'fig-alphaoptimo.pdf'), height = 7, width = 7)
  # saveRDS(alpha.info, file='data/alpha_info.rds') 
  # alpha.info <- readRDS('data/alpha_info.rds')
  out = list(alpha.info = alpha.info, alpha_plot=pl)
  return(out)
}
