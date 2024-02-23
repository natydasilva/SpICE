# funcion que calcule ice: suave, derivadas, distancias y clauster, para un valor de alpha
# argumentos: D0 = curveDT, D1= datos coordenadas, alpha

# D0 = curveDT
# D1 = coordDT

ff <- function( D0, D1, alp = 0.5 ) { 
  zz <- D0 |>   split( f = D0$yhat.id) 
  
  ice.smooth <- lapply(zz, FUN=function(crv) {
    ll <- locpoly(x=crv$grid.val, y=crv$ice, gridsize = nrow(crv), bandwidth = diff(crv$grid.val)[1] )
    ll$y |> as_tibble() |> t()
  })  |>  do.call(rbind, args=_)
  
  ice.deriv <- lapply(zz, FUN=function(crv) {
    ll <- locpoly(x=crv$grid.val, y=crv$ice, gridsize = nrow(crv), bandwidth = diff(crv$grid.val)[1] , drv=1)
    ll$y |> as_tibble() |> t()
  })  |>  do.call(rbind, args=_)
  
  # compute distances matrices 
  DD1 <- D1 |> 
    arrange(yhat.id) |> 
    st_as_sf( coords = c("lat", "long"), crs = 4326) |> 
    st_distance()
  
  # Distancias Sovolev entre ICEs
  DD0 <- sqrt( dist(ice.smooth)^2 + dist(ice.deriv)^2 )
  
  # Create clusters
  hclustgeo(D0=DD0, D1=DD1, alpha=al) 
}

