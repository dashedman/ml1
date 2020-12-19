library(plotly)
library(htmlwidgets)
library(MASS)
args <- commandArgs(trailingOnly = T)

get3DSurface <- function(boundedBox, z_func, delta = 0.1){
  #boundedBox - c(minX, minY, maxX, maxY)
  x = seq(boundedBox[1], boundedBox[3], delta)
  y = seq(boundedBox[2], boundedBox[4], delta)
  z = c()

  for(xi in x){
    z_line <- rep(0, length(y))
    zi = 1
    for(yi in y){
        z_line[zi] <- z_func(c(xi,yi))
        zi <- zi + 1
    }
    z <- cbind(z, z_line)
  }

  return(list(x = x, y = y, z = z))
}

get3DScatter <- function(v, z_func){
  #boundedBox - c(minX, minY, maxX, maxY)
  z = rep(0, length(x))
  z_index = 1
  for(vi in v){
    z[z_index] <- z_func(vi[1:2])
    z_index <- z_index+1
  }

  return(list(x = v[,1], y = v[,2], z = z))
}



####################################
.ADALINE <- function(){

}
######################################

.ADALINE()
