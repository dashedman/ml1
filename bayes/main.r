library(plotly)
library(htmlwidgets)
library(MASS)
args <- commandArgs(trailingOnly = T)

gausDistr <- function(x, u, SigmMatr){
    # %*% - matrix multiply
    return( exp(-0.5 * (t(x-u) %*% ginv(SigmMatr) %*% (x-u))) / sqrt((2 * pi) ^ length(x) * det(SigmMatr)) )
}
logGausDistr <- function(x, u, SigmMatr){
  return( -0.5 * (t(x-u) %*% ginv(SigmMatr) %*% (x-u)) - log(sqrt((2 * pi) ^ length(x) * det(SigmMatr))) )
}

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


muEstimate <- function(dat, class){
  #dat - (x1,...,xn, class)*l
  #class - int
  #
  #return u(1/m * sum(x))
  l <- dim(dat)[1]
  n <- dim(dat)[2] - 1

  mu <- rep(0,n)
  for(i in 1:n){
    sum <- 0
    counter <- 0
    for(j in 1:l){
      if(dat[j,n+1] == class){
        counter <- counter + 1
        sum <- sum + dat[j, i]
      }
    }
    mu[i] <- sum/counter
  }

  return(mu)
}

singleSigmaEstimate <- function(dat, class, mu){
  #dat - (x, class)*l
  #
  #return sigm
  l <- dim(dat)[1]

  sigm <- 0
  counter <- 0
  for(i in 1:l){
    if(dat[i,2] == class){
      counter <- counter + 1
      sigm <- sigm + (dat[i, 1] - mu) * (dat[i, 1] - mu)
    }

  }
  if(counter > 1)
    return((1/(counter-1)) * sigm)
  else
    return(1)
}

sigmaEstimate <- function(dat, class, mu){
  #dat - (x1,...,xn, class)*l
  #class - int
  #
  #return sigm
  l <- dim(dat)[1]
  n <- dim(dat)[2] - 1

  sigm <- matrix(0, nrow=n, ncol=n)
  counter <- 0
  for(i in 1:l){
    if(dat[i,n+1] == class){
      counter <- counter + 1
      sigm <- sigm + ((dat[i, 1:n] - mu) %*% t(dat[i, 1:n] - mu))
    }

  }
  if(counter > 1)
    return((1/(1-counter)) * sigm)
  else
    return(diag(1,n,n))
}

averageSigmaEstimate <- function(dat, mu){
  #dat - (x1,...,xn, class)*l
  #mu - list of mu from other classes
  #
  #return sigm
  l <- dim(dat)[1]
  n <- dim(dat)[2] - 1

  sigm <- matrix(0, nrow=n, ncol=n)
  for(i in 1:l){
      sigm <- sigm + ((dat[i, 1:n] - mu[[ dat[i, n+1] ]]) %*% t(dat[i, 1:n] - mu[[ dat[i, n+1] ]]))
  }

  return((1/l) * sigm)
}

NaiveBayes <- function(x, Py, lambdas){
  #x - object for classification length n
  #Py - list of likelyhoods parmeters gausian's for classes
  #Py[i,] = [$u, $sigm] - gausians parameters - mu center and corelation matrix sigm
  #u - (u_1,...,u_n) - n real numbers
  #sigm - symetric matrix nxn, det()>0
  #lambdas - (l1,...,ln) lambdas for risk

  l <- length(Py)
  n <- length(x)
  props <- rep(0, l)
  for(i in 1:l){
    props[i] <- log(lambdas[i])
    u <- Py[[i]]$u
    sigm <- Py[[i]]$sigm
    for(j in 1:n){
      props[i] <- props[i] - 0.5*(2*log(sigm[j]) + log(2*pi) + ((x[j] - u[j])/sigm[j])**2)
    }
  }
  return(which.max(props))
}
####################################
.LevelLines <- function(){
  u <- c(0, 0)
  SigmMatr <- matrix(c(4, 1.2, 1.2, 1), nrow = 2, byrow = T)
  #1 2
  #3 4
  z_func <- function(x){
    return( gausDistr(x, u, SigmMatr) )
  }

  coords <- get3DSurface(c(-4,-4,4,4), z_func)
  #считаем
  #рисуим

  fig <- plot_ly(
    type = "surface",
    x = ~coords$x,
    y = ~coords$y,
    z = ~coords$z,
    colorscale = list(c(0, "#10D0C0"), c(0.5, "#C0D030"), c(1,"#F02010")),
    contours = list(
      x = list(
        highlightcolor="#f0f0c0"
      ),
      y = list(
        highlightcolor="#f0f0c0"
      ),
      z = list(
        show = TRUE,
        start = 0.01,
        end = 0.2,
        size = 0.015,
        usecolormap=TRUE,
        highlightcolor="#f0f0c0",
        project=list(z=TRUE)
      )
    )
  ) %>% layout(
      scene = list(
        camera=list(
          eye = list(x=2, y=1, z=-0.5)
        ),
        bgcolor = "#333333",
        xaxis=list(
          title = "X axis",
          backgroundcolor="#555555",
          gridcolor="#999999",
          showbackground=TRUE,
          zerolinecolor="#AAAAAA"
        ),
        yaxis=list(
          title = "Y axis",
          backgroundcolor="#555555",
          gridcolor="#999999",
          showbackground=TRUE,
          zerolinecolor="#AAAAAA"
        ),
        zaxis=list(
          title = "Value(Z axis)",
          range = c(0, 0.2),
          backgroundcolor="#555555",
          gridcolor="#999999",
          showbackground=TRUE,
          zerolinecolor="#AAAAAA"
        )
      )
  )
  saveWidget(fig, file = "contour5.html", selfcontained = FALSE, libdir = "plotly_lib", background = "#333333")
}

.LevelLines2 <- function(){
  u1 <- c(0.8, -1)
  SigmMatr1 <- matrix(c(2, 1, 1, 1), nrow = 2)
  u2 <- c(-1, 1.5)
  SigmMatr2 <- matrix(c(0.8, -0.5, -0.5, 1.2), nrow = 2)
  #1 2
  #3 4
  z_func <- function(x){
    return( gausDistr(x, u1, SigmMatr1) - gausDistr(x, u2, SigmMatr2) )
  }

  coords <- get3DSurface(c(-4,-4,4,4), z_func)
  #считаем
  #рисуим

  fig <- plot_ly(
    type = "surface",
    x = ~coords$x,
    y = ~coords$y,
    z = ~coords$z,
    colorscale = list(c(0, "#10D0C0"),c(0.5, "#FFFFAA"), c(1,"#F02010")), #c(0.5, "#FFFF00")
    #opacityscale = list(c(0, 1),c(0.45, 1),c(0.49, 0),c(0.51, 0), c(0.55,1), c(1, 1)),
    contours = list(
      x = list(
        highlightcolor="#f0f0c0"
      ),
      y = list(
        highlightcolor="#f0f0c0"
      ),
      z = list(
        show = TRUE,
        start = -0.4,
        end = 0.4,
        size = 1/64,
        usecolormap=TRUE,
        highlightcolor="#f0f0c0",
        project=list(z=TRUE)
      )
    )
  ) %>% layout(
      scene = list(
        camera=list(
          eye = list(x=2, y=1, z=-0.5)
        ),
        bgcolor = "#333333",
        xaxis=list(
          title = "X axis",
          backgroundcolor="#555555",
          gridcolor="#999999",
          showbackground=TRUE,
          zerolinecolor="#AAAAAA"
        ),
        yaxis=list(
          title = "Y axis",
          backgroundcolor="#555555",
          gridcolor="#999999",
          showbackground=TRUE,
          zerolinecolor="#AAAAAA"
        ),
        zaxis=list(
          title = "Value(Z axis)",
          range = c(-0.4, 0.4),
          backgroundcolor="#555555",
          gridcolor="#999999",
          showbackground=TRUE,
          zerolinecolor="#AAAAAA"
        )
      )
  )
  saveWidget(fig, file = "contour2.html",selfcontained = FALSE,libdir = "plotly_lib",background = "#333333")
}

.NaiveBayes <- function(){
  colormap <- c("darkseagreen", "darkorchid")

  #centres
  u1 <- c(2,-2)
  u2 <- c(-2,2)

  #cov matrixes
  M1 <- matrix(c(2, -1, -1, 2) ,nrow = 2)
  M2 <- matrix(c(1, 0, 0, 3) ,nrow = 2)

  #generate data
  set.seed(1488)
  genDat1 <- MASS::mvrnorm(100, mu = u1, Sigma = M1)
  set.seed(1488)
  genDat2 <- MASS::mvrnorm(100, mu = u2, Sigma = M2)

  allData <- rbind( cbind(genDat1, 1), cbind(genDat2, 2))
  lambdas <- c(0.5, 1)


  #classificate
  eu1 <- c(muEstimate(allData[,c(1,3)], 1),muEstimate(allData[,c(2,3)], 1))
  eu2 <- c(muEstimate(allData[,c(1,3)], 2),muEstimate(allData[,c(2,3)], 2))
  eM1 <- c(singleSigmaEstimate(allData[,c(1,3)], 1, eu1[1]), singleSigmaEstimate(allData[,c(2,3)], 1, eu1[2]))
  eM2 <- c(singleSigmaEstimate(allData[,c(1,3)], 2, eu2[1]), singleSigmaEstimate(allData[,c(2,3)], 2, eu2[2]))
  Py <- list(
    list(u = eu1, sigm = eM1),
    list(u = eu2, sigm = eM2)
  )

  n <- dim(allData)[1]
  classes <- rep(0, n)

  print(eu1)
  print(eu2)
  print(eM1)
  print(eM2)
  for(i in 1:n){
    classes[i] <- NaiveBayes(allData[i, c(1,2)], Py, lambdas)
  }


  z_func1 <- function(x){
    return( gausDistr(x, u1, M1))
  }
  z_func2 <- function(x){
    return( gausDistr(x, u2, M2) )
  }
  boundedBox <- c(
    floor(min(allData[,1])),
    floor(min(allData[,2])),
    ceiling(max(allData[,1])),
    ceiling(max(allData[,2]))
  )

  coords1 = get3DSurface(boundedBox, z_func1, delta = 0.25)
  coords2 = get3DSurface(boundedBox, z_func2, delta = 0.25)
  print(boundedBox)

  fig <- plot_ly(
  ) %>% add_surface(#"darkorchid4"
    x = ~coords1$x,
    y = ~coords1$y,
    z = ~coords1$z,
    colorscale = list(c(0, "rgba(64,64,64,0)"),c(0.1, colormap[1]),c(1, colormap[1])),
    opacity = 0.7
  ) %>% add_surface(
    x = ~coords2$x,
    y = ~coords2$y,
    z = ~coords2$z,
    colorscale = list(c(0, "rgba(64,64,64,0)"),c(0.1, colormap[2]),c(1,colormap[2])),
    opacity = 0.7
  )  %>% add_markers(
    x = ~allData[,1],
    y = ~allData[,2],
    z = rep(0, dim(allData)[1]),
    marker = list(
      size = 4,
      color = colormap[classes]
    )
  ) %>% layout(
      scene = list(
        camera=list(
          eye = list(x=2, y=1, z=-0.5)
        ),
        bgcolor = "#333333",
        xaxis=list(
          title = "X axis",
          backgroundcolor="#333333",
          gridcolor="#999999",
          showbackground=TRUE,
          zerolinecolor="#AAAAAA"
        ),
        yaxis=list(
          title = "Y axis",
          backgroundcolor="#333333",
          gridcolor="#999999",
          showbackground=TRUE,
          zerolinecolor="#AAAAAA"
        ),
        zaxis=list(
          title = "Value(Z axis)",
          range = c(-0.3, 0.3),
          backgroundcolor="#333333",
          gridcolor="#999999",
          showbackground=TRUE,
          zerolinecolor="#AAAAAA"
        )
      )
  )
  saveWidget(fig, file = "naive.html",selfcontained = FALSE,libdir = "plotly_lib",background = "#333333")
}

.PlugInBayes <- function(){
  colormap <- c("palegreen", "saddlebrown")

  #points
  points <- matrix(
    #(x, y, class)
    c(0.2, 0.2,   1,
      0.1, 0.0, 1,
      0.2, -0.2, 1,
      -0.2, 0.2,2,
      -0.1, 0, 2,
      -0.3, 0, 2,
      -0.2,  -0.2,  2)
  ,ncol = 3, byrow=T)

  #generate data points
  #set.seed(1488)
  #genDat1 <- MASS::mvrnorm(100, mu = c(2,-2), Sigma = matrix(c(2, -1, -1, 2) ,nrow = 2))
  #set.seed(1488)
  #genDat2 <- MASS::mvrnorm(100, mu = c(-2,2), Sigma = matrix(c(1, 0, 0, 3) ,nrow = 2))
  #points <- rbind( cbind(genDat1, 1), cbind(genDat2, 2))

  #estimate
  u1 = muEstimate(points, 1)
  u2 = muEstimate(points, 2)

  M1 = sigmaEstimate(points, 1, u1)
  M2 = sigmaEstimate(points, 2, u2)

  print(points)
  print(u1)
  print(u2)
  print(M1)
  print(M2)

  #draw
  z_func <- function(x){ return( gausDistr(x, u1, M1) - gausDistr(x, u2, M2) ) }
  boundedBox <- c(-1,-1,1,1)
  #boundedBox <- c(
  #  floor(min(points[,1])),
  #  floor(min(points[,2])),
  #  ceiling(max(points[,1])),
  #  ceiling(max(points[,2]))
  #)
  coords = get3DSurface(boundedBox, z_func, delta = 0.02)

  fig <- plot_ly() %>% add_trace(
    x = ~coords$x,
    y = ~coords$y,
    z = ~coords$z,
    type = "contour",
    colorscale = list(c(0, colormap[2]),c(0.5, "yellow"),c(1, colormap[1])),
    contours = list(
      showlabels = TRUE,
      start = 0,
      end = 0,
      size = 4
    ),
    opacity = 0.6
  ) %>% add_trace(
    type = 'scatter',
    x = ~points[,1],
    y = ~points[,2],
    mode = "markers",
    marker = list(
      size = 10,
      color = colormap[points[,3]]
    )
  ) %>% colorbar(
    title = "Classes \n Likelyhood"
  )
  saveWidget(fig, file = "plug-in.html",selfcontained = FALSE,libdir = "plotly_lib",background = "#333333")
}

.LDF <- function(){
  colormap <- c("beige", "midnightblue")

  #points

  #generate data points
  set.seed(1488)
  genDat1 <- MASS::mvrnorm(100, mu = c(2,-2), Sigma = matrix(c(2, -1, -1, 2) ,nrow = 2))
  set.seed(1488)
  genDat2 <- MASS::mvrnorm(100, mu = c(-2,2), Sigma = matrix(c(1, 0, 0, 3) ,nrow = 2))
  points <- rbind( cbind(genDat1, 1), cbind(genDat2, 2))

  #estimate
  mus = list(muEstimate(points, 1),muEstimate(points, 2))

  AM = averageSigmaEstimate(points, mus)

  #draw
  z_func <- function(x){ return( gausDistr(x, mus[[ 1 ]], AM) - gausDistr(x, mus[[ 2 ]], AM) ) }
  boundedBox <- c(-1,-1,1,1)
  boundedBox <- c(
    floor(min(points[,1])),
    floor(min(points[,2])),
    ceiling(max(points[,1])),
    ceiling(max(points[,2]))
  )
  coords = get3DSurface(boundedBox, z_func, delta = 0.1)

  fig <- plot_ly() %>% add_trace(
    x = ~coords$x,
    y = ~coords$y,
    z = ~coords$z,
    type = "contour",
    colorscale = list(c(0, colormap[1]),c(1, colormap[2])),
    contours = list(
      showlabels = TRUE,
      start = 0,
      end = 0,
      size = 4
    ),
    opacity = 0.6
  ) %>% add_trace(
    type = 'scatter',
    x = ~points[,1],
    y = ~points[,2],
    mode = "markers",
    marker = list(
      size = 10,
      color = colormap[points[,3]],
      line = list(
        color = 'rgba(152, 0, 0, .8)',
        width = 1
      )
    )
  ) %>% colorbar(
    title = "Classes \n Likelyhood"
  )
  saveWidget(fig, file = "ldf.html",selfcontained = FALSE,libdir = "plotly_lib",background = "#333333")
}
######################################

.LevelLines()
#.LevelLines2()
#.NaiveBayes()
#.PlugInBayes()
#.LDF()
