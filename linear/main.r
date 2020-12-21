library(plotly)
library(htmlwidgets)
library(MASS)
library(pracma)
args <- commandArgs(trailingOnly = T)

stohastGrad <- function(dat, L, dL, etha = 1, lambda = 0.5, history = F){

  l <- dim(dat)[1]
  n <- dim(dat)[2] - 1

  omega <- runif(n, -0.5, 0.5)
  omegaHist <- matrix(c(omega), ncol = n)

  Q <- 0
  for(i in 1:l){
    x = dat[i,1:n]
    y = dat[i,n+1]
    Q <- Q + L(x, y, omega)
  }
  QHist <- c(Q)

  for(it in 1:10000){
    randIndex <- sample(1:l, 1)

    eps <- L(dat[randIndex, 1:n], dat[randIndex, n+1], omega)

    step <- etha*dL(dat[randIndex, 1:n], dat[randIndex, n+1], omega)
    newQ <-  (1-lambda)*Q + lambda*eps

    if(all(abs(step) < 0.0005) && all(abs(Q - newQ) < 0.00001)){
      print(paste("break", it))
      break
    }

    omega <- omega - step
    Q <- newQ

    if(history){
      QHist <- c(QHist, Q)
      omegaHist <- rbind(omegaHist, omega)
    }
  }

  return(list(omega=omega, omegaHist=omegaHist, QHist = QHist))
}


####################################
.ADAandHebb <- function(){
  #generate data
  set.seed(1488)
  genDat1 <- MASS::mvrnorm(100, mu = c(3,-1), Sigma = matrix(c(1, 0, 0, 1) ,nrow = 2))
  set.seed(1488)
  genDat2 <- MASS::mvrnorm(100, mu = c(-3,2), Sigma = matrix(c(1, 0, 0, 1) ,nrow = 2))
  set.seed(1488)
  points <- rbind( cbind(cbind(genDat1,-1), 1), cbind(cbind(genDat2,-1), -1))


  #clac
  adaL <- function(xi, yi, omega){
    return((dot(omega, xi)*yi-1)^2)
  }
  adadL <- function(xi, yi, omega){
    return(2*(dot(omega, xi) - yi)*xi)
  }
  hebbL <- function(xi, yi, omega){
    return(max(-1*dot(omega, xi)*yi, 0))
  }
  hebbdL <- function(xi, yi, omega){
    return(if(dot(omega, xi)*yi > 0) -yi*xi else 0)
  }

  res <- stohastGrad(points, hebbL, hebbdL, etha = 1, lambda = 0.1, history = T)
  history_len <- dim(res$omegaHist)[1]-1

  #draw
  boundedBox <- c(-1,-1,1,1)
  boundedBox <- c(
    floor(min(points[,1])),
    floor(min(points[,2])),
    ceiling(max(points[,1])),
    ceiling(max(points[,2]))
  )

  lines = matrix(NA, ncol = 2, nrow = history_len*3)
  lastline = matrix(NA, ncol = 2, nrow = 2)

  if(history_len>0)
    for(i in 1:history_len){
      omega <- res$omegaHist[i,]
      lines[3*i-2,  1] = boundedBox[1]
      lines[3*i-2,  2] = omega[3]/omega[2] * boundedBox[1] - omega[1]/omega[2]
      lines[3*i-1,1] = boundedBox[3]
      lines[3*i-1,2] = omega[3]/omega[2] * boundedBox[3] - omega[1]/omega[2]
      lines[3*i,1] = NA
      lines[3*i,2] = NA
    }
  omega <- res$omega
  lastline[1,1] = boundedBox[1]
  lastline[1,2] = omega[3]/omega[2] * boundedBox[1] - omega[1]/omega[2]
  lastline[2,1] = boundedBox[3]
  lastline[2,2] = omega[3]/omega[2] * boundedBox[3] - omega[1]/omega[2]

  colormap <- c("palegreen","rgba(152, 152, 152, .1)", "saddlebrown","orange")
  fig <- plot_ly() %>% add_trace(
    type = 'scatter',
    x = ~lines[,1],
    y = ~lines[,2],
    mode = "lines",
    line = list(
      color = colormap[2]
    )
  ) %>% add_trace(
    type = 'scatter',
    x = ~lastline[,1],
    y = ~lastline[,2],
    mode = "lines",
    line = list(
      color = colormap[4]
    )
  ) %>% add_trace(
    type = 'scatter',
    x = ~points[,1],
    y = ~points[,2],
    mode = "markers",
    marker = list(
      size = 10,
      color = colormap[points[,4]+2],
      line = list(
        color = colormap[2],
        width = 1
      )
    )
  ) %>% layout(
    xaxis = list(range = c(boundedBox[1], boundedBox[3])),
    yaxis = list(range = c(boundedBox[2], boundedBox[4]))
  )
  saveWidget(fig, file = "hebb.html",selfcontained = FALSE,libdir = "plotly_lib",background = "#333333")


  fig <- plot_ly() %>% add_trace(
    type = 'scatter',
    x = 1:(history_len+1),
    y = ~res$QHist,
    mode = "lines",
    line = list(
      color = "red"
    )
  )
  saveWidget(fig, file = "hebbQ.html",selfcontained = FALSE,libdir = "plotly_lib",background = "#333333")
}
######################################

.ADAandHebb()
