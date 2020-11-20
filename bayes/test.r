library(plotly)
library(htmlwidgets)
args <- commandArgs(trailingOnly = T)

gausDistr <- function(x, u, SigmMatr){
    # %*% - matrix multiply
    return( exp(-0.5 * t(x - u) %*% solve(SigmMatr) %*% (x - u)) / sqrt((2 * pi) ^ length(x) * det(SigmMatr)) )
}

get3DMesh <- function(boundedBox, z_func, delt = 0.1){
  #boundedBox - c(minX, minY, maxX, maxY)
  x = seq(boundedBox[1], boundedBox[3], delt)
  y = seq(boundedBox[2], boundedBox[4], delt)
  z = c()
  for(xi in x){
    z_line <- rep(0, length(y))
    z_index = 1
    for(yi in y){
        z_line[z_index] <- z_func(c(xi,yi))
        z_index <- z_index + 1
    }
    z <- rbind(z, z_line)
  }
  return(list(x = x, y = y, z = z))
}

####################################
.LevelLines <- function(){
  u <- c(0, 0)
  SigmMatr <- matrix(c(1, 0, 0, 1), nrow = 2)
  #1 2
  #3 4
  z_func <- function(x){
    return( gausDistr(x, u, SigmMatr) )
  }

  coords <- get3DMesh(c(-4,-4,4,4), z_func)
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
  saveWidget(fig, file = "contour.html",selfcontained = FALSE,background = "#333333")
}

.LevelLines2 <- function(){
  u1 <- c(0.8, -1)
  SigmMatr1 <- matrix(c(2, 1, 0, 0.5), nrow = 2)
  u2 <- c(-1, 1.5)
  SigmMatr2 <- matrix(c(0.8, 0, -1, 1.2), nrow = 2)
  #1 2
  #3 4
  z_func <- function(x){
    return( gausDistr(x, u1, SigmMatr1) - gausDistr(x, u2, SigmMatr2) )
  }

  coords <- get3DMesh(c(-4,-4,4,4), z_func)
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
        start = -0.4,
        end = 0.4,
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
          range = c(-0.4, 0.4),
          backgroundcolor="#555555",
          gridcolor="#999999",
          showbackground=TRUE,
          zerolinecolor="#AAAAAA"
        )
      )
  )
  saveWidget(fig, file = "contour2.html",selfcontained = FALSE,background = "#333333")
}
######################################

#.LevelLines()
.LevelLines2()
