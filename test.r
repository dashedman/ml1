distance = function(x, y){
  sqrt(sum((u-v)^2))
}

dist_sort = function(dat, point, dist_funct = distance){
  n = dim(x1)[1]
  m = dim(x1)[2] - 1
  
  distances = matrix(NA, 1, 2)
  
  for(i in 1:n){
    distances [i, ] = c(i, dist_func())
  }
}

colors <- c("setosa" = "orange", 
            "versitcolor" = "violet", 
            "virginica" = "purple")


plot(iris[, 2:3], 
     pch = 21, 
     bg = colors[iris$Species], 
     col = colors[iris$Species])


points(iris[, 2], 
       pch = 23, 
       bg = colors[iris$Species], 
       col = colors[iris$Species])
