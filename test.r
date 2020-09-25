args <- commandArgs(trailingOnly = T)

euclid_distance = function(x, y){
  sqrt(sum((x-y)^2))
}

dist_sort = function(dat, point, dist_func = euclid_distance){
      l <- dim(dat)[1]
      n <- dim(dat)[2] - 1

      distances <- matrix(NA, l, 1)

      for (i in 1:l){
          distances[i, ] <- dist_func(dat[i, 1:n], point)
      }

      ##  Сортируем
      orderedDat <- dat[order(distances[, 1]), ]

      return (orderedDat);
  }


  ##  Применяем метод kNN
  kNN <- function(dat, point, k = 1, dist_func = euclid_distance)
  {
      ##  Сортируем выборку согласно классифицируемого объекта
      orderedDat <- dist_sort(dat, point, dist_func)
      n <- dim(orderedDat)[2] - 1

      ##  Получаем классы первых k соседей
      classes <- orderedDat[1:k, n + 1]

      ##  Составляем таблицу встречаемости каждого класса
      counts <- table(classes)

      ##  Находим класс, который доминирует среди первых k соседей
      class <- names(which.max(counts))

      return (class)
  }


  kwNN <- function(dat, point, k = 1, dist_func = euclid_distance)
  {

      orderedDat <- dist_sort(dat, point, dist_func)
      n <- dim(orderedDat)[2] - 1

      w <- exp((-1:-k)/dim(orderedDat)[1])

      classes = rep(0, length(levels(orderedDat[, n+1])))
      for(i in 1:k){
          classes[orderedDat[i, n+1]] = classes[orderedDat[i, n+1]] + w[i]
      }

      class <- levels(orderedDat[, n+1])[which.max(classes)]


      return (class)
  }

LOO <- function(dat, func, maxK = 1, l = 2){
    indexses = sample(1:150, l, replace = F)
    rat = rep(0, maxK)
    selection = dat[indexses,]
    sel_dim = dim(selection)[2] - 1

    for(i in 1:l){
        new_selection = selection[-i,]
        point = selection[i,]

        for(k in 1:maxK){
            class = func(
              new_selection,
              point[1:sel_dim],
              k=k
            )
            if(point[1, sel_dim+1] != class){
                rat[k] = rat[k] + 1
            }
        }
    }
    return (list(rating=rat, selection=selection))
}

####################################
.random_1NN <- function(){
    #1NN calc
    #paint iris
    png(paste0("1NN_plot", ".png"))
    plot(iris[, c(1,4)],
         pch = 21,
         bg = colors[iris$Species],
         col = colors[iris$Species],
         asp = T,
         main = "Classification of five random points with use of 1NN"
    )
    #generate data
    n = 5
    if(!is.na(args[1])) {
      n = args[1]
    }
    points = vector("list", n)
    for(i in 1:n){
      points[[i]] = c(runif(1, 4, 7), runif(1, 0, 2))
    }
    #use 1NN
    for(point in points){
        class = kNN(iris[, c(1, 4, 5)], point)
        #paint points
        points(point[1],point[2],
              pch = 24,
              bg = colors[class],
              col = colors[class])
    }
    dev.off()
}

#calc 6NN map
.map_6NN <- function(){
    print("map")
    png(paste0("6NN_map_plot", ".png"), width = 1080, height = 540)
    plot(
      iris[,3:4],
      pch = 21,
      bg = colors[iris$Species],
      col = colors[iris$Species],
      asp = T,
      main = "Classification map for 6NN algorithm"
    )
    x = seq(1,7,0.1)
    y = seq(0.1,2.5,0.1)
    cols = rep(0, 150)
    it = 1
    for(i in x){
        print(i)
        for(j in y){
            points(i, j, pch=1, col = colors[kNN(iris[,3:5], c(i,j), k=6)])
        }
    }

    dev.off()
}

#calc LOO for kNN
.LOO_15_20 <- function(){
    result = LOO(iris[, 3:5], kNN, maxK=19, l=20)
    png(paste0("LOO_kNN_plot", ".png"), width = 1080, height = 540)
    par(mfrow=c(1,2))
    plot(
      data.frame(k=1:length(result$rating),LOO=result$rating),
      type="l",
      col="red",
      ylim = c(0, 20),
      main = "Leave One Out estimate for kNN algorithm"
    )
    points(
      which.min(result$rating),
      result$rating[which.min(result$rating)],
      pch = 21,
      col = "red"
    )
    plot(
      result$selection[, 1:2],
      pch = 21,
      bg = colors[result$selection$Species],
      col = colors[result$selection$Species]
    )
    dev.off()
}

#calc LOO for kNN
.LOO_map_w_15_20 <- function(){
    result = LOO(iris[, 3:5], kwNN, maxK=19, l=20)

    png(paste0("LOO_kwNN_plot", ".png"), width = 1080, height = 540)
    par(mfrow=c(1,2))
    plot(
      data.frame(k=1:length(result$rating), LOO=result$rating),
      type="l",
      col="red",
      ylim = c(0, 20),
      main = "Leave One Out estimate for kwNN algorithm"
    )
    points(
      which.min(result$rating),
      result$rating[which.min(result$rating)],
      pch = 21,
      col = "red"
    )
    plot(
      result$selection[, 1:2],
      pch = 21,
      bg = colors[result$selection$Species],
      col = colors[result$selection$Species],
      asp = T,
      main = "Classification map for 5wNN"
    )
    for(x in seq(1,8,0.1)){
        for(y in seq(0.1,4.5,0.1)){
            points(x,y,pch=1,col = colors[kwNN(result$selection[, 1:3], c(x,y), k=5)])
        }
    }
    dev.off()

    png(paste0("w_func", ".png"), width = 1080, height = 540)
    plot(
      data.frame("x"=1:50, "w[x]"=exp((-1:-50)/50)),
      type="l",
      col="red",
      ylim = c(0,1)
    )
    dev.off()
}




######################################

colors <- c(
    "setosa" = "orange",
    "versicolor" = "forestgreen",
    "virginica" = "purple"
)

.random_1NN()
#.map_6NN()
#.LOO_15_20()
#.LOO_map_w_15_20()
