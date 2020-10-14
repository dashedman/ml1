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

fast_kwnn_LOO <- function(selection, q=0.9){
    l = dim(selection)[1]
    rat = rep(0, l-1)
    w <- rep(0, l)
    for(i in 1:l){
      w[i] = q^i
    }

    for(i in 1:l){
        dat = selection[-i,]
        point = selection[i,]

        n <- dim(dat)[2] - 1
        orderedDat <- dist_sort(dat, point[1:n], euclid_distance)
        classes = rep(0, length(levels(orderedDat[, n+1])))

        for(k in 1:(l-1)){
            classes[orderedDat[k, n+1]] = classes[orderedDat[k, n+1]] + w[k]
            class <- levels(orderedDat[, n+1])[which.max(classes)]
            if(point[1, n+1] != class){
                rat[k] = rat[k] + 1
            }
        }
    }
    return (list(rating=rat, selection=selection))
}

SqrKernel <- function(x){
    return((if(abs(x) < 1) 0.5 else 0))
}
TriKernel <- function(x){
    return((if(abs(x) < 1) 1-abs(x) else 0))
}
QuadKernel <- function(x){
    return((if(abs(x) < 1) (15/16)*(1-abs(x)**2)**2 else 0))
}
EpanKernel <- function(x){
    return((if(abs(x) < 1) (3/4)*(1-abs(x)**2) else 0))
}
GausKernel <- function(x){
    return(1/sqrt(2*pi*exp(1)**(x*x)))
}

parzenWind <- function(x, select, h=1, kernel = SqrKernel, dist_func = euclid_distance){

    l <- dim(select)[1]
    n <- dim(select)[2] - 1

    classes = rep(0, length(levels(select[, n+1])))

    for(i in 1:l) {
        tmp_sel = select[i,1:(n+1)]
        classes[tmp_sel[,n+1]] <- classes[tmp_sel[, n+1]] + kernel(dist_func(tmp_sel[, 1:n], x) / h)
    }

    # Проверяем, что хоть один класс попал в окно
    if (max(classes) != 0 ) {
      # Возвращаем имя класса, у которого максимальное кол-во "голосов"
      return(levels(select[, n+1])[which.max(classes)])

    }

    # Если никакой класс не попал в окно, возвращаем другой класс
    return(0)
}

fast_parz_LOO <- function(selection, kernel = SqrKernel){
  l = dim(selection)[1]
  rat = rep(0, 12)

  for(i in 1:l){
      print(i)
      dat = selection[-i,]
      point = selection[i,]
      n <- dim(dat)[2] - 1

      #orderedDat <- dist_sort(dat, point[1:n], euclid_distance)
      #classes = rep(0, length(levels(orderedDat[, n+1])))

      for(k in 1:12){
      #    classes[orderedDat[k, n+1]] = classes[orderedDat[k, n+1]] + w[k]
          h = k/4
          class <- parzenWind(point[, 1:n], dat, h=h, kernel = kernel)#levels(orderedDat[, n+1])[which.max(classes)]
          if(point[1, n+1] != class){
              rat[k] = rat[k] + 1
          }
      }
  }
  return (list(rating=rat, selection=selection))
}

potentMethod <- function(distances, potentials, dat, h=1, kernel = SqrKernel){
  l <- dim(dat)[1]
  n <- dim(dat)[2] - 1

  classes <- dat[, n+1]
}

getPotentials <- function(dat, h=1, eps=10, kernel = SqrKernel){
  l <- dim(dat)[1]
  n <- dim(dat)[2] - 1

  potentials <- rep(0, l)
  err <- eps+1

  distances <- matrix(0, l, l)
  for(i in 1:l){
      point = dat[i, 1:n]
      for(j in 1:l){
          distances[i,j] = dist_func(point, dat[j, 1:n])
      }
  }

  while(err>eps){
      while(T){
          indexR = sample(1:l, 1)
          class <- potentMethod(distances[indexR], potentials, dat, h, kernel)
      }
  }
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
    png(paste0("6NN_map_plot", ".png"), width = 540, height = 270)
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
    result = fast_kwnn_LOO(iris[, 3:5],q=1)
    png(paste0("LOO_kNN_plot", ".png"), width = 1080, height = 540)
    par(mfrow=c(1,2))
    plot(
      data.frame(k=1:length(result$rating),LOO=result$rating),
      type="l",
      col="red",
      ylim = c(0, 150),
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
      col = colors[result$selection$Species],
      asp = T,
      main = "Classification map for 6NN"
    )
    x = seq(
      result$selection[which.min(result$selection[, 1]), 1],
      result$selection[which.max(result$selection[, 1]), 1],
      0.1
    )
    y = seq(
      result$selection[which.min(result$selection[, 2]), 2],
      result$selection[which.max(result$selection[, 2]), 2],
      0.1
    )
    for(xi in x){
        for(yi in y){
            points(xi,yi,pch=1,col = colors[kwNN(result$selection[, 1:3], c(xi,yi), k=6)])
        }
    }
    dev.off()
}

#calc LOO for kNN
.LOO_map_w_15_20 <- function(){
    result = fast_kwnn_LOO(iris[, 3:5], q = 0.9994)

    png(paste0("LOO_kwNN_plot", ".png"), width = 1080, height = 540)
    par(mfrow=c(1,2))
    plot(
      data.frame(k=1:length(result$rating), LOO=result$rating),
      type="l",
      col="red",
      ylim = c(0, 150),
      main = "Leave One Out estimate for kwNN algorithm; q=0.9994"
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
      main = "Classification map for 3wNN"
    )
    x = seq(
      result$selection[which.min(result$selection[, 1]), 1],
      result$selection[which.max(result$selection[, 1]), 1],
      0.1
    )
    y = seq(
      result$selection[which.min(result$selection[, 2]), 2],
      result$selection[which.max(result$selection[, 2]), 2],
      0.1
    )
    for(xi in x){
        for(yi in y){
            points(xi,yi,pch=1,col = colors[kwNN(result$selection[, 1:3], c(xi,yi), k=3)])
        }
    }
    dev.off()
}


.BIG_LOO <- function(){
    print("q=1")
    png(paste0("big_LOO", ".png"), width = 1920, height = 1080)
    par(bg = "#feffff")

    result = fast_kwnn_LOO(iris[, 3:5], q=1)
    plot(
      data.frame(k=1:length(result$rating), LOO=result$rating),
      type="l",
      col="red",
      ylim = c(0, dim(result$selection)[1]),
      main = "Leave One Out estimate for kwNN algorithm; w=q^i"
    )
    points(
      which.min(result$rating),
      result$rating[which.min(result$rating)],
      pch = 21,
      col = "red",
      bg = "red"
    )
    print(which.min(result$rating));print(result$rating[which.min(result$rating)])

    print("q=0.9")
    result = fast_kwnn_LOO(iris[, 3:5], q=0.9999)
    lines(
      1:length(result$rating),
      y=result$rating,
      col="darkorchid4"
    )
    points(
      which.min(result$rating),
      result$rating[which.min(result$rating)],
      pch = 21,
      col = "darkorchid4",
      bg = "darkorchid4"
    )
    print(which.min(result$rating));print(result$rating[which.min(result$rating)])

    print("q=0.7")
    result = fast_kwnn_LOO(iris[, 3:5], q=0.9994)
    lines(
      1:length(result$rating),
      y=result$rating,
      col="deepskyblue4"
    )
    points(
      which.min(result$rating),
      result$rating[which.min(result$rating)],
      pch = 21,
      col = "deepskyblue4",
      bg = "deepskyblue4"
    )
    print(which.min(result$rating));print(result$rating[which.min(result$rating)])

    print("q=0.5")
    result = fast_kwnn_LOO(iris[, 3:5], q=0.999)
    lines(
      1:length(result$rating),
      y=result$rating,
      col="darkseagreen"
    )
    points(
      which.min(result$rating),
      result$rating[which.min(result$rating)],
      pch = 21,
      col = "darkseagreen",
      bg = "darkseagreen"
    )
    print(which.min(result$rating));print(result$rating[which.min(result$rating)])

    print("q=0.3")
    result = fast_kwnn_LOO(iris[, 3:5], q=0.99)
    lines(
      1:length(result$rating),
      y=result$rating,
      col="forestgreen"
    )
    points(
      which.min(result$rating),
      result$rating[which.min(result$rating)],
      pch = 21,
      col = "forestgreen",
      bg = "forestgreen"
    )
    print(which.min(result$rating));print(result$rating[which.min(result$rating)])

    print("q=0.1")
    result = fast_kwnn_LOO(iris[, 3:5], q=0.9)
    lines(
      1:length(result$rating),
      y=result$rating,
      col="lawngreen"
    )
    points(
      which.min(result$rating),
      result$rating[which.min(result$rating)],
      pch = 21,
      col = "lawngreen",
      bg = "lawngreen"
    )
    print(which.min(result$rating));print(result$rating[which.min(result$rating)])

    legend(
      x=0, y=150,
      legend=c("kNN","kwNN, q=0.9999","kwNN, q=0.9994","kwNN, q=0.999","kwNN, q=0.99","kwNN(1NN), q=0.9"),
      col = c("red", "darkorchid4", "deepskyblue4", "darkseagreen", "forestgreen", "lawngreen")
    )
    dev.off()
}


.Windows6 <- function(){
    result = fast_parz_LOO(iris[,3:5], kernel = SqrKernel)
    png(paste0("parzens_sqr", ".png"), width = 1080, height = 540)
    par(mfrow=c(1,2))
    plot(
      result$selection[, 1:2],
      pch = 21,
      bg = colors[result$selection$Species],
      col = colors[result$selection$Species],
      asp = T,
      main = "Classification map for Parzen's Window(Square kern)"
    )
    x = seq(
      result$selection[which.min(result$selection[, 1]), 1],
      result$selection[which.max(result$selection[, 1]), 1],
      0.1
    )
    y = seq(
      result$selection[which.min(result$selection[, 2]), 2],
      result$selection[which.max(result$selection[, 2]), 2],
      0.1
    )
    for(xi in x){
        print(xi)
        for(yi in y){
            points(xi,yi,pch=1,col = colors[parzenWind(c(xi, yi), iris[, 3:5], kernel = SqrKernel)])
        }
    }
    plot(
      data.frame(h=(1:12) / 4, LOO=result$rating),
      type="l",
      col="red",
      ylim = c(0, 150),
      main = "Leave One Out estimate for Parzen's Window(Square kern)"
    )
    points(
      which.min(result$rating)/4,
      result$rating[which.min(result$rating)],
      pch = 21,
      col = "red"
    )
    print(which.min(result$rating))
    print(result$rating[which.min(result$rating)])
    dev.off()


    result = fast_parz_LOO(iris[,3:5], kernel = TriKernel)
    png(paste0("parzens_tri", ".png"), width = 1080, height = 540)
    par(mfrow=c(1,2))
    plot(
      result$selection[, 1:2],
      pch = 21,
      bg = colors[result$selection$Species],
      col = colors[result$selection$Species],
      asp = T,
      main = "Classification map for Parzen's Window(Triange kern)"
    )
    x = seq(
      result$selection[which.min(result$selection[, 1]), 1],
      result$selection[which.max(result$selection[, 1]), 1],
      0.1
    )
    y = seq(
      result$selection[which.min(result$selection[, 2]), 2],
      result$selection[which.max(result$selection[, 2]), 2],
      0.1
    )
    for(xi in x){
        print(xi)
        for(yi in y){
            points(xi,yi,pch=1,col = colors[parzenWind(c(xi, yi), iris[, 3:5], kernel = TriKernel)])
        }
    }
    plot(
      data.frame(h=(1:12) / 4, LOO=result$rating),
      type="l",
      col="red",
      ylim = c(0, 150),
      main = "Leave One Out estimate for Parzen's Window(Triange kern)"
    )
    points(
      which.min(result$rating)/4,
      result$rating[which.min(result$rating)],
      pch = 21,
      col = "red"
    )
    print(which.min(result$rating))
    print(result$rating[which.min(result$rating)])
    dev.off()


    result = fast_parz_LOO(iris[,3:5], kernel = QuadKernel)
    png(paste0("parzens_quad", ".png"), width = 1080, height = 540)
    par(mfrow=c(1,2))
    plot(
      result$selection[, 1:2],
      pch = 21,
      bg = colors[result$selection$Species],
      col = colors[result$selection$Species],
      asp = T,
      main = "Classification map for Parzen's Window(Quadratic kern)"
    )
    x = seq(
      result$selection[which.min(result$selection[, 1]), 1],
      result$selection[which.max(result$selection[, 1]), 1],
      0.1
    )
    y = seq(
      result$selection[which.min(result$selection[, 2]), 2],
      result$selection[which.max(result$selection[, 2]), 2],
      0.1
    )
    for(xi in x){
        print(xi)
        for(yi in y){
            points(xi,yi,pch=1,col = colors[parzenWind(c(xi, yi), iris[, 3:5], kernel = QuadKernel)])
        }
    }
    plot(
      data.frame(h=(1:12) / 4, LOO=result$rating),
      type="l",
      col="red",
      ylim = c(0, 150),
      main = "Leave One Out estimate for Parzen's Window(Quadratic kern)"
    )
    points(
      which.min(result$rating)/4,
      result$rating[which.min(result$rating)],
      pch = 21,
      col = "red"
    )
    print(which.min(result$rating))
    print(result$rating[which.min(result$rating)])
    dev.off()


    result = fast_parz_LOO(iris[,3:5], kernel = EpanKernel)
    png(paste0("parzens_epan", ".png"), width = 1080, height = 540)
    par(mfrow=c(1,2))
    plot(
      result$selection[, 1:2],
      pch = 21,
      bg = colors[result$selection$Species],
      col = colors[result$selection$Species],
      asp = T,
      main = "Classification map for Parzen's Window(Epanechnikov kern)"
    )
    x = seq(
      result$selection[which.min(result$selection[, 1]), 1],
      result$selection[which.max(result$selection[, 1]), 1],
      0.1
    )
    y = seq(
      result$selection[which.min(result$selection[, 2]), 2],
      result$selection[which.max(result$selection[, 2]), 2],
      0.1
    )
    for(xi in x){
        print(xi)
        for(yi in y){
            points(xi,yi,pch=1,col = colors[parzenWind(c(xi, yi), iris[, 3:5], kernel = EpanKernel)])
        }
    }
    plot(
      data.frame(h=(1:12) / 4, LOO=result$rating),
      type="l",
      col="red",
      ylim = c(0, 150),
      main = "Leave One Out estimate for Parzen's Window(Epanechnikov kern)"
    )
    points(
      which.min(result$rating)/4,
      result$rating[which.min(result$rating)],
      pch = 21,
      col = "red"
    )
    print(which.min(result$rating))
    print(result$rating[which.min(result$rating)])
    dev.off()


    result = fast_parz_LOO(iris[,3:5], kernel = GausKernel)
    png(paste0("parzens_gaus", ".png"), width = 1080, height = 540)
    par(mfrow=c(1,2))
    plot(
      result$selection[, 1:2],
      pch = 21,
      bg = colors[result$selection$Species],
      col = colors[result$selection$Species],
      asp = T,
      main = "Classification map for Parzen's Window(Gaus kern)"
    )
    x = seq(
      result$selection[which.min(result$selection[, 1]), 1],
      result$selection[which.max(result$selection[, 1]), 1],
      0.1
    )
    y = seq(
      result$selection[which.min(result$selection[, 2]), 2],
      result$selection[which.max(result$selection[, 2]), 2],
      0.1
    )
    for(xi in x){
        print(xi)
        for(yi in y){
            points(xi,yi,pch=1,col = colors[parzenWind(c(xi, yi), iris[, 3:5], kernel = GausKernel)])
        }
    }
    plot(
      data.frame(h=(1:12) / 4, LOO=result$rating),
      type="l",
      col="red",
      ylim = c(0, 150),
      main = "Leave One Out estimate for Parzen's Window(Gaus kern)"
    )
    points(
      which.min(result$rating)/4,
      result$rating[which.min(result$rating)],
      pch = 21,
      col = "red"
    )
    print(which.min(result$rating))
    print(result$rating[which.min(result$rating)])
    dev.off()
}


.Potention <- function(){
  result = fast_parz_LOO(iris[,3:5], kernel = SqrKernel)
  png(paste0("parzens_sqr", ".png"), width = 1080, height = 540)
  par(mfrow=c(1,2))
  plot(
    result$selection[, 1:2],
    pch = 21,
    bg = colors[result$selection$Species],
    col = colors[result$selection$Species],
    asp = T,
    main = "Classification map for Parzen's Window(Square kern)"
  )
  x = seq(
    result$selection[which.min(result$selection[, 1]), 1],
    result$selection[which.max(result$selection[, 1]), 1],
    0.1
  )
  y = seq(
    result$selection[which.min(result$selection[, 2]), 2],
    result$selection[which.max(result$selection[, 2]), 2],
    0.1
  )
  for(xi in x){
      print(xi)
      for(yi in y){
          points(xi,yi,pch=1,col = colors[parzenWind(c(xi, yi), iris[, 3:5], kernel = SqrKernel)])
      }
  }
  plot(
    data.frame(h=(1:12) / 4, LOO=result$rating),
    type="l",
    col="red",
    ylim = c(0, 150),
    main = "Leave One Out estimate for Parzen's Window(Square kern)"
  )
  points(
    which.min(result$rating)/4,
    result$rating[which.min(result$rating)],
    pch = 21,
    col = "red"
  )
  print(which.min(result$rating))
  print(result$rating[which.min(result$rating)])
  dev.off()
}
######################################

colors <- c(
    "setosa" = "orange",
    "versicolor" = "forestgreen",
    "virginica" = "purple"
)

#.random_1NN()
#.map_6NN()
#.LOO_15_20()
#.LOO_map_w_15_20()
#.BIG_LOO()
#.Windows6()
.Potention()
