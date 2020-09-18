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
  kNN <- function(dat, point, k = 1)
  {
      ##  Сортируем выборку согласно классифицируемого объекта
      orderedDat <- dist_sort(dat, point)
      n <- dim(orderedDat)[2] - 1

      ##  Получаем классы первых k соседей
      classes <- orderedDat[1:k, n + 1]

      ##  Составляем таблицу встречаемости каждого класса
      counts <- table(classes)

      ##  Находим класс, который доминирует среди первых k соседей
      class <- names(which.max(counts))

      return (class)
  }

LOO <- function(dat, func, maxK = 1, l = 2){
    print(paste0("LOO for k=",maxK,";l=",l))
    indexses = sample(1:150, l, replace = F)
    rat = rep(0, maxK)
    selection = dat[indexses,]
    sel_dim = dim(selection)[2] - 1

    for(i in 1:l){
        new_selection = selection[-i,]
        point = selection[i,]

        for(k in 1:maxK){
            print(paste(i,k))
            if(point[1, sel_dim+1] != kNN(
              selection[, 1:sel_dim],
              point[1:sel_dim],
              k=k
            )){
                rat[k] = rat[k]+1
            }
        }
    }
    print(rat)
    return (rat)
}

#paint iris
png(paste0("kNN_plot", ".png"))
colors <- c(
    "setosa" = "orange",
    "versicolor" = "violet",
    "virginica" = "purple"
)
plot(iris[, c(1,4)],
     pch = 21,
     bg = colors[iris$Species],
     col = colors[iris$Species]
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
cat("Points:\n")

for(point in points){
    print(point)
    class = kNN(iris[, c(1, 4, 5)], point)

    #paint points
    points(point[1],point[2],
          pch = 24,
          bg = colors[class],
          col = colors[class])
}
dev.off()

#calc LOO for kNN
rating = LOO(iris[, 3:5], kNN, maxK = 15, l=20)

png(paste0("LOO_kNN_plot", ".png"))
plot(
  data.frame(k=1:length(rating),Rating=rating),
  type="l",
  col="red"
)
dev.off()
