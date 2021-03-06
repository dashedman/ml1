Метрические Алгоритмы
============================
kNN
-----------------------------
Алгоритм находит ближайшие k обучающих объектов относительно искомого и классифицирует его, исходя из частоты встретившихся классов из этих объектов.

![1NN - график для 5 точек](1NN_plot.png)


```R
#dat - данные в формате: список векторов типа [ x1, x2, ... , xn, class]
#где x1, ..., xn - координаты екземпляра
#p - данные в формате: [ x1, x2, ... , xn]
#по умолчанию k=1
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
```

![LOO для kNN - оценка для kNN](LOO_kNN_plot.png)

kwNN
-----------------------------------
~~КВН~~

Алгоритм работает подобно алгоритму kNN, но для определения класса искомой точки использует не наиболее встречающийся класс из k ближайших точек, а считает сумму весов из ближайших обучающих точек для каждого класса. По наибольшей сумме весов определяется класс точки.

```R
#dat - данные в формате: список векторов типа [ x1, x2, ... , xn, class]
#где x1, ..., xn - координаты екземпляра
#point - данные в формате: [ x1, x2, ... , xn]
#по умолчанию k=1
kwNN <- function(dat, point, k = 1)
{
    #в качестве функции весов берем убывающую неотрицательную e^(-x/l)
    w <- exp(-1:-k)

    #сортируем по расстоянию
    orderedDat <- dist_sort(dat, point, dist_func)
    n <- dim(orderedDat)[2] - 1

    #создаем счетчик весов для классов
    classes = rep(0, length(levels(orderedDat[, n+1])))

    #прибавляем веса к счетчику, исходя из ближайших найденных классов
    for(i in 1:k){
        classes[orderedDat[i, n+1]] + w[i]
    }

    #определяем максимальный счетчик и привязанный к нему класс
    class <- levels(orderedDat[, n+1])[which.max(classes)]

    return (class)
}
```
![LOO для kwNN - оценка для kwNN](LOO_kwNN_plot.png)

Заключение
-----------------------
kNN и kwNN довольно простые в реализации алгоритмы, но имеют довольно большое время выполнения.
Исходя из скользящих оценок, алгоритмы kNN и kwNN теряют свою надежность по мере возрастания k. Оптимальные k:

Таблица для kwNN, w = {x: x=q^i, 0<q<1, i for (1,k)}:

| q-параметр для W | Оптимальный k | Погрешность |
|:----------------:| -------------:| -----------:|
| 1.0              | 6             | 5/150       |
| 0.9999           | 3             | 6/150       |
| 0.9994           | 3             | 6/150       |
| 0.999            | 3             | 6/150       |
| 0.99             | 3             | 6/150       |
| 0.9              | 3             | 6/150       |

![LOO for some kwNN](big_LOO.png)

Парзеновское окно
------------------------
Алгоритм похож принципом работы на kwNN, только k будет равнятся длине всей выборки, а весовая функция будет считатся в зависимости от растояния.
Сама весовую функция будем называть ядром. Ядро должно быть четной невозврасающей функцией на промежутке [0,+8)

Существует 5 классических ядер для этого метода:
- Квадратное: y = 0.5 if |x| < 1 else 0
- Треугольное: y = 1-x if |x| < 1 else 0
- Квадратическое: y = (15/16) * (1-x^2)^2 if |x| < 1 else 0
- Епаничникова: y = (3/4) * (1-x^2) if |x| < 1 else 0
- Гауса: y = 1/sqrt( 2\*PI\*e^(x^2) )

Все выше перечисленные ядра обладают одним свойством: их площадь равна еденице. В будущем это позволит рассматривать алгоритм как Баесовский.

Код алгоритма:
```R
#dat - данные в формате: список векторов типа [ x1, x2, ... , xn, class]
#где x1, ..., xn - координаты екземпляра
#x - данные в формате: [ x1, x2, ... , xn]
#h - ширина окна double
#kernel - функция ядра

parzenWind <- function(x, select, h=1, kernel = SqrKernel, dist_func = euclid_distance){

    #получаем размеры выборки
    l <- dim(select)[1]
    n <- dim(select)[2] - 1

    #создаем счетик для классов
    classes = rep(0, length(levels(select[, n+1])))

    #проходимся по выборке считаем дистанцию и ядро
    for(i in 1:l) {
        tmp_sel = select[i,1:(n+1)]
        classes[tmp_sel[,n+1]] <- classes[tmp_sel[, n+1]] + kernel(dist_func(tmp_sel[, 1:n], x) / h)
    }

    # проверяем, что хоть один класс попал в окно
    if (max(classes) != 0 ) {
      # возвращаем имя класса, у которого максимальное кол-во "голосов"
      return(levels(select[, n+1])[which.max(classes)])

    }

    # Если никакой класс не попал в окно, возвращаем альтернативный класс
    return(0)
}
```

Ниже представленны карты классификаций и скользящие оценки по ширине окна для разных ядер:
![квадратное ядро](parzens_sqr.png)
![треугольное ядро](parzens_tri.png)
![квадратическое ядро](parzens_quad.png)
![епаничникова ядро](parzens_epan.png)
![гаусовское ядро](parzens_gaus.png)

Ниже представлена сравнительная таблица для разных ядер

| Ядро           | Оптимальный h | Погрешность |
|:--------------:| -------------:| -----------:|
| Квадратное     | 1             | 6/150       |
| треугольное    | 0.5           | 6/150       |
| Квадратическое | 0.5           | 6/150       |
| Епаничникова   | 0.5           | 6/150       |
| Гаусовское     | 0.25          | 6/150       |

Метод Потенциальных функций
------------------------
Алгоритм похож принципом работы на Парзеновское Окно, только ядра будут применяться не к искомому объекту, а к объектам из обучающей выборки, также к каждому такому объекту привязывается потенциал, обозначающий значимость данного объекта.

Перед применением самомго алгоритма классификации, следует получить список потенциалов. Благодаря этому списку, отпадает нужда сортировать обьекты во время классификации.

Код алгоритма:
```R
#distances - список векторов содержащий данные о дистанции искомого объекта к каждому объекту выборки
#potentials - список потенциалов для каждого объекта выборки
#dat - данные в формате: список векторов типа [ x1, x2, ... , xn, class]
#где x1, ..., xn - координаты екземпляра
#h - ширина окна double
#kernel - функция ядра

potentMethod <- function(distances, potentials, dat, h=1, kernel = SqrKernel){

  #получаем размеры выборки
  l <- dim(dat)[1]
  n <- dim(dat)[2] - 1

  #создаем счетик для классов
  weights <- rep(0, length(levels(dat[, n+1])))

  #проходимся по выборке считаем ядро
  for(i in 1:l){
      class <- dat[i, n+1]
      weights[class] <- weights[class] + potentials[i]*kernel(distances[i]/h)
  }

  # проверяем, что хоть один класс классифицировался
  if(max(weights) != 0) return(levels(dat[, n+1])[which.max(weights)])
  return(0)
}
```

Вспомогательный алгоритм для подсчета потенциалов:
```R
#dat - данные в формате: список векторов типа [ x1, x2, ... , xn, class]
#где x1, ..., xn - координаты екземпляра
#h - ширина окон double
#eps - допустимое разрешенное количество ошибок для массива потенциалов
#kernel - функция ядра
#dist_func - метрическая функция расстояния
getPotentials <- function(dat, h=1, eps=10, kernel = SqrKernel, dist_func = euclid_distance){
  l <- dim(dat)[1]
  n <- dim(dat)[2] - 1

  potentials <- rep(0, l)
  err = eps+1

  distances <- matrix(0, l, l)
  for(i in 1:l){
      point = dat[i, 1:n]
      for(j in 1:l){
          distances[i,j] = dist_func(point, dat[j, 1:n])
      }
  }

  while(err>eps){
      for(lim in 1:l){
          indexR = sample(1:l, 1)
          class <- potentMethod(distances[indexR,], potentials, dat, h, kernel)

          if(class != dat[indexR, n+1]){
              print(paste("New potention for",indexR))
              potentials[indexR] = potentials[indexR] + 1
              break
          }
      }

      err = 0
      for(i in 1:l){
          err = err + (potentMethod(
            distances[i,],
            potentials,
            dat, h, kernel
          ) != dat[i, n+1])
      }
  }
  return(potentials)
}
```
Ниже представленна карта классификации и карта потенциалов для Квадратного ядра:
![квадратное ядро](potent_sqr.png)

STOLP метод выбора эталонов
-------------------------------
Это вспомогательный алгоритм который позволяет значительно уменьшить размер выборки, при этом незначительно теряя в предсказательной точности. Алгорит занимается тем что отбрасывает шумовые объекты, и вычисляет эталонные.

> Данный листинг является реализацией алгоритма STOLP для метода парзеновского окна.

```R
#dat - data frame выборки, последний столбец фактор класс
#eps - допустимое количество ошибок
#delta - граница допустимого размера отступа
#далее аргументы для применяемого парзеновского окна
STOLP_parz <- function(dat, eps = 10, delta = 0, h=1, kernel = SqrKernel, dist_func = euclid_distance){
  #считаем отступы
  margin = getMarginParz(dat, h=h,kernel=kernel, dist_func=dist_func)

  #отбрасываем шумовые
  clearMarginIndexes = (margin > delta)
  margin = margin[clearMarginIndexes]
  dat = dat[clearMarginIndexes,]

  l <- dim(dat)[1]
  n <- dim(dat)[2] - 1
  classes = levels(dat[, n+1])

  #начальный омега
  OmegaLuL <- rep(0, length(classes))
  for(i in 1:length(classes)){
    OmegaLuL[i] <- getMaxForClass(margin , dat, classes[i])
  }

  #наращивание омега
  error_counter <- eps+1
  for(it in 1:100){
    #проверяем омега на валидность
    error_counter <- 0
    for(i in 1:l){
      x = dat[i,]
      if(parzenWind(x[,1:n], dat[OmegaLuL,], h, kernel, dist_func) != x[,n+1]) error_counter <- error_counter + 1
    }

    #дополняем омега
    if(error_counter > eps){
      OmegaLuL <- c(OmegaLuL, getAddOmegaVect(OmegaLuL, dat, h=h,kernel=kernel, dist_func=dist_func))
    }else{
      break;
    }
  }
  return(dat[OmegaLuL,])
}
```

![Отступы для ирисов](margin.png)

![Карта классификации](stolp_map.png)
### Сравнение показателей для Парзеновского окна на полной выборке и на эталонах(Гаусово ядро)

| h=1        | Полная выборка | Эталоны |
|:----------:| --------------:| -------:|
| Время(sec) | 57.73          | 1.27    |
| Ошибки     | 8/150          | 6/150   |

Имеем значительный прирост скорости классификации(аж на 4500%), и даже легкое увеличение точности на oбучающей выборке!

Сравнительная таблица для метрических классификаторов
-------------------------------


| Алгоритм       | Параметры            | Ошибка  |
|:--------------:| --------------------:| -------:|
| 1NN            |                      | 7/150   |
| kNN            | k=6                  | 5/150   |
| kwNN           | k=3                  | 6/150   |
| Parzen Windows | kernel:Gause; h=0.25 | 6/150   |
| STOLP for PW   | kernel:Gause; h=1    | 6/150   |

**Заключение.** Все рассмотреные метрические классификаторы относительно просты и имеют схожую скорость работы.
Применение алгоритма STOLP позволяет качественно ускорить классификацию метрическими алгоритмами.
