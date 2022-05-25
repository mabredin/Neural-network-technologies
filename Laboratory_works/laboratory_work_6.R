library(kohonen)

data("wines")
set.seed(5)

# Загрузка данных из файла
data.wines <- read.csv("winequality-white.csv", header = TRUE, sep = ";", 
                       quote = "\"", dec = ".", fill = TRUE, comment.char = "", 
                       encoding = "unknown")

# Формирование SOM карты
sommap <- som(scale(data.wines), grid = somgrid(10, 17, "hexagonal"))

# Разбиение карты на отдельные кластеры
groups <- 3
som.hc <- cutree(hclust(dist(sommap$codes[[1]])), groups)

# Построение SOM карты
plot(sommap, type="codes", bgcol=rainbow(groups)[som.hc], main="SOM карта вин")

# Прорисовка кластеров на карте
add.cluster.boundaries(sommap, som.hc)
