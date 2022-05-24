library(nnet)
library(NeuralNetTools)

# Загрузка данных во фрейм data.x_ray
data.x_ray <- read.table("column_3C.dat")

# Исключение столбца v7
data.x_ray$V7 <- NULL

# Формирование выходных реакций в виде трехкомпонентного вектора
targets <- class.ind(c(rep('DH', 60), rep('SL', 150), rep('NO', 100)))

# Формирование номеров векторов тренировочного набора
set.seed(100)
samp <- c(sample(1:60, 45), sample(61:210, 113), sample(211:310, 75))

# Формирование обучающего и тестового наборов
train.set <- data.x_ray[samp,]
test.set <- data.x_ray[-samp,]

# Обучение сети net.x_ray
set.seed(100)
net.x_ray <- nnet(train.set, targets[samp,], size = 16, rang=0.3, maxit = 1000)

# Проверка сети на тестовом наборе
net.pred <- predict(net.x_ray, test.set)

# Построение архитектуры сети
plotnet(net.x_ray)

# Построение проверочной таблицы
table(max.col(targets[-samp,]), max.col(net.pred))




