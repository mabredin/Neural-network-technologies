#Задание - 1
#формирование данных learn для обучения 
set.seed(9)
x <- runif(50,0,8)
y <- sin(x) / (cos(2*x) + 2)
plot(x, y, pch=16, cex=1.5, col='green')
learn = data.frame(x, y)

#загрузка библиотек и обучения сети netcos
library(neuralnet)
netcos <- neuralnet(y~x, hidden = 4, learn)

#формирование тестовых данных и проверка сети netcos
set.seed(6)
x <- runif(5000, 0, 8)
y <- 0
test = data.frame(x,y)
yp = predict(netcos, test)
points(x, yp, col = 'black', pch ='.', cex = 1.5)

#Архитектура сети netcos
plot(netcos)

# Задание - 2

# формирование данных learn для обучения 
x = seq(from = 0, to = 3.15, by = 0.05)
y = rep(c(1,1,1,1,0,0,0,0), 8)
learn = data.frame(x, y)

#Загрузка библиотек
library(neuralnet)

# Создание функции для обучения сети net, формирования 
# тестовых данных и создания графиков
train = function(hd, th) {
  net = neuralnet(y~x, hidden=hd, threshold=th, learn)
  y1 = predict(net, learn)
  plot(x, y, type = "l", col = "yellow", lwd = 9)
  lines(x, y1)
}

# Использование функции train
par(mfrow = c(2, 2))
train(c(15, 15), 0.01)
train(c(25, 20), 0.005)
train(c(25, 20, 25), 0.001)
train(c(20, 35, 20), 0.0005)