library(nnet)
library(NeuralNetTools)
numbers <- c(1:15)
# Перевод в двоичную систему
y <- sapply(numbers, function(numbers){ as.integer(intToBits(numbers))})[1:4,]
# Исходная матрица (без сдвига)
original <- rbind(y[4,], y[3,], y[2,], y[1,])
# Матрица со сдвигом влево без исходного
L_shift <- rbind(original[2,], original[3,], original[4,], original[1,])
# Матрица со сдвигом вправо без исходного
R_shift <- rbind(original[4,], original[1,], original[2,], original[3,])
# Соединение исходной матрицы и матрицы со сдвигом влево
OrigL <- rbind(original, L_shift)
# Соединение исходной матрицы и матрицы со сдвигом вправо
OrigR <- rbind(original, R_shift)
# Создание общей матрицы со сдвигом и влево и вправо
general <- t(cbind(OrigL, OrigR))
# Оставляем только уникальные строки
Un <- unique(general)
# Переменная с конечными результатами
# где 0 - сдвиг влево, 1 - сдвиг вправо, 2 - сдвиг в неизвестном направлении
answer <- c(0,0,0,0,2,0,0,0,0,2,0,0,0,0,2,1,1,1,1,1,1,1,1,1,1,1,1)
# Обучание нейросети
Nnet<-nnet(Un, answer, size=4, linout=TRUE)
# Визуализация нейросети
plotnet(Nnet)
# Проверка прогнозирования нейросетью уникальных данных
pred <- round(predict(Nnet, Un))
# Визуализация прогноза
plot(pred, pch = 21, bg = "red")
# Замена цифр на слова
pred = ifelse(pred[,1] == 0, "влево", ifelse(pred[,1] == 1, "вправо", "оба"))
# Соединение общей матрицы с результатом 
Fpred <-cbind(pred, Un)
# Проверка прогнозирования нейросетью всех данных
pred <- round(predict(Nnet, general))
# Визуализация прогноза
plot(pred, pch = 21, bg = "blue")
