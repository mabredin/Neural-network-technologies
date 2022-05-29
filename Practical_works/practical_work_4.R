# /Универ/6 семестр/Neural-network-technologies/Practical_works
library(RSNNS)
library(neuralnet)
#забор данных из файла
data.set1 = read.csv("C:/Users/User/Desktop/SET1.csv",
                     as.is = 1,
                     header = FALSE,
                     sep = ";",
                     quote = "\"",
                     dec = ".",
                     fill = TRUE,
                     comment.char = "")

#функция отображение чисел
print.numbers <- function(x) {
  number <- matrixToActMapList(x, nrow = 7)
  par(mar=c(1,1,1,1))
  par(mfrow=c(4,7))
  n <- c(1:10)
  for (i in n) plotActMap(number[[i]])
}

#формирование последовательности цифр
inputs <- data.set1[1:35]
print.numbers(inputs)

#формирование данных для обучения
inputs1 <- rbind(inputs[2,1:35],inputs[4,1:35],inputs[6,1:35],inputs[8,1:35],inputs[10,1:35])
train.data = data.frame(inputs1,inputs1)

#формирование формул для вывода и ввода
x.names <- paste0("V", 1:35, collapse = " + ")
y.names <- paste0("V", 1:35, collapse = " + ")
form <- as.formula(paste(x.names, " ~ ", y.names))

#обучение нейросети
set.seed(55)
Nnet <- neuralnet(form, train.data, hidden = 4, threshold = 0.0006)

#тестирование распознавания данных
test.data = data.frame(inputs)
y.predict=predict(Nnet, test.data)
print.numbers((y.predict))