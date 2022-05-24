library(RSNNS)
library(neuralnet)

print.alphabet <- function(x) {
  letter <- matrixToActMapList(x, nrow = 7)
  par(mar=c(1,1,1,1))
  par(mfrow=c(4,7))
  n <- c(1:26)
  for (i in n) plotActMap(letter[[i]])
}

# Подготовка обучающей выборки
data(snnsData)
inputs <- snnsData$artmap_test.pat
print.alphabet(inputs)
train.data=data.frame(inputs, inputs)

# Формирование формул для входов и выходов
x.names <- paste0("in", 1:35, collapse = " + ")
y.names <- paste0("in", 1:35, collapse = " + ")
form <- as.formula(paste(x.names, ' ~ ', y.names))

# Обучение автокодировщика
set.seed(55)
net <- neuralnet(formula = form, train.data, hidden = 10, threshold = 0.0006)

# Тестирование обученного автокодировщика
test.data=data.frame(inputs)
y.predict=predict(net, test.data)
print.alphabet(y.predict)



