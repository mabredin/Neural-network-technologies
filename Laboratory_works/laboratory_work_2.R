library(nnet)
library(fpp2)

# Загрузка данных для анализа
scripts <- elecequip
n <- length(scripts) # n - 195 месяца (16 лет)

# Подготовка матрицы наблюдений заполненной нулями
LearnSeq <- matrix(rep(0, (n-12)*13), nrow = n-12, ncol = 13)

# Заполнение матрицы данными для обучения
for (i in 1:(n-12)) LearnSeq[i, ] <- scripts[i:(12 + i)]

# Обучение сети Nnet
set.seed(55)
Nnet <- nnet(LearnSeq[,1:12], LearnSeq[,13], size = 60, linout = TRUE, rang=0.4, maxit = 300)

# Прогноз составленный обученной сетью Nnet
prognos <- c(rep(0,12), Nnet$fitted.values)

# График фактического числа рецептов (blue) и предсказанный (red)
ggplot(scripts, aes(x)) +
  geom_line(aes(y=scripts), color="blue", size = 2) +
  geom_line(aes(y=prognos), color="red", size = 0.5)