library(RBM)
library(EBImage)
library(pbapply)

ResultRBM <- function(test, model, layers = 1) {
  if (!missing(layers)) {
    if ( layers == 1) {
      print('Layers is 1, will treat the model as a regular RBM. 
            If the model is a stacked RBM only the first layer weights will be used')
    }
  }
  if (!is.null(dim(test))) {
    stop('It is only possible te reconstruct one training image at a time')
  }
  if (any(!is.numeric(test))) {
    stop('Sorry the data has non-numeric values, the function is executed')
  }
  if (any(!is.finite(test))) {
    stop('Sorry this function cannot handle NAs or non-finite data')
  }
  if (length(model)  != layers) {
    stop("Number of layers is unequal to the number of weight matrices in the model")
  } 
  test <- matrix(test, nrow = 1)
  # Add bias term to visible layer
  V <- cbind(1, test[1,, drop = FALSE])
  # Reconstruct the image
  if (missing(layers)) {
    # Calculate hidden
    H <- VisToHid(V, model$trained.weights)
    # Reconstruct the visible layer
    V.rec <- HidToVis(H, model$trained.weights)
  } 
  else {
    # First do one forward pass
    for (i in 1:layers) {
      V <- VisToHid(V, model[[i]]$trained.weights)
    }
    # Set last sampled layer to start of backward pass
    H <- V
    # Perform backward pass to get reconstruction
    for (i in layers:1) {
      H <- HidToVis(H, model[[i]]$trained.weights)
    }
    # Set last sampled layer to reconstruction
    V.rec <- H
    # Set V back to input 
    V <- cbind(1, test[1,, drop = FALSE])
  }
  
  return(matrix(V.rec[, -1], nrow = sqrt(ncol(test))))
}

VisToHid <- function(vis, weights, y, y.weights) {
  # Function for calculating a hidden layer.
  #
  # Args:
  #   vis: Visual layer, or hidden layer from previous layer in DBN
  #   weights: Trained weights including the bias terms (use RBM)
  #   y: Label vector if only when training an RBM for classification
  #   y.weights: Label weights and bias matrix, only neccessary when training a RBM for classification
  #
  # Returns:
  #   Returns a hidden layer calculated with the trained RBM weights and bias terms.
  #
  # Initialize the visual, or i-1 layer
  V0 <- vis
  if ( is.null(dim(V0))) {
    # If visual is a vector create matrix
    V0 <- matrix(V0, nrow= length(V0))
  }
  if(missing(y) & missing(y.weights)) {
    # Calculate the hidden layer with the trained weights and bias
    H <- 1/(1 + exp(-( V0 %*% weights))) 
  } else {
    Y0 <- y
    H <- 1/(1 + exp(- ( V0 %*% weights + Y0 %*% y.weights))) 
  }
  return(H)
}

HidToVis <- function(inv, weights, y.weights) {
  # Function for reconstructing a visible layer.
  #
  # Args:
  #   inv: Invisible layer
  #   vis.bias: Trained visible layer bias (use RBM)
  #   weights: Trained weights (use RBM)
  #   y.weights: Label weights, only nessecessary when training a classification RBM.
  #
  # Returns:
  #   Returns a vector with reconstructed visible layer or reconstructed labels.
  #
  if(missing(y.weights)) {
    # Reconstruct only the visible layer when y.weights is missing
    V <- 1/(1 + exp(-(   inv %*% t(weights)) ))
    return(V)
  } else {
    # Reconstruct visible and labels if y.weights
    Y <- 1/(1 + exp(-( inv %*% t(y.weights)))) 
    return(Y)
  }
}

image_conversion <- function(dir_path, width, height){
  # Формирование списка файлов с изображениями из директория
  images_names <- list.files(dir_path)
  # Визуализация процесса обработки изображений
  print(paste("Обработка: ", length(images_names), "изображений"))
  # Функция обработки изображения
  image_processing <- pblapply(images_names, function(imgname){
    # Чтение изображения
    img <- readImage(file.path(dir_path, imgname))
    # Изменение размеров
    img_resized <- resize(img, w=width, h=height)
    
    # Преобразование в черно-белое
    img_gray <- channel(img_resized, "gray")
    # par(mfrow=c(2, 10))
    
    # Получение матрицы размера weight x height
    img_matrix <- img_gray@.Data
    img_matrix <- rotate(img_matrix, 90)
    # Запись как вектор
    img_vector <- as.vector(t(img_matrix))
    return(img_vector)
  })
  # Формирование матриц всех изображений из директории
img_matrix <- do.call(rbind, image_processing)
return(img_matrix)
}

# Подготовка обучающей выборки
trainData <- image_conversion("fot", 100, 100)
# Обучение ограниченной машины Больцмана
set.seed(55)
# Подготовка для печати 2-х строк по 10 изображений
mod <- RBM(trainData, n.iter=400, n.hidden=196)
par(mar=c(1,1,1,1))
par(mfrow=c(2, 10))
# Реконструкция 10 реконструрированных изображений (hidden = 196)
for (i in 1:10)
  image(ResultRBM(trainData[i, ], model=mod))
# Обучение стека ограниченных машин Больцмана
set.seed(55)
modCat <- StackRBM(x = trainData, n.iter = 400, 
                   layers = c(392, 196, 98), learning.rate = 0.1, 
                   size.minibatch = 10, lambda = 0.1, momentum = 0.5)
# Реконструкция 10 реконструрированных изображений (hidden = 98)
for (i in 1:10)
  image(ResultRBM(trainData[i, ], model=modCat, layers = 3))
