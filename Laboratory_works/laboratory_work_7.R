library(keras)
library(tensorflow)
use_condaenv("keras-tf", required = T)

datagen <- image_data_generator(
  rescale = 1/255,
  rotation_range = 40,
  width_shift_range = 0.2,
  height_shift_range = 0.2,
  shear_range = 0.2,
  zoom_range = 0.2,
  horizontal_flip = TRUE,
  fill_mode = "nearest"
)

fnames <- list.files('photo_for_lab_7', full.names = TRUE)
img_path <- fnames[[9]]
img <- image_load(img_path, target_size = c(150, 150))
img_array <- image_to_array(img)
img_array <- array_reshape(img_array, c(1, 150, 150, 3))

augmentation_generator <- flow_images_from_data(
  img_array,
  generator = datagen,
  batch_size = 1
)

op <- par(mfrow = c(5, 2), pty = 's', mar = c(0, 0, 0, 0))
for (i in 1:10) {
  batch <- generator_next(augmentation_generator)
  plot(as.raster(batch[1,,,]))
}