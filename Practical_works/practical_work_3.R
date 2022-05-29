library(kohonen)

data(yeast)

x <- yeast
for (i in 1:(length(x) - 1)) {
  df <- data.frame(x[[i]])
  df[is.na.data.frame(df)] <- 0
  x[[i]] <- as.matrix(df)
}

yeast.supersom <- supersom(x, somgrid(40, 20, "hexagonal"),
                           whatmap = c("cln", "clb", "alpha", "cdc15", "cdc28", "elu"),
                           maxNA.fraction = .6)
classes <- as.integer(yeast$class)
colors <- c("black", "yellow", "green", "blue", "red", "orange")
groups <- 10
plot(yeast.supersom, type="mapping",
     bgcol=rainbow(groups), col = colors[classes],
     pch = classes, main = "yeast data")
legend(x = "bottomleft", col = colors, pch = classes,
       legend = c("cln", "clb", "alpha", "cdc15", "cdc28", "elu"))

