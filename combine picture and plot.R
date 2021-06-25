library(ggplot2)
library(jpeg)
library(grid)
img <- readJPEG("C:/Users/hannesrosenbusch/Documents/introducing conjoint/bsp.jpg")
g = rasterGrob(img, interpolate=TRUE)
g = NA
ggplot(mtcars) + geom_point(aes(x = mpg, y = cyl)) + 
  {
    if(!all(is.na(g))){annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=5, ymax=Inf)}
  }
  
