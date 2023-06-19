library(grid)
library(scales)

m = tcrossprod(seq(1,2,length=1e2), seq(2, 3, length=1e2))
pal <- gradient_n_pal(c("red","green","yellow","blue"), values = c(2, 4, 3, 6), space = "Lab")
cols = matrix(pal(m), nrow(m))
grid.raster(cols)
# sources: https://stackoverflow.com/questions/11070101/2d-color-gradient-plot-in-r/26573256#26573256