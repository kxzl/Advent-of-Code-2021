# Day 9

library(raster)

#Read it
#need this to find out row width
input_width <- read.table("input.txt", colClasses = 'character', header = FALSE)
input <- read.fwf("input.txt", widths=rep(1, max(nchar(input_width$V1))), colClasses = 'numeric', header=FALSE)
remove(input_width)


#create x,y,z data.frame

df_XYZ = data.frame(x=numeric(), y=numeric(), z=numeric())

l_x = length(input$V1)
l_y = length(input)

for (x_idx in 1:l_x){
  for (y_idx in 1:l_y) {
    df_XYZ = rbind(df_XYZ, data.frame(x=x_idx,y=y_idx,z=input[x_idx,y_idx]))
  }

}

r = rasterFromXYZ(df_XYZ)
extent(r) <- extent(c(0, l_x, 0, l_y) + 0.5)

## Find the maximum value within the 9-cell neighborhood of each cell
f <- function(X) min(X, na.rm=TRUE)
ww <- matrix(1, nrow=3, ncol=3) ## Weight matrix for cells in moving window
localmin <- focal(r, fun=f, w=ww, pad=TRUE, padValue=NA)

r2 <- r==localmin

minXY <- xyFromCell(r2, Which(r2==1, cells=TRUE))

ans = sum(input[minXY] +1)

cat("Answer: ", ans)
