# Day 9

library(raster)
library(rgeos)

basin_count <- function(coors) {
  
  basin_coors = rbind(coors)
  cont = TRUE
  
  while (cont){
    prev = length(basin_coors)
    
    for (t in 1:nrow(basin_coors)) {
      test = basin_coors[t,]
      for(x in -1:1) {
        for(y in -1:1) {
          
          if (length(input[test[1]+x,test[2]+y]) == 0) {next}
          if (is.null(input[test[1]+x,test[2]+y]) ) {next}
          if ( (abs(x) == abs(y) ) ) { next }
          if (is.na(input[test[1] + x, test[2] + y])) {next}
          if (input[test[1]+x,test[2]+y] == 9) {next}
          #print(input[test[1]+x,test[2]+y])
          basin_coors = rbind(basin_coors, c(test[1]+x,test[2]+y))
          
        }
        
      }
    }
    
    basin_coors = unique(basin_coors)
    cont = !(prev == length(basin_coors))
    
  }
  
  return(prev/2)
  
}



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

basins = numeric()

for (c in 1:nrow(minXY)) {
  basins = c(basins, basin_count(minXY[c,]))
}

cat ("Answer: ", prod(sort(basins, decreasing = TRUE)[1:3]))
