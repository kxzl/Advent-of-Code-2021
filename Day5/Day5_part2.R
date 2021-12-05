# Day 5
# Got to read the stuff, using stolen function https://stackoverflow.com/questions/18186357/importing-csv-file-with-multiple-character-separator-to-r

library(plyr)

#fileName <- file name with fully qualified path
#separators <- each of them separated by '|'

read <- function(fileName, separators) {
  data <- readLines(con <- file(fileName), warn = FALSE)
  close(con)
  records <- sapply(data, strsplit, split=separators)
  dataFrame <- data.frame(t(sapply(records,c)))
  rownames(dataFrame) <- 1: nrow(dataFrame)
  return(as.data.frame(dataFrame,stringsAsFactors = FALSE))
}


# read and fix column names
# remeber that the -> should also include whotespaces for proper formatting " -> "
input = read("input.txt", ",| -> ")
names(input)[names(input) == "X2"] <- "Y1"
names(input)[names(input) == "X3"] <- "X2"
names(input)[names(input) == "X4"] <- "Y2"

filter_input = data.frame(X1=integer(),Y1=integer(),X2=integer(),Y2=integer())
world_matrix = matrix(0,999,999)

for (i in 1:length(input$X1)) {
  
  angle = atan2(as.integer(input$Y2[i]) - as.integer(input$Y1[i]), as.integer(input$X2[i]) - as.integer(input$X1[i])) * 180 / pi
  #  || abs(angle) == 45
  if (input$X1[i] == input$X2[i] || input$Y1[i] == input$Y2[i]) {

    x_start = as.integer(input$X1[i])+1
    x_stop = as.integer(input$X2[i])+1
    
    y_start = as.integer(input$Y1[i])+1
    y_stop = as.integer(input$Y2[i])+1
    
    for (x in x_start:x_stop){
      for (y in y_start:y_stop){
        world_matrix[x,y] = world_matrix[x,y] + 1
      }
      
    }
    
  } else if (abs(angle) == 45 || abs(angle) == 135){
    x_start = as.integer(input$X1[i])+1
    x_stop = as.integer(input$X2[i])+1
    
    y_start = as.integer(input$Y1[i])+1
    y_stop = as.integer(input$Y2[i])+1
    
    m = (as.integer(input$Y2[i]) - as.integer(input$Y1[i]))/(as.integer(input$X2[i]) - as.integer(input$X1[i]))
    b = y_start-(x_start*m)
    for (x in x_start:x_stop){
      
      y = m*x+b
      world_matrix[x,y] = world_matrix[x,y] + 1
      
    }
    
  }
}

print(sum(world_matrix > 1))
