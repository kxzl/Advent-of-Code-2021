# Day 5
# Got to read the stuff, using stolen function https://stackoverflow.com/questions/18186357/importing-csv-file-with-multiple-character-separator-to-r


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

write_out_vector <- function(vector) {
  koord_list = NULL
  x_start = vector$X1[1]
  x_stop = vector$X2[1]
  y_start = vector$Y1[1]
  y_stop = vector$Y2[1]
  for (x in x_start:x_stop){
    for (y in y_start:y_stop){
      koord_list = rbind(koord_list, c(x,y))
    }
    
  }
  
  return(koord_list)
  
}


# read and fix column names
# remmeber that the -> should also include whotespaces for proper formatting " -> "
input = read("input.txt", ",| -> ")
names(input)[names(input) == "X2"] <- "Y1"
names(input)[names(input) == "X3"] <- "X2"
names(input)[names(input) == "X4"] <- "Y2"

filter_input = data.frame(X1=character(),Y1=character(),X2=character(),Y2=character())

for (i in 1:length(input$X1)) {
  
  if (input$X1[i] == input$X2[i] || input$Y1[i] == input$Y2[i]) {
    # Add values to new filter dataframe
    filter_input <- rbind(filter_input, input[i,])
  } 
  
}

#Comment this out if you want to process filtered results
#filter_input = input

writen_vectors = NULL

for ( i in 1:length(filter_input$X1)){
  
  writen_vectors <- rbind(writen_vectors, write_out_vector(filter_input[i,]))
  
}

writen_vectors = as.data.frame(writen_vectors)

library(plyr)
vec_duplicates = ddply(writen_vectors,.(V1,V2),nrow)


valid_overlaps = colSums(vec_duplicates["V1"] > 1)

cat("The answer is: ", valid_overlaps)