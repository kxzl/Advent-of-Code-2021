# Day 8

#Read it
input = read.table("input.txt", header = FALSE)
known_num = c(2,4,3,7)
unique_num = 0

for(i in 1:length(input$V1)) {
  numbers = nchar(input[i,12:15])
  unique_num = unique_num + sum(numbers %in% known_num)
}
cat("Found knowns: ", unique_num)