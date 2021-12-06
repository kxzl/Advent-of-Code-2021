# Day 3
library(data.table)
#Read it
input = read.table("input.txt", header = FALSE, sep = ",", col.names = "fish")
input = data.table(input, input$fish=="fish")

setnames(input, c("V2"), c("manipulated"))

days = 80

for (d in 1:days) {
  
  cat("\nDay: ", d)
  
  input$manipulated <- FALSE
  
  #subtract 1 day if above 0 
  input$manipulated[input$fish > 0] = TRUE
  input$fish[input$fish > 0] <- (input$fish[input$fish > 0] - 1)

  
  # Add new fish based on how many fish reset to 6
  num_of_new_fish = 0
  num_of_new_fish = length(input$manipulated[input$fish == 0 & input$manipulated == FALSE])
  
  if (num_of_new_fish > 0) {
    for (i in 1:num_of_new_fish) {
      #input[nrow(input) + 1,] = c(8, TRUE)
      input = rbind(input, list(8,TRUE))
    }
  }
  
  # if zero from previous round reset counter to 6
  input$fish[input$fish == 0 & input$manipulated == FALSE] <- (6)
  input$manipulated[input$fish == 6] = FALSE
  

}



cat(length(input$fish), " here after ", days, " days")