# Day 7


#Read it
input = read.table("input.txt", header = FALSE, sep = ",", col.names = "crabs")

med_pos = median(input$crabs)

cat("Optimal fuel: ", sum(abs(input$crabs -med_pos)))

