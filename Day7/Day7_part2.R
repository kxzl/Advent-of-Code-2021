# Day 7


#Read it
input = read.table("input.txt", header = FALSE, sep = ",", col.names = "crabs")

# approximately correct optimal position
opt_pos = round(mean(input$crabs))


# Instead of ploting functions and derive, just test +-100 space of optimal position for what consumes less fuel
fuel_prev = Inf
pos = 0
for (i in (opt_pos-100):(opt_pos+100)) {
  n = (abs(input$crabs - i))
  fuel = sum((n*(n+1))/2)
  
  if(fuel < fuel_prev) {
    
    fuel_prev = fuel
    pos = i
    
  }
  
}

cat("Optimal fuel: ", fuel_prev)
cat("Optimal pos: ", pos)

