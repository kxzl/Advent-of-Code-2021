# Day 1
# test comment in script

input = read.table("input.txt", header = FALSE, sep = "", dec = ".")

depth = 0
horpos = 0
aim = 0

for (i in 1:length(input$V1)) {
  
  if (input$V1[i] == "forward") {
    
    horpos = horpos + input$V2[i]
    depth = depth + (aim*input$V2[i])
     
  } else if (input$V1[i] == "down"){
    aim = aim + input$V2[i]
    
  } else {
    
    aim = aim - input$V2[i]
    
  }
  
}

res = depth * horpos

cat("Multiple of position is: ", res)