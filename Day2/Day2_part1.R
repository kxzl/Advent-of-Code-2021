# Day 1
# test comment in script

input = read.table("input.txt", header = FALSE, sep = "", dec = ".")

depth = 0
horpos = 0

for (i in 1:length(input$V1)) {
  
  if (input$V1[i] == "forward") {
    
    horpos = horpos + input$V2[i]
     
  } else if (input$V1[i] == "down"){
    
    depth = depth + input$V2[i]
  } else {
    
    depth = depth - input$V2[i]
    
  }
  
}

res = depth * horpos

cat("Multiple of position is: ", res)