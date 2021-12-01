# Day 1
# test comment in script

input = read.table("C:\\Users\\romslo\\Desktop\\AoC2021\\Day1\\Input.txt", header = FALSE, sep = "", dec = ".")

prev_depth = 100000000
increase = 0

for (i in 1:length(input$V1)) {
  
  if (i == 1) {
    
    # do nothing
  
  } else if (i+2 > length(input$V1)) {
    
    break
    
  }
  
  depth = input$V1[i] + input$V1[i+1] + input$V1[i+2]   

  if (depth > prev_depth) {
    
    increase = increase + 1
    
  }
  
  
  prev_depth = depth
    
}

cat(increase, " meassurements are larger than previous")