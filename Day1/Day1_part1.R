# Day 1
# test comment in script

input = read.table("C:\\Users\\romslo\\Desktop\\AoC2021\\Day1\\Input.txt", header = FALSE, sep = "", dec = ".")

prev_depth = input$V1[1]
increase = 0

for (i in 1:length(input$V1)) {
  
  depth = input$V1[i]
  
  if (i == 1 ) {
    
    # Do nothing
    
  } else if (depth > prev_depth) {
    
    increase = increase + 1
  }
  
  prev_depth = depth
    
}

cat(increase, " meassurements are larger than previous")