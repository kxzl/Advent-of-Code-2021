#Day 11
#Read it
#need this to find out row width
input_width <- read.table("input.txt", colClasses = 'character', header = FALSE)
input <- read.fwf("input.txt", widths=rep(1, max(nchar(input_width$V1))), colClasses = 'numeric', header=FALSE)
remove(input_width)

# Modulo is probably key ex 11%%10

check_neighbour <- function(inp, test, step, proc) {
  
  s = 10
  for(x in -1:1) {
    for(y in -1:1) {
      if (length(inp[test[1]+x,test[2]+y]) == 0) {next}
      if (is.null(inp[test[1]+x,test[2]+y]) ) { next }
      if (is.na(inp[test[1] + x, test[2] + y])) {next}
      if ( (x == 0 & y == 0 ) ) { next }
      if (proc[test[1] + x, test[2] + y] == TRUE) {next}
      if (inp[test[1] + x, test[2] + y] == s) {next}
      inp[test[1]+x,test[2]+y] = inp[test[1]+x,test[2]+y] + 1
    }
  }
  return(inp)
}

tot = 0
for (i in 1:2000) {
  input = input + 1
  processed = matrix(FALSE, 10,10)
  change = FALSE
  count = 0

  
  while (count < 5) {
    prev_proc = processed
    idx_flash = which((input%%10 == 0 & processed == FALSE), arr.ind = TRUE)
    if (length(idx_flash) == 0) { break }
    
    #print(idx_flash)
    for (t in 1:length(idx_flash[,1])) {
        if (processed[idx_flash[t,1], idx_flash[t,2]]) { } else {
          input = check_neighbour(input, idx_flash[t,],i, processed)
          processed[idx_flash[t,1],idx_flash[t,2]] = TRUE
        }
    }
    
    count = count + (sum(prev_proc- processed) == 0)
  }
  input = input%%10
  tot = tot + sum(input == 0)
  if (sum(input == 0) == 100) {cat("Step: ", i); break}
}

#cat("Total: ", tot)
