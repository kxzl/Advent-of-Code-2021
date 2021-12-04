# Day 4



# Got to read this board and number stuff

numbers = as.matrix(read.table("input.txt", nrows = 1, sep = ",", as.is = TRUE))
boards_raw = read.table("input.txt", skip = 2)

# do some splitting

chunk <- 5
n <- nrow(boards_raw)
r  <- rep(1:ceiling(n/chunk),each=chunk)[1:n]
boards <- split(boards_raw,r)



got_row = FALSE
got_column = FALSE
win_board_num = 0

# stop all the looping
stop = FALSE

# llop through available numbers
for (i in 1:length(numbers)) {
  #loop through boards
  for (z in 1:length(boards)) {
    # if number exist on board set it as NaN
    boards[[z]][boards[[z]] == numbers[i]] <- NA

    # Check if we have got a winner with 5 NA in row or column 
    for (q in 1:5) {
      
      got_row = all(is.na(boards[[z]][q,]))
      got_column = all(is.na(boards[[z]][,q]))
      
      if (got_column == TRUE || got_row == TRUE) {
        # if we get in here, we found the winner!
        stop = TRUE
        win_board_num = z
        break
      }
      
    }
    if (stop){break}
  }
  
  if (stop){break}
  
}


# Priint me the answer
cat("winning sum*winning_number: ",sum(boards[[win_board_num]], na.rm = TRUE)*numbers[i])