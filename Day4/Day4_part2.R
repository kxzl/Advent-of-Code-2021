# Day 4



# Got to read this board and number stuff

numbers = as.matrix(read.table("input.txt", nrows = 1, sep = ",", as.is = TRUE))
boards_raw = read.table("input.txt", skip = 2)

# do some splitting

chunk <- 5
n <- nrow(boards_raw)
r  <- rep(1:ceiling(n/chunk),each=chunk)[1:n]
boards <- split(boards_raw,r)




win_boards = 1:length(boards)

# stop all the looping
stop = FALSE

# loop through available numbers
for (i in 1:length(numbers)) {
  #loop through boards
  for (z in 1:length(boards)) {
    # if number exist on board set it as NaN
    boards[[z]][boards[[z]] == numbers[i]] <- NA

    # Check if we have got a winner with 5 NA in row or column 
    for (q in 1:5) {
      
      got_row = FALSE
      got_column = FALSE
      in_boards = FALSE
      
      got_row = all(is.na(boards[[z]][q,]))
      got_column = all(is.na(boards[[z]][,q]))
      in_boards = (z %in% win_boards)
      
      if ((got_column == TRUE | got_row == TRUE) & in_boards) {
        # if we get in here, we found the winner!
      
        
        if (length(which(!is.na(win_boards))) == 1) { 
          stop = TRUE
          break
        }
        
        # remove win_board from win_board list
        win_boards[win_boards == z] <- NA
      }
      
    }
    if (stop){break}
  }
  
  if (stop){break}
  
}


# Print me the answer
cat("win number: ", numbers[i])
cat("winning sum*winning_number: ",sum(boards[[sum(win_boards, na.rm = TRUE)]], na.rm = TRUE)*numbers[i])
