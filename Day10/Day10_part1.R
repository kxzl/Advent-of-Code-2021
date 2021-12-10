# Day 8

library(stringr)
#Read it
input = read.table("input.txt", header = FALSE)


brak_remov <- function(my_string) {
  same_len = TRUE 
  while (same_len) {
    start = my_string
    my_string = str_remove_all(my_string, "\\(\\)")
    my_string = str_remove_all(my_string, "\\[\\]")
    my_string = str_remove_all(my_string, "\\{\\}")
    my_string = str_remove_all(my_string, "\\<\\>")

    same_len = !(str_length(my_string) == str_length(start))
  }
  
 return(my_string)
}


brak_id <- function(my_string) {
  
  my_string = str_remove_all(my_string, "\\(")
  my_string = str_remove_all(my_string, "\\[")
  my_string = str_remove_all(my_string, "\\{")
  my_string = str_remove_all(my_string, "\\<")
  
  return(substr(my_string,1,1))
  
}


brak_points <- function(my_string) {
  
  points = NA

  if (my_string == ")") { points = 3 }
  else if (my_string == "]") { points = 57 }
  else if (my_string == "}") { points = 1197 }
  else if (my_string == ">") { points = 25137 }
  else if (my_string == "") { points = 0 }

  return(points)
}


#brak_points(brak_id(brak_remov(input$V1)))

score = 0
for (s in input$V1) {
  
  # is string valid?
  score = score + brak_points(brak_id(brak_remov(s)))
  
}

cat("Score is: ", score)

