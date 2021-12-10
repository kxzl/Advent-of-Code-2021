# Day 8

library(stringr)
library(stringi)
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
  
  points = 0
  p_tot = 0
  my_string <- strsplit(my_string, "")[[1]]
  
  for (c in my_string) {
    if (c == ")") { points = 1 }
    else if (c == "]") { points = 2 }
    else if (c == "}") { points = 3 }
    else if (c == ">") { points = 4 }
    p_tot = 5*p_tot + points
  }

  return(p_tot)
}

brak_bros <- function(my_string) {
  
  my_string = stri_reverse(my_string)
  
  my_string = str_replace_all(my_string, "\\(", "\\)")
  my_string = str_replace_all(my_string, "\\[", "\\]")
  my_string = str_replace_all(my_string, "\\{", "\\}")
  my_string = str_replace_all(my_string, "\\<", "\\>")
  
  return(my_string)
  
}

#brak_points(brak_id(brak_remov(input$V1)))

score = numeric()
for (s in input$V1) {
  
  # is string valid?
  if (brak_id(brak_remov(s)) == "") {
    
    score = c(score, brak_points(brak_bros(brak_remov(s))))
    
  }
  
}

cat("Score is: ", median(score))

