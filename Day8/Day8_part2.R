# Day 8

sorting_hat <- function(my_string) {
  # Sort strings alphabetically
  # vapply(B, function(xi) paste(sort(strsplit(xi, NULL)[[1]]), collapse=''), '')
  out_string = vapply(my_string, function(xi) paste(sort(strsplit(xi, NULL)[[1]]), collapse=''), '')
  return(out_string)
}


#Read it
input = read.table("input.txt", header = FALSE)



#known numbers index numbered. Some strings are sorted so they dont need to go to sortinghat function

zero = sorting_hat("cagedb")
one = "ab"
two = sorting_hat("gcdfa")
three = sorting_hat("fbcad")
four = sorting_hat("eafb")
five = sorting_hat("cdfbe")
six = sorting_hat("cdfgeb")
seven = sorting_hat("dab")
eight = sorting_hat("acedgfb")
nine = sorting_hat("cefabd")



known_num = c(zero,one,two,three,four,five,six,seven,eight,nine)
sum_values = 0


for(i in 1:length(input$V1)) {
  numbers = input[i,12:15]
  count_n = ""
  for (n in numbers) {
    
    sorted_n = sorting_hat(n)
    
    for (q in 1:9) {
      if (known_num[q] == sorted_n) {
        count_n = paste(count_n, q-1, sep = "")
        break        
      }
    }
  }
  
  sum_values = sum_values + as.integer(count_n)
  
}

cat("Sum is: ", sum_values)
