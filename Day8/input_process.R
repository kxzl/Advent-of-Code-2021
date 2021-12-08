sorting_hat <- function(my_string) {
  # Sort strings alphabetically
  # vapply(B, function(xi) paste(sort(strsplit(xi, NULL)[[1]]), collapse=''), '')
  out_string = vapply(my_string, function(xi) paste(sort(strsplit(xi, NULL)[[1]]), collapse=''), '')
  return(out_string)
}


#Read it
input = read.table("input.txt", header = FALSE)

# known numbers first 1,4,7 and 8 

num_seq = input[1,1:10]

zero = ""
one = ""
two = ""
three = ""
four = ""
five = ""
six = ""
seven = ""
eight = ""
nine = ""

# populate known strings first
# 1 -> 2
# 4 -> 4
# 7 -> 3
# 8 -> 7
for (z in num_seq) {
  
  if (nchar(z) == 2) { one = z}
  else if (nchar(z) == 4) { four = z}
  else if (nchar(z) == 3) { seven = z}
  else if (nchar(z) == 7) { eight = z}
  
  ""
}

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