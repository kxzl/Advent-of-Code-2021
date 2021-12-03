# Day 3

#Read it
input = readLines("input.txt", warn=FALSE)
#Split binary into columns
input = apply(do.call(rbind, strsplit(input, '')), 2, as.numeric)
input.df = as.data.frame(input)

gamma = ""

for (i in colnames(input.df)){
  
  # Find most common binary occurance in column
  gamma = paste(gamma,tail(names(sort(table(input.df[[i]]))),1), sep ="" )
  
}

# Invert string
epsilon = chartr("01","10",gamma)

g = strtoi(gamma, base = 2)
e = strtoi(epsilon, base = 2)

cat("Consumption: ", g*e)