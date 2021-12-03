# Day 3

#Read it
input = readLines("input.txt", warn=FALSE)
#Split binary into columns
input = apply(do.call(rbind, strsplit(input, '')), 2, as.numeric)
input.df = as.data.frame(input)

gamma = input.df
epsilon = input.df

for (i in colnames(input.df)){
  
  # get for most common value
  Keeps = tail(names(sort(table(gamma[[i]]))),1)
  # get least common value
  Keeps_eps = names(which.min(table(epsilon[[i]])))

  
  if (nrow(gamma) == 1) {
    # Do nothing, we have our value
  } else {
    gamma = gamma[gamma[[i]] %in% Keeps,]
  }
  
  if (nrow(epsilon) == 1) {
    # Do nothing, we have our value
  } else {
    epsilon = epsilon[epsilon[[i]] %in% Keeps_eps,]
  }
  
  
}

# Convert dataframes to contracted strings and convert the string to integer
g = strtoi(apply(gamma, 1, paste, collapse=""), base = 2)
e = strtoi(apply(epsilon, 1, paste, collapse=""), base = 2)

# Print out the product
cat("Consumption: ", g*e)