# Day 15

# Libraries, using tidyverse and igraph
library(tidyverse)
library(igraph)


#Read it
input_width <- read.table("input.txt", colClasses = 'character', header = FALSE)
input <- read.fwf("input.txt", widths=rep(1, max(nchar(input_width$V1))), colClasses = 'numeric', header=FALSE)
remove(input_width)

# Expand 5x5 add one and wrap around 9 -> 1 with %% 
input = cbind(input, (input + 1) %% 9, (input + 2) %% 9, (input + 3) %% 9, (input + 4) %% 9)
input[input == 0] = 9
input = rbind(input, (input + 1) %% 9, (input + 2) %% 9, (input + 3) %% 9, (input + 4) %% 9)
input[input == 0] = 9

width = ncol(input)
height = nrow(input) 

inp = numeric()

for (x in 1:ncol(input)) {
  for (y in 1:nrow(input)) {
    
    inp = c(inp, input[y,x])
    
  }
  
}

input = inp
remove(inp)


cave_network <- make_lattice(dimvector = c(width,height), directed = T, mutual = T)

# assign risk levels to vertices

V(cave_network)$risk <- input

# edges inherit risk from vertices

edgelist <- get.edgelist(cave_network)
E(cave_network)$weight <- V(cave_network)[edgelist[,2]]$risk

# find shortest path

distances(cave_network, v = 1, to = (length(input)), mode = "out")


