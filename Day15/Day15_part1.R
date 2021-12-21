# Day 15

# Libraries, using tidyverse and igraph
library(tidyverse)
library(igraph)


#Read it
input_width <- read.table("input.txt", colClasses = 'character', header = FALSE)
width = max(nchar(input_width$V1))
height = length(readLines("input.txt"))
remove(input_width)

input = read_lines("input.txt") %>% strsplit("") %>% unlist() %>% as.numeric()


cave_network <- make_lattice(dimvector = c(width,height), directed = T, mutual = T)

# assign risk levels to vertices

V(cave_network)$risk <- input

# edges inherit risk from vertices

edgelist <- get.edgelist(cave_network)
E(cave_network)$weight <- V(cave_network)[edgelist[,2]]$risk

# find shortest path

distances(cave_network, v = 1, to = (length(input)), mode = "out")


