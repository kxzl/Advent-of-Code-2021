# Day 3
library(data.table)
options(scipen=999)
#Read it
input_raw = read.table("input.txt", header = FALSE, sep = ",", col.names = "fish")
fish = as.vector(matrix(0,9,1))

for (i in input_raw$fish) {
  
  if(i == 0) {fish[1] = fish[1] +1}
  if(i == 1) {fish[2] = fish[2] +1}
  if(i == 2) {fish[3] = fish[3] +1}
  if(i == 3) {fish[4] = fish[4] +1}
  if(i == 4) {fish[5] = fish[5] +1}
  if(i == 5) {fish[6] = fish[6] +1}
  if(i == 6) {fish[7] = fish[7] +1}
  if(i == 7) {fish[8] = fish[8] +1}
  if(i == 8) {fish[9] = fish[9] +1}

}

days = 256
while (days != 0) {
  
  # save popable fish
  ready_to_pop = fish[0+1]
  
  # Age by one day
  fish = shift(fish, -1)  
  
  #New Fish
  fish[8+1] = ready_to_pop
  fish[6+1] = ready_to_pop + fish[6+1]
  
  days = days - 1
}

cat("number of fish ", sum(fish)," after ", days, " days")