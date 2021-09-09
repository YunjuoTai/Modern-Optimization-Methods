#Roulette-Wheel Selection

##define function for computing Roulette-Wheel Selection
##parameters: 
#n_MatingPool:number of strings in mating pool
#fitness: fitness for each string

Wheel <- function(n_MatingPool , fitness){
  
  ##calculate the cumulative probability by given fitness
  #calculate the frequency for each string
  total <- sum(fitness)
  select_probability <- fitness/total
  #cumulate probability
  cdf <- c(select_probability[1])
  for(i in 2:length(fitness)){
    cdf[i] <- cdf[i-1] + select_probability[i]
  }
  
  ##sample which string to copy
  #generate n_MatingPool random numbers
  set.seed(1) #for reproducing
  random_number <- runif(n_MatingPool)
  #determine the string by the range of cumulative probability
  string_to_copy <- c()
  copy_cnt <- rep(0,length(fitness))
  
  for (r in random_number ) {
    if(r <= cdf[1]){
      append(string_to_copy , 1)
      copy_cnt[1] <- copy_cnt[1] + 1
    }else if(r > cdf[1] & r <= cdf[2]){
      append(string_to_copy , 2)
      copy_cnt[2] <- copy_cnt[2] + 1
    }else if(r > cdf[2] & r <= cdf[3]){
      append(string_to_copy , 3)
      copy_cnt[3] <- copy_cnt[3] + 1
    }else if(r > cdf[3] & r <= cdf[4]){
      append(string_to_copy , 4)
      copy_cnt[4] <- copy_cnt[4] + 1
    }else if(r > cdf[4] & r <= cdf[5]){
      append(string_to_copy , 5)
      copy_cnt[5] <- copy_cnt[5] + 1
    }else if(r > cdf[5] & r <= cdf[6]){
      append(string_to_copy , 6)
      copy_cnt[6] <- copy_cnt[6] + 1
    }else if(r > cdf[6] & r <= cdf[7]){
      append(string_to_copy , 7)
      copy_cnt[7] <- copy_cnt[7] + 1
    }
  }
  
  return(list(string_to_copy , copy_cnt ))
  
}

##simulation for 10 times
all_copy_cnt <- matrix(nrow = 10 , ncol = 7)
for (iter in c(1:10)){
  all_copy_cnt[iter,] <- Wheel(n_MatingPool = 12 , fitness = c(8,12,6,2,18,9,10))[[2]]
}

avarage_copy <- round(colSums(all_copy_cnt)/10)


