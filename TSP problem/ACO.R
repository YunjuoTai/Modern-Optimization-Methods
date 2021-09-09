##parameter
#objective: objective function for optimization problem
#N: number of particles
#n_store: number of store
#iter: maximum iteration
#alpha: the power for pheromone in node probability, defalut is 1
#beta: the power for distance value in node probability, defalut is 1
#p: parameter for pheromone evaporation, defalut is 0.5
ACO <- function(objective , N , n_store , iter,
                alpha = 1, beta = 1, p = 0.5){
  starttime <- proc.time()
  #setting hyperparameters and initial values
  # initial pheromone
  pheromone <- matrix(1, ncol = n_store, nrow = n_store )
  distan <- 1/distance
  
  #start the iteration
  l <- 1
  while (l < iter) {
    # initial start position
    ant_initial_position <- sample(1:n_store, N , replace = F)
    
    # Roulette-Wheel Selection to complete the path
    path <- matrix(0,nrow = n_store+1, ncol = N)
    path[1,] <- ant_initial_position
    path[n_store+1,] <- ant_initial_position
    for (i in 1:N) {
      possible_path <- c(1:n_store)
      ant <- ant_initial_position[i]
      for (j in 2:n_store) {
        possible_path <- possible_path[-which(possible_path == ant)]
        if(length(possible_path) <= 1){
          ant_next <- possible_path
        }else{
          value <- pheromone[ant,possible_path]^alpha * distan[ant,possible_path]^beta
          node_prob <- value/sum(value)
          ant_next <- sample(possible_path,1,replace = F,prob = node_prob)
        }
        path[j,i] <- ant_next
        ant <- ant_next
      }
    }
      
      # update local phromone
      fit_value <- apply(path, 2, objective)
      for (k in 1:N) {
        for (j in 1:n_store) {
          pheromone[path[j,k],path[j+1,k]] <- 1/fit_value[k] + pheromone[path[j,k],path[j+1,k]]
        }
      }
      
      # update global phromone
      pheromone <- (1-p) * pheromone
      
      l <- l+1
      
      # stopping criterion
      fit_value <- apply(path, 2, objective)
      if(sd(fit_value) == 0){
        print('The ants are converged')
        print('optimum solution:')
        print(path[1])
        print('value for optimum solution:')
        print(fit_value[1])
        break
      }
      
  }
  fit_value <- apply(path, 2, objective)
  best_path <- path[,which.min(fit_value)]
  print('The iteration number reaches the maximum value')
  print('optimum solution:')
  print(best_path)
  print('value for optimum solution:')
  print(fit_value[which.min(fit_value)])
  
  exe_time <- proc.time() - starttime
  print(exe_time)
  
  return(best_path)
}
