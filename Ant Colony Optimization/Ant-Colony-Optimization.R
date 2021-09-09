##parameter
#objective: objective function for optimization problem
#N: number of particles
#Xrange: the range for the design variable
#differs: difference between the permissible discrete value
#iter: maximum iteration
#alpha: the power in node probability, defalut is 1
#p: parameter for pheromone evaporation, defalut is 0.5
#ita:scaling parameter, defalut is 2
#pheromone: initial pheromone for each of the paths , defalut is 1 for every path
ACO <- function(objective , N , Xrange , differs, iter,
                alpha = 1 , p = 0.5 , ita = 2 , pheromone = 1){
  #setting hyperparameters and initial values
  X <- seq(Xrange[1] , Xrange[2] , differs)
  n_path <- length(X)
  if(pheromone == 1){
    pheromone <- rep(1 , n_path)
  }
  #start the iteration
  l <- 1
  while (l < iter) {
    #calculate the node probability
    node_prob <- pheromone^alpha/sum(pheromone^alpha)
    #roulette wheel selection
    #randomly select N particles by the node probability
    particles <- sample(X , N , replace = T , prob = node_prob)
    
    #calculate the objective function values
    values <- objective(particles)
    
    #find out the best and the worst path
    value_best <- min(values)
    value_worst <- max(values)
    X_best <- particles[which(values == value_best)]
    X_worst <- particles[which(values == value_worst)]
    
    #calculate the pheromone for next step
    #calculate tau_old
    tau_old <- (1-p)*pheromone
    #pheromone in the next step
    pheromone_update <- tau_old
    #calculate the pheromone deposited by the best ant
    delta_tau <- sum(ita*abs(objective(X_best)/value_worst))
    #update pheromone for best path in the next step
    for(i in 1:length(X_best)){
      pheromone_update[which(X_best[i] == X)] <- pheromone[which(X_best[i] == X)] + delta_tau
    }
    #set intial pheromone as new pheromone
    pheromone <- pheromone_update
    
    #update iteration number
    l <- l+1
    
    #test for convergency
    if(sd(particles) == 0){
      print('The ants are converged')
      print('optimum solution:')
      print(particles[1])
      print('value for optimum solution:')
      print(objective(particles[1]))
      break
    }
  }
  if(l == iter){
    print('The iteration number reaches the maximum value')
    return(X_best[1])
  }
}


example <- function(x){
  return(x^2 - 2*x - 11)
}

ACO(objective = example , N = 4 , Xrange = c(0,3) , differs = 0.5 , iter = 20)

