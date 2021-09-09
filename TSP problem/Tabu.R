##parameter
#objective: objective function for optimization problem
#n_Candidates: size of Neighbourhood set
#n_store: number of store
#max_iter: maximum iteration
#tabu_length: size of Tabu list

Tabu <- function(objective, n_Candidates, n_store,
                 tabu_length, max_iter){
  starttime <- proc.time()
  # initial solution
  initial_s <- sample(1:n_store,n_store,replace = F)
  initial_s_value <- objective(initial_s)
  # set Aspirant set
  best_solution <- initial_s_value
  best_path <- initial_s
  # set Tabu list
  Tabulist <- list()
  now_tabu_length <- 0
  # set stop criterion
  no_change_step <- 0
  
  # start iteration
  for (iter in 1:max_iter) {
    
    #find neighbourhood set
    N_set <- matrix(0,nrow = n_Candidates, ncol = n_store)
    N_set_value <- rep(0,n_Candidates)
    change <- list()
    for (n in 1:n_Candidates) {
      #sample two store to exchange
      exchange_point <- sample(1:length(initial_s),2,replace = F)
      while (list(exchange_point) %in% change){
        exchange_point <- sample(1:length(initial_s),2,replace = F)
      }
      change[[n]] <- exchange_point
      
      # exchange
      candidate_s <- initial_s
      candidate_s[exchange_point[1]] <- initial_s[exchange_point[2]]
      candidate_s[exchange_point[2]] <- initial_s[exchange_point[1]]
      N_set[n,] <- candidate_s
      # solution value
      candidate_s_value <- objective(candidate_s)
      N_set_value[n] <- candidate_s_value
    }
    
    # evaluate candidate move
    min_candidate <- min(N_set_value)
    #print(min_candidate)
    min_candidate_move <- change[[which.min(N_set_value)]]
    # check Tabu status
    if(list(min_candidate_move) %in% Tabulist){
      # check aspiration level
      if(min_candidate == best_solution){
        no_change_step <- no_change_step + 1
      }
      if(min_candidate<=best_solution){
        # set as current solution
        initial_s <- N_set[which.min(N_set_value),]
        initial_s_value <- objective(initial_s)
        # set as best solution
        best_path <- initial_s
        best_solution <- initial_s_value
      }else{
        next_choice <- 2
        while (list(min_candidate_move) %in% Tabulist) {
          min_candidate <- sort(N_set_value)[next_choice]
          min_candidate_move <- change[[which(N_set_value == min_candidate)[1]]]
          next_choice <- next_choice + 1
        }
        # set as current solution
        initial_s <- N_set[which.min(N_set_value),]
        initial_s_value <- objective(initial_s)
      }
    }else{
      # set as current solution
      initial_s <- N_set[which.min(N_set_value),]
      initial_s_value <- objective(initial_s)
      if(initial_s_value == best_solution){
        no_change_step <- no_change_step + 1
      }
      if(initial_s_value <= best_solution){
        best_path <- initial_s
        best_solution <- initial_s_value
      }
    }
    
    # update Tabu list
    if(now_tabu_length < tabu_length){
      now_tabu_length <- now_tabu_length + 1
      Tabulist[[now_tabu_length]] <- min_candidate_move
    }
    
    
    #check stop criterion
    if(no_change_step > 20){
      print('The algorithm is converged')
      print('optimum solution:')
      print(best_path)
      print('value for optimum solution:')
      print(best_solution)
      break
    }
    
  }
  
  print('The iteration number reaches the maximum value')
  print('optimum solution:')
  print(best_path)
  print('value for optimum solution:')
  print(best_solution)
  
  exe_time <- proc.time() - starttime
  print(exe_time)
  return(best_path)
  
}
