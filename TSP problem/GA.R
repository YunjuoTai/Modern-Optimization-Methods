#Programming of Genetic Algorithm

'''

parameters:
problem_function: a function, include objective function and all contraints of the optimization problem
iter_max: a positive number, the maximum number of generation
s_max: a positive number, the value of standard deviation use as a convergence criterion
N_population: a positive number, size of the population
N_gene: a positive number, number of design variables
p_c: probability of crossover
p_m: probability of mutation
seed_num: set seed number
'''
GA <- function(problem_function, 
               iter_max, s_max = NULL,  N_population , N_gene,
               p_c, p_m, seed_num = NULL){
  starttime <- proc.time()
  ##STEP 1
  #generate a random population of size N_population
  if(!is.null(seed_num)){
    set.seed(seed_num)
  }
  population <- matrix(rep(0,N_population*N_gene), N_population , N_gene)
  for (n in 1:N_population) {
    random_route <- sample(1:N_gene , size =N_gene, replace = F )
    population[n,] <- random_route
  }
  
  #if violate constraints than sample population again
  fit_value <- apply(population , 1 , problem_function)
  # print('--initial fit value--')
  # print(fit_value)
  while(sum(is.na(fit_value)) > 0 ){
    if(!is.null(seed_num)){
      seed_num <- seed_num+1
      set.seed(seed_num)
    }
    population <- matrix(rep(0,N_population*N_gene), N_population , N_gene)
    for (n in 1:N_population) {
      random_route <- sample(1:N_gene , size =N_gene, replace = F )
      population[n,] <- random_route
    }
    fit_value <- apply(population , 1 , problem_function)
  }
  #print('--initial fit_value--')
  #print(fit_value)
  #calculate the standard deviation of fitness value of the first generation as defalut value for s_max
  if(is.null(s_max)){
    s_max <- sd(fit_value)
  }
  
  for(G in 1:iter_max){
    #print('--step--')
    #print(G)
    
    next_population <- matrix(0 , N_population , N_gene)
    
    ##STEP 2
    #reproduction: use Roulette-Wheel Selection generate next generation
    #calculate the fitness value
    fit_value <- apply(population , 1 , problem_function)
    #print(fit_value)
    #calculate selection probability (frequency of each individual)
    total <- sum(fit_value)
    selection_prob <- fit_value/total
    
    #produce the next generation
    idx <- 1:N_population
    N_next_population <- 0
    while(N_next_population < N_population) {
      #select parents
      if(!is.null(seed_num)){
        set.seed(seed_num)
      }
      parents <- sample( idx, 2, replace = F, prob = selection_prob)
      parents_1 <- population[parents[1],]
      parents_2 <- population[parents[2],]
      #print('--selected parents--')
      #print(parents_1)
      #print(parents_2)
      #next generation
      parents_1_new <- parents_1
      parents_2_new <- parents_2
      
      #print('--initial parent new--')
      #print(parents_1_new)
      #print(parents_2_new)
      
      ##STEP 3
      #crossover
      if(runif(1) >= p_c){
        
          #randomly select crossover site
          if(!is.null(seed_num)){
            set.seed(seed_num)
          }
          parents_1_new <- rep(0,N_gene)
          parents_2_new <- rep(0,N_gene)
          
          switch_point <- sample(c(2:(N_gene-1)) ,1)
          #switch gene
          #note:need to avoid location repeat
          parents_1_new[1:switch_point] <- parents_2[1:switch_point]
          #print(parents_1_new)
          parents_2_new[1:switch_point] <- parents_1[1:switch_point]
          #print(parents_2_new)
          
          parents_1_new[(switch_point+1):N_gene] <- parents_1[!(parents_1 %in% parents_1_new)]
          parents_2_new[(switch_point+1):N_gene] <- parents_2[!(parents_2 %in% parents_2_new)]
      }
      #print('--cross over--')
      #print(parents_1_new)
      #print(parents_2_new)
      
      ##STEP 4
      #mutation
      #mutation for parent1
      if(!is.null(seed_num)){
          set.seed(seed_num)
        }
      if(runif(1) >= p_m){
          #randomly select two site to exchange
          if(!is.null(seed_num)){
            set.seed(seed_num)
          }
          mutation_site <- sample(N_gene , 2, replace = F)
          m1 <- parents_1_new[mutation_site[1]]
          m2 <- parents_1_new[mutation_site[2]]
          parents_1_new[mutation_site[1]] <- m2
          parents_1_new[mutation_site[2]] <- m1
        }
        
      #mutation for parent2
      if(!is.null(seed_num)){
          set.seed(seed_num+1)
        }
      if(runif(1) >= p_m){
          #randomly select mutation site
          if(!is.null(seed_num)){
            set.seed(seed_num+1)
          }
          mutation_site <- sample(N_gene , 2, replace = F)
          m1 <- parents_2_new[mutation_site[1]]
          m2 <- parents_2_new[mutation_site[2]]
          parents_2_new[mutation_site[1]] <- m2
          parents_2_new[mutation_site[2]] <- m1
        }
        
      #print(parents_1_new)
      #print(parents_2_new)
      
      #checking constraints
      #if violate constraints than reproduce again
      #else save to next population
      fit_value_1 <- problem_function(parents_1_new)
      fit_value_2 <- problem_function(parents_2_new)
      #print(fit_value_1)
      #print(fit_value_2)
      if(!is.na(fit_value_1) & !is.na(fit_value_2)){
        next_population[N_next_population+1,] <- parents_1_new
        next_population[N_next_population+2,] <- parents_2_new
        
        N_next_population <- N_next_population+2
      }
      #print(fit_value_1)
      #print(fit_value_2)
    }
    
    #STEP 5
    #evaluation: evaluate the fitness value of new population
    eval_fit_value <- apply(next_population , 1 , problem_function)
    best_fit_value <- max(eval_fit_value)
    best_individual_idx <- which.max(eval_fit_value)
    best_individual_gene <- next_population[best_individual_idx,]
    
    #calculate standard deviation
    s_f <- sd(eval_fit_value)
    # print('--eval fit value--')
    # print(eval_fit_value)
    # print('--max s--')
    # print(s_max)
    # print('--sf--')
    # print(s_f)
    
    ##Termination
    #Criterion 1
    if(s_f <= s_max){
      print(paste0('Algorithm converged'))
      print(paste0('generation: ', G ))
      print(paste0('maximum fitness value: ', best_fit_value ))
      print(paste0('minimum total distance: ', 1/(best_fit_value) ))
      print(paste0('design variable: ', paste0(best_individual_gene , collapse = ", ") ))
      break
    }
    
    #display information
    # print(paste0('generation: ', G ))
    # print(paste0('maximum fitness value: ', best_fit_value ))
    # print(paste0('design variable: ', paste0(best_individual_gene , collapse = ", ") ))
    
    
    #set new population as parents
    population <- next_population
  }
  ##Termination
  #Criterion 2
  if(G == iter_max){
    print(paste0('the maximum number of generation is reached'))
    print(paste0('generation: ', G ))
    print(paste0('maximum fitness value: ', best_fit_value ))
    print(paste0('minimum total distance: ', 1/(best_fit_value) ))
    print(paste0('design variable: ', paste0(best_individual_gene , collapse = ", ") ))
  }
  exe_time <- proc.time() - starttime
  print(exe_time)
  
  return(best_individual_gene)
}
