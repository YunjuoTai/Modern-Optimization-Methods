#Programming of Genetic Algorithm

##define Genetic Algorithm function
##parameters:
#problem_function: a function, include objective function and all contraints of the optimization problem
#iter_max: a positive number, the maximum number of generation
#s_max: a positive number, the value of standard deviation use as a convergence criterion
#N_population: a positive number, size of the population
#N_gene: a positive number, number of design variables
#p_c: probability of crossover
#one_point: logical, one-point or multi-point crossover
#n_point: a positive number, number of points to crossover
#p_m: probability of mutation
#single_point: logical, single-point or bit-wise mutation
GA <- function(problem_function, 
               iter_max, s_max = NULL,  N_population , N_gene,
               p_c, one_point = TRUE, n_point = NULL,
               p_m, single_point = TRUE,
               seed_num = NULL){
  ##STEP 1
  #generate a random population of size N_population
  if(!is.null(seed_num)){
    set.seed(seed_num)
    }
  population <- matrix(sample(c(0,1) , size = N_population*N_gene , replace = T) , N_population , N_gene)
  
  #if violate constraints than sample population again
  fit_value <- apply(population , 1 , problem_function)
  #print(fit_value)
  while(sum(is.na(fit_value)) > 0 ){
    if(!is.null(seed_num)){
      seed_num <- seed_num+1
      set.seed(seed_num)
    }
    population <- matrix(sample(c(0,1) , size = N_population*N_gene , replace = T) , N_population , N_gene)
    fit_value <- apply(population , 1 , problem_function)
  }
  #print(fit_value)
  #calculate the standard deviation of fitness value of the first generation as defalut value for s_max
  if(is.null(s_max)){
    s_max <- sd(fit_value)
    }
  
  for(G in 1:iter_max){
    
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
      #print(parents)
      #next generation
      parents_1_new <- parents_1
      parents_2_new <- parents_2
      
      ##STEP 3
      #crossover
      if(runif(1) >= p_c){
        
        if(one_point == T){
          #randomly select crossover site
          if(!is.null(seed_num)){
            set.seed(seed_num)
          }
          switch_point <- sample(N_gene ,1)
          #switch gene
          parents_1_new[switch_point:N_gene] <- parents_2[switch_point:N_gene]
          parents_2_new[switch_point:N_gene] <- parents_1[switch_point:N_gene]
        }else{
          #randomly select crossover site
          if(!is.null(seed_num)){
            set.seed(seed_num)
          }
          switch_point <- sort(sample(N_gene , n_point))
          #switch gene
          if(n_point %% 2 == 1){
            append(switch_point,N_gene)
          }
          for (site in 1:ceiling(n_point/2)) {
            parents_1_new[switch_point[2*site-1]:switch_point[2*site]] <- parents_2[switch_point[2*site-1]:switch_point[2*site]]
            parents_2_new[switch_point[2*site-1]:switch_point[2*site]] <- parents_1[switch_point[2*site-1]:switch_point[2*site]]
          }
        }
      }
      #print(parents_1_new)
      #print(parents_2_new)
      ##STEP 4
      #mutation
      if(single_point == T){
        
        #mutation for parent1
        if(!is.null(seed_num)){
          set.seed(seed_num)
        }
        if(runif(1) >= p_m){
          #randomly select mutation site
          if(!is.null(seed_num)){
            set.seed(seed_num)
          }
          mutation_site <- sample(N_gene , 1)
          parents_1_new[mutation_site] <- ! parents_1_new[mutation_site]
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
          mutation_site <- sample(N_gene , 1)
          parents_2_new[mutation_site] <- ! parents_2_new[mutation_site]
        }
        
      }else{
        #mutation for parent1
        if(!is.null(seed_num)){
          set.seed(seed_num)
        }
        mutation_site <- runif(N_gene) >= p_m
        parents_1_new[mutation_site] <- ! parents_1_new[mutation_site]
        
        #mutation for parent2
        if(!is.null(seed_num)){
          set.seed(seed_num+1)
        }
        mutation_site <- runif(N_gene) >= p_m
        parents_2_new[mutation_site] <- ! parents_2_new[mutation_site]
      }
      #print(parents_1_new)
      #print(parents_2_new)
      
      #checking constraints
      #if violate constraints than reproduce again
      #else save to next population
      fit_value_1 <- problem_function(parents_1_new)
      fit_value_2 <- problem_function(parents_2_new)
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
    
    ##Termination
    #Criterion 1
    if(s_f > s_max){
      print(paste0('Algorithm converged'))
      print(paste0('generation: ', G ))
      print(paste0('maximum fitness value: ', best_fit_value ))
      print(paste0('design variable: ', paste0(best_individual_gene , collapse = ", ") ))
      break
    }
    
    #display information
    print(paste0('generation: ', G ))
    print(paste0('maximum fitness value: ', best_fit_value ))
    print(paste0('design variable: ', paste0(best_individual_gene , collapse = ", ") ))
    
    
    #set new population as parents
    population <- next_population
  }
  ##Termination
  #Criterion 2
  if(G == iter_max){
    print(paste0('the maximum number of generation is reached'))
    print(paste0('generation: ', G ))
    print(paste0('maximum fitness value: ', best_fit_value ))
    print(paste0('design variable: ', paste0(best_individual_gene , collapse = ", ") ))
  }
  return(best_individual_gene)
}


#define objective function and constraints
Knapsack_f <- function(X){
  #constraints
  #if violate constraint than return NA
  #else return fitness value
  if(X[1]*15 + X[2]*3 + X[3]*2 + X[4]*5 + X[5]*9 + X[6]*20 > 30){
    return(NA)
  }else{
    #objective function
    return(X[1]*15 + X[2]*7 + X[3]*10 + X[4]*5 + X[5]*8 + X[6]*17 )
  }
}

GA(problem_function = Knapsack_f,
   iter_max = 100, s_max = NULL,  N_population = 10 , N_gene = 6,
   p_c = 0.8, one_point = TRUE, n_point = NULL,
   p_m = 0.05, single_point = TRUE,
   seed_num = NULL)



