SA <- function(optim_func , temp , path_init , temp_reduct, Boltzmann_constant = 1, 
               n_store, iter, max_cycle){
  
  starttime <- proc.time()
  ##step 0 and step 1
  #calculate the objective function value of the initial design point
  # path_init <- c(path_init,path_init[1])
  # design_init <- matrix(rep(0,34*34),34,34)
  # for (i in 1:34) {
  #   design_init[path_init[i],path_init[i+1]] <- 1
  # }
  f_init <- optim_func(path_init)
  #number of cycle
  p <- 0
  #print(f_init)
  
  #test for convergence
  #te <- c()
  while (temp > 1){
    p <- p+1
    ##step 2
    #generate a new design point
    for(iteration in 1:iter){
      
      #sample a new route
      rand <- runif(1)
      change_point <- sample(2:(n_store-1), 2, replace = F)
      u <- path_init
      if(rand>0.5){
        u[min(change_point)] <- path_init[max(change_point)]
        u[max(change_point)] <- path_init[min(change_point)]
      }else{
        reverse_section <- path_init[min(change_point):max(change_point)]
        for(po in 1:length(reverse_section)){
          u[min(change_point)+po-1] <- reverse_section[(length(reverse_section))+1-po]
        }
      }
      # u <- c(u,u[1])
      
      #transfer the new route into the design vector
      # x_new <- matrix(rep(0,n_store*n_store),n_store,n_store)
      # for (i in 1:n_store) {
      #   x_new[u[i],u[i+1]] <- 1
      # }
      #calculate the objective function value of the new design point
      f_new <- optim_func(u)
      #calculate the difference between previous and new objective function value
      delta_f <- f_new - f_init
      #print(delta_f)
      
      ##step 3 and step 4
      if(delta_f <= 0){
        #if the difference is negative, accept the current point
        path_init <- u
        f_init <- optim_func(path_init)
      }else{
        #use Matropolis criterion to decide whether to accpet or reject
        #randomly choose a RV in range (0,1)
        prob_thershold <- runif(1)
        #calculate the probability of accepting current point
        prob <- exp(-delta_f/(Boltzmann_constant*temp))
        #print(prob)
        if(prob > prob_thershold){
          #if the probability is greater than the RV, accept the current point
          path_init <- u
          f_init <- optim_func(path_init)
        }
      }
    }
    
    temp <- temp_reduct*temp
    
    if(p > max_cycle){
      print('reach max cycle number')
      break
    }
  }
  print(paste0('cycle:' , p))
  if(temp < 1){
    print('temperature is smaller than 1')
  }
  # print(paste0('design variable:', paste0(x_new , collapse = ',')))
  print(paste0('value:', f_init))
  
  exe_time <- proc.time() - starttime
  print(exe_time)
  
  return(list(path_init , f_init))
  
}
