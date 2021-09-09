#optim_func : function of optimization problem
#temp : initial temperature
#design_init : initial design varaible
#temp_reduct : temperature reduction rate
#Boltzmann_constant : Boltzmann constant
#range_type : fixed or dynamic. whether the range of design variable is fixed or adding and subtracting by given value 
#range_design : range of design variable
#n_design : number of design variable
#iter : number of iteration

SA <- function(optim_func , temp , design_init , temp_reduct, Boltzmann_constant = 1, 
               range_type = 'fixed' ,range_design , n_design, iter){
  ##step 0 and step 1
  #setting the initial points vector
  x_init <- matrix(design_init , n_design , 1)
  #calculate the objective function value of the initial design point
  f_init <- optim_func(design_init)
  #save the objective function values
  f_values <- c()
  f_values[1] <- f_init
  #number of cycle
  p <- 0
  
  #test for convergence
  not_converge <- TRUE
  while (temp > 1 & not_converge){
    p <- p+1
    ##step 2
    #generate a new design point
    for(i in 1:iter){
      
      #save the objective function values
      f_values[i+1] <- f_init
      
      #select uniformly distributed RV
      u <- runif(n_design)
      #found the range of x
      if(range_type == 'fixed'){
        range_x <- matrix(rep(range_design , n_design) , 2 , n_design)
      }else if(range_type == 'dynamic'){
        #add and subtract design range by given value
        range_x <- apply(x_init , 1 , function(x){ return(x + range_design) })
      }
      #found the new design point in the range of x and corresponding to u
      r <- range_x[1,] + u*(range_x[2,]-range_x[1,])
      #calculate the objective function value of the new design point
      x_new <- matrix(r , n_design , 1)
      f_new <- optim_func(r)
      #calculate the difference between previous and new objective function value
      delta_f <- f_new - f_init
      
      ##step 3 and step 4
      if(delta_f <= 0){
        #if the difference is negative, accept the current point
        x_init <- x_new
        f_init <- optim_func(c(x_init))
      }else{
        #use Matropolis criterion to decide whether to accpet or reject
        #randomly choose a RV in range (0,1)
        prob_thershold <- runif(1)
        #calculate the probability of accepting current point
        prob <- exp(-delta_f/(Boltzmann_constant*temp))
        if(prob > prob_thershold){
          #if the probability is greater than the RV, accept the current point
          x_init <- x_new
          f_init <- optim_func(c(x_init))
        }
      }
    }
    
    temp <- temp_reduct*temp
    
    #test for convergence
    if(length(f_values) >= 50){
      if(sd(tail(f_values , 50)) != 0){
        not_converge <- TRUE
      }else{
        not_converge <- FALSE
      }
    }else{
      not_converge <- TRUE
    }
  }
  
  print(paste0('cycle:' , p))
  if(temp < 1){
    print('temperature is smaller than 1')
  }else if(sd(tail(f_values , 10)) == 0){
    print('the algprithm is converged')
  }
  print(paste0('design variable:', paste0(x_new , collapse = ',')))
  print(paste0('value:', f_new))
  return(list(x_new , f_new))
  
}

p2 <- function(x){
  x1 <- x[1]
  x2 <- x[2]
  return(6*x1^2 - 6*x1*x2 + 2*x2^2 -x1 -2*x2)
}

SA(optim_func = p2 , temp = 434.25 , design_init = c(18,13) , temp_reduct = 0.9 , n_design = 2, iter = 500 )
SA(optim_func = p2 , temp= 434.25 , design_init = c(18,13) , temp_reduct = 0.9, Boltzmann_constant = 1, 
   range_type = 'dynamic' ,range_design = c(-6,6) , n_design = 2, iter = 500)

