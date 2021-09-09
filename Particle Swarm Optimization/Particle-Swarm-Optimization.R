##parameters
#object: objective function
#N: number of particles
#init: initial population, the defalut is randomly generated population
#x_range: the range of the design variable
#iter: number of iterations
#C: a vector, relative importance of the global and local
#W: the weight of velocity
PSO <- function(object, N, init = NA, x_range, iter, C, W ){
  #generate initial position
  if (length(init) == 1 & is.na(init[1]) == T){
    init <- sample(seq(x_range[1],x_range[2]) , N)
  }
  #save initial position
  position <- matrix(NA,N,iter+1)
  position[,1] <- init
  #set the initial velocity to zero
  init_velocity <- rep(0,N)
  #evaluate the objective function values
  init_value <- object(init)
  #save initial value
  value <- matrix(NA,N,iter+1)
  value[,1] <- init_value
  #save w
  inertia <- rep(0,iter+1)
  inertia[1] <- W
  #set the iteration
  for (i in 1:iter) {
    #find the local best position
    local_value_ind <- apply(value, 1, which.max)
    p_local <- position[cbind(seq_along(local_value_ind),local_value_ind)]
    #find the global best position
    p_global <- position[which.max(value)]
    
    #find the velocities of particles
    #randomly choose 2 number in range of (0,1)
    r <- runif(2)
    velocity <- W*init_velocity + C[1]*r[1]*(p_local-init) + C[2]*r[2]*(p_global-init)
    
    #find the new values of x_j ans save it
    x <- init + velocity
    position[,i+1] <- x
    
    #evaluate the objective function value of the new x_j and save it
    value[,i+1] <- object(x)
    
    #test convergency
    if(sd(object(x)) == 0){
      print('Converged')
      print('number of iteration: ', iter)
      print(paste0('optimum solution:', x[which.max(object(x))]))
      print(paste0('optimun objective function value: ', max(object(x))))
      break
    }
    
    #update x and velocity
    init <- x
    init_velocity <- velocity
    #update W
    W <- max(inertia) - ((max(inertia) - min(inertia))/iter)*i
    inertia[i+1] <- W
  }
  print('Maximum iteration is reached')
  print(paste0('number of iteration: ', iter))
  print(paste0('optimum solution:', x[which.max(object(x))]))
  print(paste0('optimun objective function value: ', max(object(x))))
  return(x[which.max(object(x))])
}

#objective function
f <- function(x){
  return(11+2*x-x^2)
}
#PSO
PSO(f, N=4, x_range = c(-2,2), iter=20, C=c(1,1), W=1)



f <- function(x){
  return(-x^5+5*x^3+20*x-5)
}

PSO(f, N=4, init=c(-2,0,1,3), x_range = c(-4,4), iter=20, C=c(1,1), W=1)
