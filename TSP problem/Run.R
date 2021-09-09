rawData <- read.csv('7-11.csv' , header = T)
# calculate distance
ball2square <- function(coor){
  att <- coor[1]
  longtt <- coor[2]
  x <- 6400*cos(att)*cos(longtt)
  y <- 6400*cos(att)*sin(longtt)
  z <- 6400*sin(att)
  return(c(x,y,z))
}
store_names <- c('馥樺','胡適','聯坊','玉德','忠陽','香城','港德','港運門市',
 '雄強門市','耀港門市','鑫貿','鵬馳','慈愛',
 '新福玉','經貿','聯成','港興','港環球','港麗',
 '華技','港高鐵','港捷','港勝','研究','凱松','港泰',
  '向揚','庄研','昆陽','林坊','中坡','中研','中貿',
  '玉成')

threeDcoor <- apply(rawData, 1, ball2square)
threeDcoor <- t(threeDcoor)

distance <- as.matrix(dist(threeDcoor))
distance <- 6400*pi*2*(asin(0.5*distance/6400))/180

# implement GA
# read GA algorithm from GA.R
if(!exists("GA", mode = "function")){
  source('GA.R')
}
# define optimzation objective function
TSPfunction_GA <- function(route){
  total_distance <- 0
  route_return <- c(route , route[1])
  for (i in 1:length(route)) {
    a <- route_return[i]
    b <- route_return[i+1]
    total_distance <- total_distance + distance[a,b]
  }
  
  return((1/total_distance))
}

# implement
GA_best_route <- GA(problem_function = TSPfunction_GA,
   iter_max = 1000, s_max = 0.0001,  N_population = 100 , N_gene = 34,
   p_c = 0.8,p_m = 0.2)

# plot
return_route <- c(GA_best_route,GA_best_route[1])
xx <- c()
yy <- c()
for(r in 1:length(return_route)){
  xx[r] <- rawData[return_route[r],1]
  yy[r] <- rawData[return_route[r],2]
}
plot(xx,yy , type = 'b' , pch = 20 ,col = "darkblue",
     xlab = '緯度', ylab = '經度',
     main = '最佳路徑地圖:GA', family = "Hiragino Sans CNS W3")


all_path <- list()

path_name <- c()
for(rr in 1:length(return_route)){
  path_name[rr] <- store_names[return_route[rr]]
}

all_path[[1]] <- paste0(path_name , collapse ="-")
# implement SA
# read SA algorithm from SA.R
if(!exists("SA", mode = "function")){
  source('SA.R')
}
# define optimzation objective function
TSPfunction_SA <- function(route){
  total_distance <- 0
  route_return <- c(route , route[1])
  for (i in 1:length(route)) {
    a <- route_return[i]
    b <- route_return[i+1]
    total_distance <- total_distance + distance[a,b]
  }
  
  return(total_distance)
}

# select initial design path
path <- sample(1:34,34,replace = F)

#select initial temperature
temperature <- c()
for (n in 1:10) {
  path_t <- sample(1:34,34,replace = F)
  temperature[n] <- TSPfunction_SA(path_t)
}
start_temper <- mean(temperature)

SA_best_answer <- SA(optim_func = TSPfunction_SA , temp = start_temper , path_init = path , temp_reduct = 0.9, Boltzmann_constant = 1.3*(10^(-23)), 
       n_store = 34, iter = 500, max_cycle = 1000)
SA_best_route <- SA_best_answer[[1]]

# plot
return_route <- c(SA_best_route,SA_best_route[1])
xx <- c()
yy <- c()
for(r in 1:length(return_route)){
  xx[r] <- rawData[return_route[r],1]
  yy[r] <- rawData[return_route[r],2]
}
plot(xx,yy , type = 'b' , pch = 20 ,col = "darkblue",
     xlab = '緯度', ylab = '經度',
     main = '最佳路徑地圖:SA', family = "Hiragino Sans CNS W3")

path_name <- c()
for(rr in 1:length(return_route)){
  path_name[rr] <- store_names[return_route[rr]]
}

all_path[[2]] <- paste0(path_name , collapse ="-")

# implement ACO
# read ACO algorithm from ACO.R
if(!exists("ACO", mode = "function")){
  source('ACO.R')
}
# define optimzation objective function
TSPfunction_ACO <- function(route){
  total_distance <- 0
  for (i in 1:(length(route)-1)) {
    a <- route[i]
    b <- route[i+1]
    total_distance <- total_distance + distance[a,b]
  }
  
  return(total_distance)
}

ACO_best_route <- ACO(objective = TSPfunction_ACO ,
                      N = 15 ,n_store = 34 , iter = 1000 ,alpha = 1,
                      beta = 1, p = 0.5)

# plot
return_route <- ACO_best_route
xx <- c()
yy <- c()
for(r in 1:length(return_route)){
  xx[r] <- rawData[return_route[r],1]
  yy[r] <- rawData[return_route[r],2]
}
plot(xx,yy , type = 'b' , pch = 20 ,col = "darkblue",
     xlab = '緯度', ylab = '經度',
     main = '最佳路徑地圖:ACO', family = "Hiragino Sans CNS W3")

path_name <- c()
for(rr in 1:length(return_route)){
  path_name[rr] <- store_names[return_route[rr]]
}

all_path[[3]] <- paste0(path_name , collapse ="-")

# implement Tabu Search
# read Tabu Search algorithm from Tabu.R
if(!exists("Tabu", mode = "function")){
  source('Tabu.R')
}
# define optimzation objective function
TSPfunction_Tabu <- function(route){
  total_distance <- 0
  route_return <- c(route , route[1])
  for (i in 1:length(route)) {
    a <- route_return[i]
    b <- route_return[i+1]
    total_distance <- total_distance + distance[a,b]
  }
  
  return(total_distance)
}

Tabu_best_route <- Tabu(objective = TSPfunction_Tabu, n_Candidates = 100, n_store = 34,
     tabu_length = 10, max_iter=1000)

# plot
return_route <- c(Tabu_best_route,Tabu_best_route[1])
xx <- c()
yy <- c()
for(r in 1:length(return_route)){
  xx[r] <- rawData[return_route[r],1]
  yy[r] <- rawData[return_route[r],2]
}
plot(xx,yy , type = 'b' , pch = 20 ,col = "darkblue",
     xlab = '緯度', ylab = '經度',
     main = '最佳路徑地圖:Tabu Search', family = "Hiragino Sans CNS W3")


path_name <- c()
for(rr in 1:length(return_route)){
  path_name[rr] <- store_names[return_route[rr]]
}

all_path[[4]] <- paste0(path_name , collapse ="-")

names(all_path) <- c("GA", "SA", "ACO", "Tabu Search")
  