
#Gaurav Baruah, PhD student, University of Zurich, www.popecol.org; 
#https://gauravkbaruah.github.io/ ; (gaurav.baruah@ieu.uzh.ch)

rm(list=ls())
set.seed(12)

#environment function
env<-function(t){
  e.mean<-matrix(NA,nrow=5000,ncol=2)
  e.mean[1,1]<-0
  e.mean[1,2]<-0
  g<-0.0
  
  if (t <500) {
    
    e.mean[t,1]<-0
    e.mean[t,2]<-0
  }
  else if (t>=500) {
    e.mean[t,1]<-5 -5*(t/500)  # environmental cue
    e.mean[t,2]<- 20 -20*(t/500)     # optimum environment
  }          
  
  return(e.mean[t,])
}


#function for simulating the dynamics of the model
#dynamics() <- function that simulates the stochastic model
#tmax<- total time
#omega<- width of fitness function
#var.size<- variation in the mean.breeding value
#rho <- environmental predictability p
#var.U<- variability in cue
#var.theta <- variability in optimum phenotype
#r0 <- net reproductive rate.
#mean.breed<- mean breeding value

dynamics<-function(tmax,noise.resids,omega,var.size,rho,var.U, var.Theta,r0,mean.breed,beta){

  c<-matrix(NA,nrow=tmax,ncol=2)
  mean.breed<-numeric()
  theta<-numeric()
  N<-numeric()
  pop.breed<-numeric()
  mean.fitness<-numeric()
  cue<-numeric()
  var<-numeric()
  phenotype<-numeric()
  rand.s<-numeric()
  
  #initialization
  var[1] <- var.size
  theta[1]<-0
  N[1]<-70
  mean.breed[1]<-phenotype[1] <-cue[1]<-rand.s[1]<-c[1,1]<-0
  #pop.breed[1]<-mean(breed[,1])
  mean.fitness[1]<-1
  c[1,2]<-theta[1]
  
  
  for (t in 2:tmax) {
    
    S = matrix(c(var.U, rho*sqrt(var.U*var.Theta), rho*sqrt(var.U*var.Theta), var.Theta),2)  #S is a covariance matrix of the environment and the cue of the environment
    c[t,] <- mvrnorm(1,env(t),S)  #multivariate random distribution
    cue[t]<- c[t,1]# env(t)[1] +rnorm(1,0,sqrt(var.U )) # c[t,1]
    theta[t] <-  c[t,2] #env(t)[2] + rnorm(1,0,sqrt(var.Theta))#c[t,2]
    
    
    rand.s[t]<-rnorm(1,0,noise.resids)
    var[t] <- var.size 
    
    #(var.size/(omega+var(s[,t-1])))*(theta[t]-pop.breed[t]-beta*cue[t]+rand.s[t]) 
    mean.breed[t]<-mean.breed[t-1]+ 
      var.size*sqrt(omega)*(1/(var.size+omega))^1.5*(theta[t-1]-mean.breed[t-1]-beta*cue[t-1]+rand.s[t-1])*exp(-(mean.breed[t-1]+beta*cue[t-1]+rand.s[t-1]-theta[t-1])^2/(2*(var.size+omega)))
    phenotype[t-1]<- mean.breed[t-1] +beta*cue[t-1]+rand.s[t-1]  # dynamics of the phenotype
    
    mean.fitness[t] = (sqrt(omega)/sqrt(var.size+omega))*exp(-( phenotype[t-1]-theta[t-1])^2/(2*(var.size+omega))) 
    N[t]<-N[t-1]*(r0^(1-N[t-1]/73))*mean.fitness[t-1]  # discrete population dynamics
    
  }
  
  list(N=N,mean.breed=mean.breed,phenotype=phenotype,theta=theta,cue=cue,one=mean.fitness,omega=omega,var=var)
  
}



### FOR parallelising the R code ####

mc.replica = function(iter, mean.breed, ...){
  set.seed(rnorm(1,as.numeric(Sys.time())-Sys.getpid(),10000))
  init = mean.breed 
  replicate = dynamics(mean.breed=init, ...)
  replicate$start = init
  replicate$iter = iter
  return(replicate)
}
require(MASS)

#######################STARTING PARAMETERS##############

start.mean.breed=0
reps<-100 #no. of replicate simulation
EWS.plasticity.0.5<-lapply(1:reps, mc.replica,tmax=800,noise.resids=0,
                             omega=20,beta=0.5,var.size=0.25,rho=1,var.U=1.25^2,
                             var.Theta=1.25^2,r0=1.2,mean.breed=start.mean.breed)


