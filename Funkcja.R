#funkcja

rmix <- function(n,family_1,par_1,family_2,par_2,p){
  
  prob <- sample(c(1,2), n, prob = c(p, 1-p), replace = T)
  
  fun <- get(paste('r',family_1, sep = ""))
  fun2 <- get(paste('r',family_2, sep = ""))
  
  if (length(par_1) == 1)
  {
    sample1 <- fun(length(which(prob == 1)),par_1[1]) 
  }
  else if (length(par_2) == 2) 
  {
    sample1 <- fun(length(which(prob == 1)),par_1[1],par_1[2])
  }
  
  if (length(par_2) == 1)
  {
    sample2 <- fun2(length(which(prob == 2)), par_2[1])
  }
  else if (length(par_2) == 2)
  {
    sample2 <- fun2(length(which(prob == 2)), par_2[1],par_2[2])
  }
  
  c(sample1,sample2)

}