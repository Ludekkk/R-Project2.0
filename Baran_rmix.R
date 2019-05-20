#funkcja

rmix <- function(n,family_1,par_1,family_2,par_2,p){
  
  prob <- sample(c(1,2), n, prob = c(p, 1-p), replace = T)
  
  sample1 <- eval(
    parse( text = 
             paste0("r", family_1, "(length(which(prob == 1)), ", 
                    paste(par_1, collapse = ", "),
                    ")"))
  )
  
  sample2 <- eval(
    parse( text = 
             paste0("r", family_2, "(length(which(prob == 2)), ", 
                    paste(par_2, collapse = ","),
                    ")"))
  )
  
  c(sample1,sample2)
  
}