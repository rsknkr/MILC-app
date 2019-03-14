hazard.predictions <- function(age, upper = 20, gender, cigs, smok_status, ts=NA, tq=NA) {
  # define if person smokes
  smokes <- "N"
  if (cigs>0) {
    smokes<-"Y"
  }
  # create empty vectors for predictions
  adiag <- rep(0,100)
  aLC <- rep(0,100)
  first_cell <- rep(0,100)
  
  for (j in 1:100){
    # hazard of the onset of 1st malignant cell between two time points (age and age+upper)
    # ht_mal_int
    
    first_cell[j] <- ht_mal_int(age, age+upper, gender, cigs, smokes)
    
    # simulate natural history
    
    nh <- nat_hist (c(runif(5),age,cigs), 
                    upper, gender, smok_status, ts, tq, 
                    0.00042, c(3.91, 3.91), c(1.1, 1.1), c(2.8, 2.8))
    
    # predicted age at diagnosis - nat.hist[T_diagn]
    adiag[j] <- unlist(nh["T_diagn"])
    
    # predicted age at death from LC - nat.hist[T_dl]
    aLC[j] <- unlist(nh["T_dl"])  

  }
  
 return(c(mean(first_cell), mean(adiag), mean(aLC)))
}

hazard.predictions(age=50, upper = 20, gender = "male", cigs = 30, 
                   smok_status = "current", ts=20, tq=NA)

# print(paste0("Hazard of the onset of the first malignant cell by the age of ", age+upper,
#              " is ", mean(first_cell))),
# print(paste0("Mean predicted age at diagnosis is ", mean(adiag), " years")),
# print(paste0("Mean predicted age at death from lung cancer is ", mean(aLC), " years"))
