library(MILC)
library(survival)

# vector of follow-up times
futime <- c()
# vector of follow-up statuses
fustat <- c()
status <- rep(FALSE,100)

# for each year in time we want to predict
for (year in 50:120){
  # make 100 simulations of natural history
  set.seed(Sys.time())
  for (i in 1:100){
    p <- nat_hist ( c(runif(5),50,30), 
                    120-year, "male", "current", 20, NA, 0.00042, 
                    c(3.91, 3.91), c(1.1, 1.1), c(2.8, 2.8))
    
    # does survival time exceed t 
    status[i] <- year > unlist(p["T_dl"]) # TRUE/FALSE  
    
  }

futime <- c(futime, rep(year,100))
fustat <- c(fustat, status)
  
}


history_simulation_data <- data.frame(Futime = as.numeric(futime), Fustat = fustat)

# create survival object
surv_object <- Surv(time = history_simulation_data$Futime, 
                    event =history_simulation_data$Fustat)
#surv_object

# Kaplan-Meier estimator
ffit <- survfit(surv_object~1, data=history_simulation_data, conf.type = "log-log")
ffit
summary(ffit)

#plot(ffit, xlim = c(50, 120), ylim=c(0.2,1), col = "#2E9FDF", lwd=2)     

survminer::ggsurvplot(ffit, palette = "#2E9FDF",
                      risk.table.x.text=FALSE,
                      xlim = c(50, 120), break.time.by = 10,
                      ylim=c(0.2,1),
                      legend="none")

