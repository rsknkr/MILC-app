library(MILC)
library(survival)

# vector of follow-up times
futime <- rep(50,100)


# vector of possible times of death from LC
dl <- replicate(100, unlist(nat_hist ( c(runif(5),50,30),
                         20, "male", "current", 20, NA, 0.00042, 
                         c(3.91, 3.91), c(1.1, 1.1), c(2.8, 2.8))["T_dl"]))


# compare t_final with time of lc death   
status <- sapply(dl, function(x) 70 > x)
    

history_simulation_data <- data.frame(Futime = as.numeric(futime), Fustat = status)

# create survival object
surv_object <- Surv(time = history_simulation_data$Futime, 
                    event = history_simulation_data$Fustat)
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

