library(MILC)

#=====================================================================================
# DATA FRAME OF PREDICTIONS: AGE OF DIAGNOSIS WITH LC AND AGE OF DEATH FOR ANY REASON
#=====================================================================================

# # CREATE BY ROW
# df <- do.call(rbind,
#               Map(function(x)
#                 data.frame(
#                   nat_hist (
#                     c(runif(5), age = 50, 30),
#                     pred_yrs = 70,
#                     gender = "male",
#                     status = "current",
#                     ts = 20,
#                     tq = NA,
#                     0.00042,
#                     c(3.91, 3.91),
#                     c(1.1, 1.1),
#                     c(2.8, 2.8)
#                   )[c("T_diagn", "T_death")]
#                 ), x = 1:100))
# 
# # system.time() for 100
# # user  system elapsed
# # 2.31    3.56    5.91
# 
# df

#========================================
# SIMULATE DATA WITH parallel processing
#========================================

library(doParallel)

registerDoParallel(detectCores())

# Return a data frame
df <-
  foreach (i = 1:100,
           .combine = rbind,
           .packages = 'MILC') %dopar% {
             unlist(
               nat_hist(
                 c(runif(5), age = 50, 30),
                 pred_yrs = 70,
                 gender = "male",
                 status = "current",
                 ts = 20,
                 tq = NA,
                 0.00042,
                 c(3.91, 3.91),
                 c(1.1, 1.1),
                 c(2.8, 2.8)
               )[c("T_diagn", "T_death")]
             )
           }


# system.time() for 100
# user  system elapsed
# 0.08    0.02    2.03

# finish parallel processing
stopImplicitCluster()


#======================
# CREATE SURVIVAL DATA
#======================

## create columns with time and status

# old colnames - c("T_diagn", "T_death")
colnames(df) <- c("status", "time")

cl <- makeCluster(detectCores())
T_final <- 120

clusterExport(cl, c("df","T_final")) #  in order for the function to see the variable

# IF diagnosed (T_diagn) before death (T_death) THEN status=1, ELSE 0
# IF dead (T_death) before the end of follow-up (T_final) THEN time=T_death, ELSE T_final
  # age+pred_yrs # also one of nat_hist values
df[, "status"] <-
  parApply(cl, df, 1, 
           function(x)
             1 * (x[1] < x[2]))
df[, "time"] <-
  parSapply(cl, df[, "time"], function(x)
    x = min(x, T_final))

stopCluster(cl)


#===================================================
# FIT SURVIVAL FUNCTION AND PLOT KAPLAN-MEIER CURVE
#===================================================
library(survival)

## Kaplan-Meier estimator without grouping

T_entry <- 50 #age at the beginning of the prediction period
T_final <- 120 #age+pred_yrs #age at the end of the prediction period

km <- survfit(Surv(time, status) ~ 1, data = data.frame(df))
plot(km,
     xlab = "Follow-up time (years)", 
     ylab = "Survival probability",
     xlim = c(T_entry, T_final),
     ylim = c(0,1))


# survminer::ggsurvplot(km, palette = "#2E9FDF",
#                       risk.table.x.text=FALSE,
#                       xlim = c(T_entry, T_final), break.time.by = 10,
#                       ylim = c(0.85,1),
#                       legend="none")
