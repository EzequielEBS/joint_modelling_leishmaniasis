# Load libraries
library(JMbayes2)
library(ggplot2)

# Load data
long_leish <- read.csv("data/long_data_leishmaniasis_tte.csv")
time_data <- read.csv("data/tte_data.csv")

head(time_data)
head(long_leish)

# Fit the model
fit_vector <- coxph(Surv(time_vector, status_vector) ~ prop_cer, 
                    data = time_data,
                    control = coxph.control(iter.max = 10000))
fm7 <- lme(temperature_c ~ 1, data = long_leish, random = ~ year | code_7d)
fm10 <- lme(wind_speed_ms ~ 1, data = long_leish, random = ~ year | code_7d,
            control = lmeControl(maxIter = 1000, opt = "optim"))
fm26 <- lme(mb_1_2_4_4_5 ~ 1, data = long_leish, random = ~ year | code_7d)
jointFit <- jm(fit_vector, 
                list(fm7, fm10, fm26), 
                time_var = "year", 
                n_iter = 150000, 
                n_burnin = 50000, 
                n_chains = 4,
                cores = 4)

save(jointFit, file = "joint_fit.RData")
load("joint_fit.RData")

summary(fit_vector)
summary(jointFit)

# Trace plots
ggtraceplot(jointFit, "betas")
ggtraceplot(jointFit, "alphas")
ggtraceplot(jointFit, "gammas")

# Predictions
data <- merge(long_leish[c("code_7d", "year", "temperature_c", "wind_speed_ms", "mb_1_2_4_4_5", "prop_cer")], 
                         time_data[c("code_7d", "time_vector", "status_vector")], 
                         by = c("code_7d"))
t0 <- 8
cities <- c(3500808,
            3501202,
            3501301,
            3550308)
ND <- data[data$code_7d %in% cities, ]
ND <- ND[ND$year < t0, ]
ND$status_vector <- 0
ND$time_vector <- t0

predLong2 <- predict(jointFit, newdata = ND,
                     times = seq(t0, 20, length.out = 51),
                     return_newdata = TRUE)

predSurv <- predict(jointFit, newdata = ND, process = "event",
                    times = seq(t0, 20, length.out = 51),
                    return_newdata = TRUE)

plot(predLong2, predSurv)

cols <- c('#F25C78', '#D973B5', '#F28322')
for (city in cities) {
  plot(predLong2, predSurv, outcomes = 1:3, subject = city,
       fun_long = list(exp, identity, identity),
       fun_event = function (x) 1 - x,
       ylab_event = "Survival Probabilities",
       #ylab_long = c("Serum Bilirubin", "Prothrombin", "Ascites"),
       bg = 'white', col_points = cols, col_line_long = cols,
       col_line_event = 'blue', col_axis = "black", 
       fill_CI_long = c("#F25C7880", "#D973B580", "#F2832280"),
       fill_CI_event = "cornflowerblue",
       pos_ylab_long = c(1.9, 1.9, 0.08))
}

# ROC curve
roc1 <- tvROC(jointFit, newdata = data, Tstart = t0, Dt = 5, cores = 4)
roc2 <- tvROC(jointFit, newdata = data, Tstart = t0+1, Dt = 5, cores = 4)
roc3 <- tvROC(jointFit, newdata = data, Tstart = t0+2, Dt = 5, cores = 4)
roc4 <- tvROC(jointFit, newdata = data, Tstart = t0+3, Dt = 5, cores = 4)

plot(roc1)
plot(roc2)
plot(roc3)
plot(roc4)

tvAUC(roc1)
tvAUC(roc2)
tvAUC(roc3)
tvAUC(roc4)
