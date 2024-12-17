# Load data
long_leish <- read.csv("data/long_data_leishmaniasis.csv")
non_temp_cov <- long_leish[long_leish$year == 2003, c("code_7d", "total_area_m2", "prop_cer", "prop_ma")]

time_data <- data.frame(code_7d = unique(long_leish$code_7d), 
                        time_vector = NA, 
                        time_dogs = NA, 
                        time_human = NA,
                        status_dogs = NA, 
                        status_vector = NA,
                        status_human = NA)

# Create time to event variable
comp_time <- function(code_7d, t0, variable){
    inv_time <- long_leish[long_leish$code_7d == code_7d & long_leish[, variable] == 1, "year"]
    tf <- ifelse(length(inv_time) > 0, min(inv_time), 2017)
    status <- ifelse(tf == 2017, 0, 1)
    time <- tf - t0
    if (variable == "status_dogs"){
        time_data[time_data$code_7d == code_7d, "time_dogs"] <<- time
    } else if (variable == "status_vector"){
        time_data[time_data$code_7d == code_7d, "time_vector"] <<- time
    } else if (variable == "status_human"){
        time_data[time_data$code_7d == code_7d, "time_human"] <<- time
    }
    time_data[time_data$code_7d == code_7d, variable] <<- status
}

# Define code
code <- unique(long_leish$code_7d)
# Define inicial time
t0 <- 2000

# Compute time to event for dogs
lapply(code, comp_time, t0 = t0, variable = "status_dogs")

# Compute time to event for vectors
lapply(code, comp_time, t0 = t0, variable = "status_vector")

# Compute time to event for humans
lapply(code, comp_time, t0 = t0, variable = "status_human")

# Substituting year in long_leish
long_leish$year <- long_leish$year - t0

# Merge time data with non-temporal covariates
time_data <- merge(time_data, non_temp_cov, by = "code_7d")

# Save data
write.csv(long_leish, "data/long_data.csv", row.names = FALSE)
write.csv(time_data, "data/time_data.csv", row.names = FALSE)
