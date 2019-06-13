#########################################################
##
## Nonlinear concentraton-response function: Health impact Calculation applying parametric function
##
## Date February 27, 2019
## by Alina Vodonos Zilberg (avodonos@hsph.harvard.edu)
## Version 2 - updated Veronica Southerland 
## Date June, 13, 2019
#########################################################

library(readr)
library(dplyr)
library(foreign)
library(tidyr)

setwd("C:/Users/vtinney/Google Drive/EDF_shared/Inv_function/")
#########################################################
#### Data frame (df) must contatin the following variables;
# Example data frame.
#Let's say we want to calculate the excess deaths addtibutble to reducing the PM2.5 levels from 'pm2.5_high' to 'pm2.5_low', 
#first we need to compute the average slope between those two concentration points.

# All-cause mortality

df.all <- data.frame(pm2.5_high = c(40, 21.9, 14.9, 9.9, 6.9, 4.9, 3.4, 2.4, 1.4),
                pm2.5_low = c(22,  15, 10, 7, 5, 3.5, 2.5, 1.5, 0.01))

# Those are the parameters from the model
B0 <- 0.0059 #intercept slope
B1 <- 0.0705 # 1/pm2.5 slope
B0var <- 1.408353e-05  #varcov[1,1]
B1var <- 0.002551382  #varcov[2,2]
B0B1cov <- -0.0001884276  #varcov[1,2]

range <- 1:40
per.change <- (B0 + ((1/range*B1))*100)
plot(per.change)

# Area under y (from x = a to x = )  = F(b) -F(a)
#F(pm2.5_high) -F(pm2.5_low) = log(pm2.5_high) - log(pm2.5_low).

df.all$pm2.5_high_log <- log(df.all$pm2.5_high)
df.all$pm2.5_low_log <- log(df.all$pm2.5_low)
df.all$delta_pm <- (df.all$pm2.5_high - df.all$pm2.5_low)

df.all["mean_beta"] <- B0 + (B1 * ((df.all$pm2.5_high_log - df.all$pm2.5_low_log) / df.all$delta_pm ))
df.all["mean_beta_var"] <- B0var + B1var * ((df.all$pm2.5_high_log - df.all$pm2.5_low_log) / df.all$delta_pm ) ^ 2 + 2 * (B0B1cov * (df.all$pm2.5_high_log - df.all$pm2.5_low_log) / df.all$delta_pm)
df.all["mean_beta_var_se"] <- sqrt(df.all$mean_beta_var)
df.all["mean_beta_low"] <- (df.all$mean_beta - (1.96 * df.all$mean_beta_var_se))
df.all["mean_beta_high"] <- (df.all$mean_beta + (1.96 * df.all$mean_beta_var_se))
write.csv(df.all, 'inv.all.csv')

#########################################################

# All-cause mortality ELDERLY

df.eld <- data.frame(pm2.5_high = c(40, 25.9, 15.9, 9.9, 6.9, 4.4, 2.4, 1.4),
                     pm2.5_low = c(26, 16, 10, 7, 4.5, 2.5, 1.5, 0.01))

# Those are the parameters from the model
B0 <- 0.0089 #intercept slope
B1 <- 0.0705 # 1/pm2.5 slope

range <- 1:40
per.change <- (B0 + ((1/range*B1))*100)
plot(per.change)

# Area under y (from x = a to x = )  = F(b) -F(a)
#F(pm2.5_high) -F(pm2.5_low) = log(pm2.5_high) - log(pm2.5_low).

df.eld$pm2.5_high_log <- log(df.eld$pm2.5_high)
df.eld$pm2.5_low_log <- log(df.eld$pm2.5_low)
df.eld$delta_pm <- (df.eld$pm2.5_high - df.eld$pm2.5_low)

df.eld["mean_beta"] <- B0 + (B1 * ((df.eld$pm2.5_high_log - df.eld$pm2.5_low_log) / df.eld$delta_pm ))
write.csv(df.eld, "inv.eld.csv")
#########################################################

# CVD mortality all ages

df.cvd <- data.frame(pm2.5_high = c(40, 26.9, 15.9, 10.9, 7.9, 5.9, 4.4, 3.4, 2.4, 1.4),
                     pm2.5_low = c(27, 16, 11, 8, 6, 4.5, 3.5, 2.5, 1.5, 0.01))

# Those are the parameters from the model
B0 <- 0.0079 #intercept slope
B1 <- 0.0705 # 1/pm2.5 slope

range <- 1:40
per.change <- (B0 + ((1/range*B1))*100)
plot(per.change)

# Area under y (from x = a to x = )  = F(b) -F(a)
#F(pm2.5_high) -F(pm2.5_low) = log(pm2.5_high) - log(pm2.5_low).

df.cvd$pm2.5_high_log <- log(df.cvd$pm2.5_high)
df.cvd$pm2.5_low_log <- log(df.cvd$pm2.5_low)
df.cvd$delta_pm <- (df.cvd$pm2.5_high - df.cvd$pm2.5_low)

df.cvd["mean_beta"] <- B0 + (B1 * ((df.cvd$pm2.5_high_log - df.cvd$pm2.5_low_log) / df.cvd$delta_pm ))
write.csv(df.cvd, "inv.cvd.csv")
