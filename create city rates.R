library(dplyr)

setwd("C:/Users/vtinney/Google Drive/EDF_shared/Cities/df cities/")

bay <- read.csv('bay.dbf.csv', header=TRUE)
pop <- read.csv('pop.dbf.csv', header=TRUE)

bay2 <- cbind(bay, pop)
select.17 <- bay2[grep(".17", colnames(bay2))]
select.25 <- bay2[grep('.25', colnames(bay2))]
select.65 <- bay2[grep('.65', colnames(bay2))]
select.all <- bay2[grep('.all', colnames(bay2))]
select.night <- bay2[,69]

select.all <- cbind(select.all, select.night)

select.17.rate <- data.matrix(select.17)
dim(select.17.rate)
select.17.rate <- select.17.rate/select.17.rate[,10]
select.17.rate <- select.17.rate*100000

select.25.rate <- data.matrix(select.25)
dim(select.25.rate)
select.25.rate <- select.25.rate/select.25.rate[,26]
select.25.rate <- select.25.rate*100000

select.65.rate <- data.matrix(select.65)
dim(select.65.rate)
select.65.rate <- select.65.rate/select.65.rate[,7]
select.65.rate <- select.65.rate*100000

select.all.rate <- data.matrix(select.all)
dim(select.all.rate)
select.all.rate <- select.all.rate/select.all.rate[,42]
select.all.rate <- select.all.rate*100000

rates <- cbind(select.17.rate, select.25.rate, select.all.rate, select.65.rate)
rates <- as.data.frame(rates)
write.csv(rates, 'bay.city.rates.csv')

#================================================================================
ala <- read.csv('ala.cities.csv', header=TRUE)
ala.pop <- read.csv('ala.pop.csv', header=TRUE)

ala2 <- cbind(ala, pop)
select.17 <- ala2[grep(".17", colnames(ala2))]
select.25 <- ala2[grep('.25', colnames(ala2))]
select.65 <- ala2[grep('.65', colnames(ala2))]
select.all <- ala2[grep('.all', colnames(ala2))]
select.night <- ala2[,74]

select.all <- cbind(select.all, select.night)


select.17.rate <- data.matrix(select.17)
dim(select.17.rate)
select.17.rate <- select.17.rate/select.17.rate[,10]
select.17.rate <- select.17.rate*100000

select.25.rate <- data.matrix(select.25)
dim(select.25.rate)
select.25.rate <- select.25.rate/select.25.rate[,26]
select.25.rate <- select.25.rate*100000

select.65.rate <- data.matrix(select.65)
dim(select.65.rate)
select.65.rate <- select.65.rate/select.65.rate[,7]
select.65.rate <- select.65.rate*100000

select.all.rate <- data.matrix(select.all)
dim(select.all.rate)
select.all.rate <- select.all.rate/select.all.rate[,47]
select.all.rate <- select.all.rate*100000

rates <- cbind(select.17.rate, select.25.rate, select.all.rate, select.65.rate)
rates <- as.data.frame(rates)
write.csv(rates, 'ala.city.rates.csv')
