library(readxl)
library(sjPlot)

setwd("C:/Users/Veronica Tinney/Google Drive/EDF_shared/Results/no2/dataframes/new/all.cause/")
no2.all <- read_excel("comb.all.no2.final.xlsx")
setwd("C:/Users/Veronica Tinney/Google Drive/EDF_shared/Results/no2/dataframes/new/asthma.er/")
no2.er <- read_excel("comb.asthma.er.no2.final.xlsx")
setwd("C:/Users/Veronica Tinney/Google Drive/EDF_shared/Results/no2/dataframes/new/asthma.inc/")
no2.inc <- read_excel("comb.asthma.inc.no2.xlsx")
setwd("C:/Users/Veronica Tinney/Google Drive/EDF_shared/Results/no2/dataframes/new/cvd/")
no2.cvd <- read_excel("comb.cvd.no2.xlsx")
no2.cvd2 <- no2.cvd[,1:8]
no2.cvd3 <- no2.cvd[,10:18]
no2.cvd <- cbind(no2.cvd2, no2.cvd3)

no2.all <- as.data.frame(no2.all)
no2.er <- as.data.frame(no2.er)
no2.inc <- as.data.frame(no2.inc)
names(no2.cvd)[11] <- 'Estimate'
names(no2.er)[11] <- 'Estimate'
names(no2.inc)[11] <- 'Estimate'
names(no2.cvd)[4] <- 'Estimates'
names(no2.er)[4] <- 'Estimates'
names(no2.inc)[4] <- 'Estimates'
no2.all$type <- 'All-cause mortality'
no2.er$type <- 'Asthma ER visits'
no2.inc$type <- 'Asthma incidence'
no2.cvd$type <- 'CVD mortality'

no2 <- rbind(no2.all, no2.cvd, no2.er, no2.inc)

#===================================================================================================================
setwd("C:/Users/Veronica Tinney/Google Drive/EDF_shared/Results/no2/dataframes/new/")

names(no2)[10] <- "Concentration type"
names(no2)[13] <- "Minimum"  
names(no2)[14] <- "25th percentile"                 
names(no2)[15] <- "Median"
names(no2)[16] <- "75th percentile"
names(no2)[17] <- "Maximum"
names(no2)[18] <- "Outcome"

no2$Extent <- factor(no2$Extent,
                         levels = c("alameda","oak", "bay"),
                         labels = c("Alameda", "Oakland", "Bay Area"))

no2 <- subset(no2, Output != 'rate')

no2$Output <- factor(no2$Output,
                         levels = c('paf','wrate'),
                         labels = c('Excess health outcome','Rate per 100,000'))

no2$'Population Fraction' <- factor(no2$'Population Fraction',
                                        levels = c('ls','wp', NA),
                                        labels = c('GPW','WP'))

no2$'Population Dataset' <- factor(no2$'Population Dataset',
                                       levels = c('night','day', '2016'),
                                       labels = c('LS Night','LS Day', 'WP 2016'))

no2$Concentration <- factor(no2$Concentration,
                                levels = c('larkin','NO2'),
                                labels = c('Larkin et al. 2017', 'GSV'))

no2$Beta <- factor(no2$Beta, 
                       levels = c("atkin","eum","jerret","ore","zheng","khr"),
                       labels = c('Atkinson & Butland, 2018', 'Eum et al. 2019', 'Jerret et al. 2013',
                                  'Orellano et al. 2017', 'Zheng et al. 2015', 'Khreis et al. 2017'))

no2$'Age group' <- factor(no2$'Age group',
                              levels = c('25','65','all', '17'),
                              labels = c('25-99 years', '65-99 years', 'All ages', '0-17 years'))

no2$Estimates <- factor(no2$Estimates, 
                            levels = c('lower', 'point', 'upper'),
                            labels = c('Lower', "Point", "Upper"))

no2$Rate <- factor(no2$Rate,
                       levels <- c('co', 'cbg', 'ct', 'zip','100m'),
                       labels <- c('County', "Census Block Group", "Census tract", "Zip-code","100m"))

no2.ex <- subset(no2, Output == 'Excess health outcome')
no2.rate <- subset(no2, Output == 'Rate per 100,000')

no2.ex$Estimate <-  as.numeric(as.character(no2.ex$Estimate))
no2.rate$Estimate <-  as.numeric(as.character(no2.rate$Estimate))

no2.rate[11:17] <- no2.rate[11:17]*100000

no2.ex$Estimate <- round(no2.ex$Estimate,3)
no2.rate$Estimate <- round(no2.rate$Estimate,3)

#===========================================================================================================================
setwd("C:/Users/Veronica Tinney/Google Drive/EDF_shared/Results/no2/dataframes/new/")

extents <- c('Bay Area', 'Alameda County', 'Oakland')
outcomes <- c("All-cause mortality","CVD mortality","Asthma ER visits","Asthma incidence")


i=3
k=4

# Loop for excess outcome (sum)

for (i in 1:length(extents)){
    print(extents[i])
    for (k in 1:length(outcomes)){
      print(outcomes[k])
      x <- subset(no2.ex, Extent == extents[i])
      x <- subset(no2.ex, Outcome == outcomes[k])
      x <- subset(x[,(c(3,4:9,11,18))])
      x <- x[c("Outcome","Beta","Estimates","Rate","Age group","Population Fraction","Population Dataset","Concentration","Estimate")]
      x <- subset(x, Estimates == 'Point')
      x <- subset(x[,(c(1:2,4:9))])
      filename=paste0(extents[i],outcomes[k],'.doc',sep='')
      sink(filename)
      p <- tab_df(x, 
             title=paste0(extents[i],' excess ',outcomes[k],' due to nitrogen dioxide',sep=' '),
             alternate.rows = TRUE,
             show.rownames = FALSE,
             sort.column=3,
             show.footnote = TRUE,
             footnote='LS=LandScan USA. LS Day=LS day-time population. LS Night= LS night time population.
GPW=Gridded Population of the World v4 (2010). WP = WorldPop (2016).',
             file=filename)
      print(p)
      sink()
    }
}


for (i in 1:length(extents)){
  print(extents[i])
  for (k in 1:length(outcomes)){
    print(outcomes[k])
    x <- subset(no2.rate, Extent == extents[i])
    x <- subset(no2.rate, Outcome == outcomes[k])
    x <- subset(x[,(c(3,4:9,11,18))])
    x <- x[c("Outcome","Beta","Estimates","Rate","Age group","Population Fraction","Population Dataset","Concentration","Estimate")]
    x <- subset(x, Estimates == 'Point')
    x <- subset(x[,(c(1:2,4:9))])
    filename=paste0('Rate ',extents[i],outcomes[k],'.doc',sep='')
    sink(filename)
    p <- tab_df(x, 
                title=paste0(extents[i],' excess per 100,000 ',outcomes[k],' due to nitrogen dioxide',sep=' '),
                alternate.rows = TRUE,
                show.rownames = FALSE,
                sort.column=3,
                show.footnote = TRUE,
                footnote='LS=LandScan USA. LS Day=LS day-time population. LS Night= LS night time population.
GPW=Gridded Population of the World v4 (2010). WP = WorldPop (2016).',
                file=filename)
    print(p)
    sink()
  }
}