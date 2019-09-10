#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# ==========================================================================================================================================
# All-cause mortality, ages 25-99 years
# ==========================================================================================================================================
# ==========================================================================================================================================
# Created: 2019-09-10, V Southerland
#
# All read-in raster files are need to be rasters with the same characteristics for: 
# (nrow, ncol, ncell) - cell number
# (xmin, xmax, ymin, ymax) - extent
# (x, y) - resolution
# Coordinate reference system (crs): +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0
#==========================================================================================================================================
# Everything in this section needs to be changed specific to your run
# Specify where the input files are
# Population raster files, rate raster files, concentration raster files, shapefiles
pops <- '/GWSPH/home/vtinney/pop1/'
rates <- '/GWSPH/home/vtinney/rates1/'
concs <- '/GWSPH/home/vtinney/conc1/'
shps <- '/GWSPH/home/vtinney/clip1/'

# Change this to where you want your output maps saved
setwd('/GWSPH/home/vtinney/results2/no2/')

# Clip groups are the shapefiles that you want to clip your results to:
# Here I am clipping the results to following areas:
clip.groups <- c('alaco2', 'oak', 'eo')
names(clip.groups) <- c('Alameda County','Oakland','East Oakland')

# I also want to aggregate results to Census Block Group (CBG) and cities in the Bay area
cbg.groups <- c('bay.cbg')
city.groups <- c('baycities')

# Specify where you want the AF raster files saved (af.path), the risk files saved (mr.path) and the hia result files saved (hia.path)
af.path <- '/GWSPH/home/vtinney/results2/no2/all.cause/af/'
mr.path <- '/GWSPH/home/vtinney/results2/no2/all.cause/mr/'
hia.path <- '/GWSPH/home/vtinney/results2/no2/all.cause/paf/'

# This part needs to match the file names listed in the file directories listed above. You can give them better looking
# labels through the names function
rate.groups <- c('co.25','cbg.25')
names(rate.groups) <- c('ages 25-99 years, County baseline disease rates','ages 25-99 years, CBG baseline disease rates')
conc.groups <- c('larkin.ppb', 'NO2.ppb')
names(conc.groups) <- c('Larkin et al. 2017','GSV')
pop.groups <- c('pop.ls.night.25')
names(pop.groups) <- c('LandScan USA night time population estimates, GPWv4 age fractions')
beta.groups <- c(0.001105454,0.000318195,0.001881231)
names(beta.groups) <- c('Atkinson & Butland, 2018, point estimate','Atkinson & Butland, 2018, lower CI','Atkinson & Butland, upper CI')

# For naming files, list what you outcome and concentration are here
pollutant <- 'nitrogen dioxide'
outcome <- 'all-cause mortality'


#===================================================================================================================================
# Everything after this point should not need to be changed and will rely on the parameters you set above.
#===================================================================================================================================
#Set working directory and load files
library(raster)
library(rgdal)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(scales)
library(extrafont)
library(maptools)
loadfonts()
library(dplyr)
library(sf)
library(OpenStreetMap)
library(rJava)
library(gridExtra)
library(rgeos)
library(ggpubr)
library(spatialEco)
library(grid)
#library(GGally)
#library(purrr)
#library(tidyr)


# I want all my output maps to have the same look, so this is a ggplot theme I created
# that is then added to each map
theme_map <- function(...) {
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(hjust = 0, size=11,family="DejaVu Sans Light"),
    plot.subtitle=element_text(hjust=0, size=9,family="DejaVu Sans Light"),
    plot.caption = element_text(hjust=0, size=8,family="DejaVu Sans Light"),
    legend.title=element_text(size=11, family="DejaVu Sans Light"),
    legend.text=element_text(size=11, family="DejaVu Sans Light"),
    axis.title=element_blank(),
    legend.position = 'bottom',
    legend.justification='center',
    legend.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
    panel.border = element_rect(colour = "black", fill=NA, size=0.5),
    rect = element_blank())
}
#///////////////////////////////////////////////////////////////////////////////////////////////////
# Two functions used in the run - a zonal statistics function and aggregate to the shapefiles of 
# interest, in this case, cities and CBGs

# Functions
myZonal <- function (x, z, stat, digits = 0, na.rm = TRUE, 
                     ...) {
  library(data.table)
  fun <- match.fun(stat) 
  vals <- getValues(x) 
  zones <- round(getValues(z), digits = digits) 
  rDT <- data.table(vals, z=zones) 
  setkey(rDT, z) 
  rDT[, lapply(.SD, fun, na.rm = TRUE), by=z] 
} 

ZonalPipe<- function (zone.in, raster.in, shp.out=NULL, stat){
  require(raster)
  require(rgdal)
  require(plyr)
  
  # Load raster
  r <- raster.in
  # Load zone shapefile
  shp <- zone.in
  # Project 'zone' shapefile into the same coordinate system than the input raster
  shp <- spTransform(shp, crs(r))
  
  # Add ID field to Shapefile
  shp@data$ID<-c(1:length(shp@data[,1]))
  
  # Crop raster to 'zone' shapefile extent
  r <- crop(r, extent(shp))	
  # Rasterize shapefile
  zone <- rasterize(shp, r, field="ID", dataType = "INT1U") # Change dataType if nrow(shp) > 255 to INT2U or INT4U
  
  # Zonal stats
  Zstat<-data.frame(myZonal(r, zone, stat))
  colnames(Zstat)<-c("ID", paste0(names(r), "_", c(1:(length(Zstat)-1)), "_",stat))
  
  # Merge data in the shapefile and write it
  shp@data <- plyr::join(shp@data, Zstat, by="ID")
  
  if (is.null(shp.out)){
    return(shp)
  }else{
    writeOGR(shp, shp.out, layer= sub("^([^.]*).*", "\\1", basename(zone.in)), driver="ESRI Shapefile")
  }
}

pdf(NULL)

# Loop in concentrations
for (j in 1:length(conc.groups)){
  print(conc.groups[j])
  a = raster(paste(concs,'conc.',conc.groups[j],'.tif',sep=''))
  a[a==0]<-NA
  
  # Loop in betas
  for (i in 1:length(beta.groups)){
    print(beta.groups[i])
    
    af <- 1-exp(-beta.groups[i]*a) #Create attributable fraction
    
    af2 <- af #duplicate for later use
    af <- af*100 # multiply by 100 for easier mapping
    
    # Loop in rate groups
    for (k in 1:length(rate.groups)){
      print(rate.groups[k])
      b = raster(paste(rates,'mortality.',rate.groups[k],'.tif',sep=''))	
      
      mr <- af2*b #mr = mortality risk - this is just the af* the baseline rates (so the whole calculation without the population data)
      mr[mr==0]<-NA
      
      #Loop in population
      for (h in 1:length(pop.groups)){
        print(pop.groups[h])
        c = raster(paste(pops,pop.groups[h],'.tif',sep=''))
        c[c==0]<- NA
        
        #The actual HIA calculation using the raster overlay function. In order to use this, all input files must be on the same extent, resolution and coordinate reference system.
        hia = overlay(c, b, a, fun=function(r1, r2, r3){return(r1*r2*(10^-4)*(1-exp(-beta.groups[i]*r3)))})
        hia[hia==0]<-NA
        
        
        # Now all the calculations are done, so I will make a series of maps. There are way more than you will need, so you can use the same general techniques to make your own.
        # First, the loop reads in the shape file. 
        for (m in 1:length(clip.groups)){
          print(clip.groups[m])
          shp <- readOGR(dsn=shps, layer=paste(clip.groups[m])) #read in the shape file
          crs(shp) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" #change coordinate reference to WGS84
          
          #Fortify shapefile for use in ggplot
          shp.f <- fortify(shp) %>% 
            mutate(id = as.numeric(id))
          
          #Crop and mask. Crop crops the shapefile to the extent, and mask masks all values. Look at the raster package documentation for more info, but this is important to do in this order.
          af <- crop(af, shp)
          af <- mask(af, shp)
          
          # Get cell statistics for the attributable fraction
          af.iqr <- quantile(af)
          af.iqr <- as.matrix(af.iqr)
          af.iqr <- t(af.iqr)
          af.mean <- cellStats(af, 'mean')
          af.df <- cbind(af.mean, af.iqr)
          
          # Print statements are good to put throughout the loop so that when it fails, you know more specifically where it went wrong.
          print(paste(names(clip.groups[m]),', attributable fraction, ',names(conc.groups[j]),', ',names(beta.groups[i]),sep=''))
          print(af.df)
          
          # Write the cropped file to a new file and save.
          f1 = paste(af.path,names(clip.groups[m]),' ',names(beta.groups[i]),' ',names(conc.groups[j]),'.tif',sep='')
          
          writeRaster(af, filename=f1, format="GTiff", overwrite=TRUE)
          
          # Cell cell statistics for mapping
          min.af <- minValue(af)
          max.af <- maxValue(af)
          min.af.label <- round(minValue(af),2)
          max.af.label <- round(maxValue(af),2)
          mean.af <- (min.af+max.af)/2
          mean.af.label <- round(mean.af,2)
          
          # Convert to a log scale
          af.log <- log(af)
          z.af <- scale(af.log)
          
          # Raster to point is what you need to do in order to map easier and faster in ggplot
          
          af.df <- rasterToPoints(af)
          af.df <- data.frame(af.df)
          colnames(af.df) <- c('lon','lat','val')
          
          z.af.df <- rasterToPoints(z.af)
          z.af.df <- data.frame(z.af.df)
          colnames(z.af.df) <- c('lon','lat','val')
          z.af.df$val <- rescale(z.af.df$val, to=c(-3,3))
          
          
          # Base map for the maps. This takes the extent of the shape file in the current loop.
          base <- openmap(c(ymin(shp),xmin(shp)),c(ymax(shp),xmax(shp)),
                          type = "esri-topo",
                          mergeTiles = TRUE)
          base <- openproj(base, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # does not default to WGS84 so you have to change it
          
          # Map of attributable risk
          autoplot(base)  +
            geom_polygon(data = shp.f, aes(x = long, y = lat, group = group), 
                         fill="grey50",alpha=0.5)+
            geom_tile(data=af.df,aes(lon, lat, fill = val),alpha=0.8) +
            scale_fill_gradient2("Attributable Fraction (%)",
                                 low = "#3ec267", 
                                 mid = "#fff429",  #ff7e29
                                 high = "#fc0339", ##ff1f40
                                 midpoint = mean.af,
                                 breaks=c(min.af,mean.af,max.af),
                                 labels=c(min.af.label,mean.af.label,max.af.label),
                                 limits=c(min.af, max.af),
                                 na.value = 'grey50',
                                 guide = guide_colourbar(
                                   direction = "horizontal",
                                   label=TRUE,
                                   keyheight = unit(2, units = "mm"),
                                   title.position = 'top',
                                   title.hjust = 0.5,
                                   label.hjust = 0.5,
                                   barwidth = 15,
                                   nrow = 1,
                                   byrow = T,
                                   label.position = "bottom"))+
            theme_map()+
            geom_path(data = shp.f, aes(x = long, y = lat, group = group), 
                      color = "grey60", size = 0.5)+
            labs(title='Attributable fraction',
                 caption=paste0(names(beta.groups[i]),', ',names(conc.groups[j]),'.',sep=''),
                 subtitle=paste0('Range: ',min.af.label,' to ',max.af.label,'%.'),sep='')
          ggsave(paste0(names(clip.groups[m]),' AF ',outcome,' ',pollutant,' ',names(beta.groups[i]),' ',names(conc.groups[j]),'.af.png',sep=''),dpi=300)
          print('af')
          
          # Map of z-score of attributable risk
          autoplot(base)  +
            geom_polygon(data = shp.f, aes(x = long, y = lat, group = group), 
                         fill="grey50",alpha=0.5)+
            geom_tile(data=z.af.df,aes(lon, lat, fill = val),alpha=0.8) +
            scale_fill_gradient2("Attributable Fraction \n log z-score",
                                 low = "blue", #scales::muted()
                                 high = "red",
                                 midpoint = 0,
                                 na.value = 'grey50',
                                 guide = guide_colourbar(
                                   direction = "horizontal",
                                   label=TRUE,
                                   keyheight = unit(2, units = "mm"),
                                   title.position = 'top',
                                   title.hjust = 0.5,
                                   label.hjust = 0.5,
                                   barwidth = 15,
                                   nrow = 1,
                                   byrow = T,
                                   label.position = "bottom"))+
            theme_map()+
            geom_path(data = shp.f, aes(x = long, y = lat, group = group), 
                      color = "grey60", size = 0.5)+
            labs(title='Attributable fraction',
                 caption=paste0(names(beta.groups[i]),', ',names(conc.groups[j]),'.',sep=''),
                 subtitle=paste0(''),sep='')
          ggsave(paste0(names(clip.groups[m]),' AF ',outcome,' ',pollutant,' ',names(beta.groups[i]),' ',names(conc.groups[j]),'.z.png',sep=''),dpi=300)
          print('af.z')
          
          #==============================================================================================================================================================    
          
          # Now we do the same thing with mortality risk
          mr <- crop(mr, shp)
          mr <- mask(mr, shp)
          
          #Cell stats sink to csv
          mr.iqr <- quantile(mr)
          mr.iqr <- as.matrix(mr.iqr)
          mr.iqr <- t(mr.iqr)
          mr.mean <- cellStats(mr, 'mean')
          df.mr <- cbind(mr.mean, mr.iqr)
          print(paste(names(clip.groups[m]),' mortality risk ',names(conc.groups[j]),' ',names(beta.groups[i]),' ',names(rate.groups[k]),sep=''))
          print(df.mr)
          
          f2 = paste(mr.path,names(clip.groups[m]),' ',names(beta.groups[i]),' ',conc.groups[j],' ',names(rate.groups[k]),'.tif',sep='')
          
          writeRaster(mr, filename=f2, format="GTiff", overwrite=TRUE)
          
          
          mr.log <- log(mr)
          z.mr <- scale(mr.log)
          
          mr.df <- rasterToPoints(mr)
          mr.df <- data.frame(mr.df)
          colnames(mr.df) <- c('lon','lat','val')
          
          z.mr.df <- rasterToPoints(z.mr)
          z.mr.df <- data.frame(z.mr.df)
          colnames(z.mr.df) <- c('lon','lat','val')
          z.mr.df$val <- rescale(z.mr.df$val, to=c(-3,3))
          
          er.df <- rasterToPoints(er)
          er.df <- data.frame(er.df)
          colnames(er.df) <- c('lon','lat','val') 
          
          er.med.df <- rasterToPoints(er.med)
          er.med.df <- data.frame(er.med.df)
          colnames(er.med.df) <- c('lon','lat','val') 
          
          
          autoplot(base)  +
            geom_polygon(data = shp.f, aes(x = long, y = lat, group = group), 
                         fill="grey50",alpha=0.5)+
            geom_tile(data=mr.df,aes(lon, lat, fill = val),alpha=0.8) +
            scale_fill_gradient2("Mortality risk per 10,000",
                                 low = "#3ec267", 
                                 mid = "#fff429",  #ff7e29
                                 high = "#fc0339", ##ff1f40
                                 midpoint = mean.mr,
                                 breaks=c(min.mr,mean.mr,max.mr),
                                 labels=c(min.mr.label,mean.mr.label,max.mr.label),
                                 limits=c(min.mr, max.mr),
                                 na.value = 'grey50',
                                 guide = guide_colourbar(
                                   direction = "horizontal",
                                   label=TRUE,
                                   keyheight = unit(2, units = "mm"),
                                   title.position = 'top',
                                   title.hjust = 0.5,
                                   label.hjust = 0.5,
                                   barwidth = 15,
                                   nrow = 1,
                                   byrow = T,
                                   label.position = "bottom"))+
            theme_map()+
            geom_path(data = shp.f, aes(x = long, y = lat, group = group), 
                      color = "grey60", size = 0.5)+
            labs(title='Mortality risk',
                 caption=paste0(names(beta.groups[i]),', ',names(conc.groups[j]),', ',names(rate.groups[k]),'.',sep=''),
                 subtitle=paste0('Range: ',min.mr.label,' to ',max.mr.label,' per 10,000. '),sep='')
          ggsave(paste0(names(clip.groups[m]),' Risk ',outcome,' ',pollutant,' ',names(beta.groups[i]),' ',names(conc.groups[j]),' ',names(rate.groups[k]),'.af.png',sep=''),dpi=300)
          print('mr')
          
          autoplot(base)  +
            geom_polygon(data = shp.f, aes(x = long, y = lat, group = group), 
                         fill="grey50",alpha=0.5)+
            geom_tile(data=z.mr.df,aes(lon, lat, fill = val),alpha=0.8) +
            scale_fill_gradient2("Mortality risk log z-score",
                                 low = "blue", #scales::muted()
                                 high = "red",
                                 midpoint = 0,
                                 na.value = 'grey50',
                                 guide = guide_colourbar(
                                   direction = "horizontal",
                                   label=TRUE,
                                   keyheight = unit(2, units = "mm"),
                                   title.position = 'top',
                                   title.hjust = 0.5,
                                   label.hjust = 0.5,
                                   barwidth = 15,
                                   nrow = 1,
                                   byrow = T,
                                   label.position = "bottom"))+
            theme_map()+
            geom_path(data = shp.f, aes(x = long, y = lat, group = group), 
                      color = "grey60", size = 0.5)+
            labs(title='Mortality risk',
                 caption=paste0(names(beta.groups[i]),', ',names(conc.groups[j]),', ',names(rate.groups[k]),'.',sep=''),
                 subtitle=paste0(''),sep='')
          ggsave(paste0(names(clip.groups[m]),' Risk ',outcome,' ',pollutant,' ',names(beta.groups[i]),' ',names(conc.groups[j]),' ',names(rate.groups[k]),'.z.png',sep=''),dpi=300)
          print('mr.z')
          
          #===========================================================================================================================      
          # Again, all the same maps, but for the excess cases results
          
          hia <- crop(hia, shp)
          hia <- mask(hia, shp)
          
          
          #Cell stats sink to csv
          hia.iqr <- quantile(hia)
          hia.iqr <- as.matrix(hia.iqr)
          hia.iqr <- t(hia.iqr)
          hia.mean <- cellStats(hia, 'mean')
          hia.sum <- cellStats(hia,'sum')
          df.hia <- cbind(hia.sum,hia.mean, hia.iqr)
          print(paste(names(clip.groups[m]),' excess mortality ',names(conc.groups[j]),' ',names(beta.groups[i]),' ',names(pop.groups[h]),' ',names(rate.groups[k]),sep=''))
          print(df.hia)
          
          f3 = paste(hia.path,names(clip.groups[m]),' ',names(beta.groups[i]),' ',names(rate.groups[k]),' ',names(pop.groups[h]),' ',names(conc.groups[j]),'.tif',sep='')
          writeRaster(hia, filename=f3, format="GTiff", overwrite=TRUE)
          
          hia.df <- rasterToPoints(hia)
          hia.df <- data.frame(hia.df)
          colnames(hia.df) <- c('lon','lat','val')
          
          # Create maps mortality risk 
          min.hia <- minValue(hia)
          max.hia <- maxValue(hia)
          min.hia.label <- round(minValue(hia),2)
          max.hia.label <- round(maxValue(hia),2)
          mean.hia <- (min.hia+max.hia)/2
          mean.hia.label <- round(mean.hia,2)
          
          hia.log <- log(hia)
          z.hia <- scale(hia.log)
          
          z.hia.df <- rasterToPoints(z.hia)
          z.hia.df <- data.frame(z.hia.df)
          colnames(z.hia.df) <- c('lon','lat','val')
          z.hia.df$val <- rescale(z.hia.df$val, to=c(-3,3))
          
          er.df <- rasterToPoints(er)
          er.df <- data.frame(er.df)
          colnames(er.df) <- c('lon','lat','val') 
          
          autoplot(base)  +
            geom_polygon(data = shp.f, aes(x = long, y = lat, group = group), 
                         fill="grey50",alpha=0.5)+
            geom_tile(data=hia.df,aes(lon, lat, fill = val),alpha=0.8) +
            scale_fill_gradient2("Excess cases (n)",
                                 low = "#3ec267", 
                                 mid = "#fff429",  #ff7e29
                                 high = "#fc0339", ##ff1f40
                                 midpoint = mean.hia,
                                 breaks=c(min.hia,mean.hia,max.hia),
                                 labels=c(min.hia.label,mean.hia.label,max.hia.label),
                                 limits=c(min.hia, max.hia),
                                 na.value = 'grey50',
                                 guide = guide_colourbar(
                                   direction = "horizontal",
                                   label=TRUE,
                                   keyheight = unit(2, units = "mm"),
                                   title.position = 'top',
                                   title.hjust = 0.5,
                                   label.hjust = 0.5,
                                   barwidth = 15,
                                   nrow = 1,
                                   byrow = T,
                                   label.position = "bottom"))+
            theme_map()+
            geom_path(data = shp.f, aes(x = long, y = lat, group = group), 
                      color = "grey60", size = 0.5)+
            labs(title=paste0('Excess ',outcome,' attributable to ',pollutant,'.',sep=''),
                 caption=paste0(names(beta.groups[i]),', ',names(conc.groups[j]),', ',names(rate.groups[k]),',\n',names(pop.groups[h]),'.',sep=''),
                 subtitle=paste0('Range: ',min.hia.label,' to ',max.hia.label,' per grid cell. '),sep='')
          ggsave(paste0(names(clip.groups[m]),' PAF ',outcome,' ',pollutant,' ',names(beta.groups[i]),' ',names(conc.groups[j]),' ',names(pop.groups[h]),' ',names(rate.groups[k]),'.af.png',sep=''),dpi=300)
          print('hia')
          
          autoplot(base)  +
            geom_polygon(data = shp.f, aes(x = long, y = lat, group = group), 
                         fill="grey50",alpha=0.5)+
            geom_tile(data=z.hia.df,aes(lon, lat, fill = val),alpha=0.8) +
            scale_fill_gradient2("Excess mortality log z-score",
                                 low = "blue", #scales::muted()
                                 high = "red",
                                 midpoint = 0,
                                 na.value = 'grey50',
                                 guide = guide_colourbar(
                                   direction = "horizontal",
                                   label=TRUE,
                                   keyheight = unit(2, units = "mm"),
                                   title.position = 'top',
                                   title.hjust = 0.5,
                                   label.hjust = 0.5,
                                   barwidth = 15,
                                   nrow = 1,
                                   byrow = T,
                                   label.position = "bottom"))+
            theme_map()+
            geom_path(data = shp.f, aes(x = long, y = lat, group = group), 
                      color = "grey60", size = 0.5)+
            labs(title=paste0('Excess ',outcome,' attributable to ',pollutant,'.',sep=''),
                 caption=paste0(names(beta.groups[i]),', ',names(conc.groups[j]),', ',names(rate.groups[k]),',\n',names(pop.groups[h]),'.',sep=''),
                 subtitle=paste0(''),sep='')
          ggsave(paste0(names(clip.groups[m]),' PAF ',outcome,' ',pollutant,' ',names(beta.groups[i]),' ',names(conc.groups[j]),' ',names(pop.groups[h]),' ',names(rate.groups[k]),'.z.png',sep=''),dpi=300)
          print('hia.z')
          
          
          #=========================================================================================================================
          
          # Here we start zonal statistics, aggregating the results of the HIA to census block groups first
          for (s in 1:length(cbg.groups)){
            print(cbg.groups[s])
            cbg.shp <- readOGR(dsn=shps, layer=paste(cbg.groups[s]))
            crs(cbg.shp) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
            
            cbg.shp <- crop(cbg.shp, shp)
            
            cbg.shp.f <- fortify(cbg.shp) %>% 
              mutate(id = as.numeric(id))
            
            zone.in <- cbg.shp
            raster.in <- hia
            
            
            # This part uses the function I created above to aggregate and create a new dataframe of excess cases summed to CBG            
            shp2 <- ZonalPipe(zone.in, raster.in, stat="sum")
            shp2@data <- shp2@data %>% mutate(id = row.names(.))
            shp_df <- fortify(shp2, region = "id")
            shp_df <- shp_df %>% left_join(shp2@data, by = c("id"="id"))
            shp_df <- as.data.frame(shp_df)
            shp_df[,ncol(shp_df)][shp_df[,ncol(shp_df)] == 0] <- NA
            r.min <- min(shp_df[,ncol(shp_df)],na.rm=TRUE)
            r.max <- max(shp_df[,ncol(shp_df)],na.rm=TRUE)
            r.med <- median(shp_df[,ncol(shp_df)],na.rm=TRUE)
            colnames(shp_df)[ncol(shp_df)] <- 'hia.val'
            
            r.mean <- (r.min+r.max)/2
            r.mean.label <- round(r.mean,2)
            r.min.label <- round(r.min,2)
            r.max.label <- round(r.max,2)
            r.med.label <- round(r.med,2)
            
            zone.in <- cbg.shp 
            raster.in <- c
            
            shp3 <- ZonalPipe(zone.in, raster.in, stat="sum")
            shp3@data <- shp3@data %>% mutate(id = row.names(.))
            pop_df <- fortify(shp3, region = "id")
            pop_df <- pop_df %>% left_join(shp3@data, by = c("id"="id"))
            pop_df <- as.data.frame(pop_df)
            pop_df[,ncol(pop_df)][pop_df[,ncol(pop_df)] == 0] <- NA
            colnames(pop_df)[ncol(pop_df)] <- "pop.val"
            
            rate_df <- merge(shp_df,pop_df,by='order')
            rate_df <- as.data.frame(rate_df)
            rate_df$rate <- NA
            rate_df$rate <- (rate_df$hia.val*100000)/rate_df$pop.val
            #rate_df$rate[rate_df$rate==0]<-NA
            rate.min <- min(rate_df[,ncol(rate_df)],na.rm=TRUE)
            rate.max <- max(rate_df[,ncol(rate_df)],na.rm=TRUE)
            rate.med <- median(rate_df[,ncol(rate_df)],na.rm=TRUE)
            rate.min.label <- round(rate.min,2)
            rate.max.label <- round(rate.max,2)
            rate.med.label <- round(rate.med,2)
            rate.mean <- (rate.min+rate.max)/2
            rate.mean.label <- round(rate.mean,2)
            
            # Write out the results to a CSV file so you can make tables later
            write.csv(rate_df, paste(names(clip.groups[m]),' ',names(beta.groups[i]),' ',names(conc.groups[j]),' ',names(pop.groups[h]),' ',names(rate.groups[k]),'cbg.results.csv'))
            
            
            # Log z-score
            # Log z-score
            z_df <- shp_df
            z_df$log.z <- shp_df[,ncol(shp_df)]
            z_df$log.z[z_df$log.z == 0] <- NA
            z_df$log.z <- log(z_df$log.z)
            z_df$log.z <- scale(z_df$log.z)
            z_df$log.z <- rescale(z_df$log.z, to=c(-3,3))
            
            # Map of excess per grid cell
            e <- autoplot(base)  +
              geom_polygon(data = shp_df, aes(x = long, y = lat, group = group, fill = shp_df[,ncol(shp_df)]),alpha=0.7)+
              scale_fill_gradient2("Count (n) cases \n per Census Block Group",
                                   low = "#3ec267", 
                                   mid = "#fff429",  #ff7e29
                                   high = "#fc0339", ##ff1f40
                                   midpoint = r.mean,
                                   na.value='grey50',
                                   breaks=c(r.min,r.mean,r.max),
                                   labels=c(r.min.label,r.mean.label,r.max.label),
                                   limits=c(r.min, r.max),
                                   guide = guide_colourbar(
                                     direction = "horizontal",
                                     label=TRUE,
                                     keyheight = unit(2, units = "mm"),
                                     title.position = 'top',
                                     title.hjust = 0.5,
                                     label.hjust = 0.5,
                                     barwidth = 15,
                                     nrow = 1,
                                     byrow = T,
                                     label.position = "bottom"))+
              theme_map()+
              geom_path(data = cbg.shp.f, aes(x = long, y = lat, group = group), 
                        color = "grey60", size = 0.1)+
              labs(title=paste0('Excess ',outcome,' attributable to ',pollutant,'.',sep=''),
                   caption=paste0(names(beta.groups[i]),', ',names(conc.groups[j]),', ',names(rate.groups[k]),', \n',names(pop.groups[h]),'.',sep=''),
                   subtitle=paste0('Range: ',r.min.label,' to ',r.max.label,' per CBG. '),sep='')
            ggsave(paste0(names(clip.groups[m]),' PAF ',outcome,' ',pollutant,' ',names(beta.groups[i]),' ',names(conc.groups[j]),' ',names(pop.groups[h]),' ',names(rate.groups[k]),'.count.cbg.png',sep=''),dpi=300)
            print('count.cbg')
            
            # Map of rate per 100,000
            autoplot(base)  +
              geom_polygon(data = rate_df, aes(x = long.x, y = lat.x, group = group.x, fill = rate_df$rate),alpha=0.7)+
              scale_fill_gradient2("Rate per 100,000\nper Census Block Group",
                                   low = "#3ec267", 
                                   mid = "#fff429",  #ff7e29
                                   high = "#fc0339", ##ff1f40
                                   midpoint = rate.mean,
                                   na.value='grey50',
                                   breaks=c(rate.min,rate.mean,rate.max),
                                   labels=c(rate.min.label,rate.mean.label,rate.max.label),
                                   limits=c(rate.min, rate.max),
                                   guide = guide_colourbar(
                                     direction = "horizontal",
                                     label=TRUE,
                                     keyheight = unit(2, units = "mm"),
                                     title.position = 'top',
                                     title.hjust = 0.5,
                                     label.hjust = 0.5,
                                     barwidth = 15,
                                     nrow = 1,
                                     byrow = T,
                                     label.position = "bottom"))+
              theme_map()+
              geom_path(data = cbg.shp.f, aes(x = long, y = lat, group = group), 
                        color = "grey60", size = 0.1)+
              labs(title=paste0('Excess ',outcome,' attributable to ',pollutant,'.',sep=''),
                   caption=paste0(names(beta.groups[i]),', ',names(conc.groups[j]),', ',names(rate.groups[k]),', \n',names(pop.groups[h]),'.',sep=''),
                   subtitle=paste0('Range: ',rate.min.label,' to ',rate.max.label,' per 100,000. '),sep='')
            ggsave(paste0(names(clip.groups[m]),' PAF ',outcome,' ',pollutant,' ',names(beta.groups[i]),' ',names(conc.groups[j]),' ',names(pop.groups[h]),' ',names(rate.groups[k]),'.rate.cbg.png',sep=''),dpi=300)
            print('rate.cbg')
            
            
            #/////////////////////////////////////////////////////////////////////////////////////////////
            # City aggregation
            # Now we do the same thing but with cities
            
            for (q in 1:length(city.groups)){
              print(city.groups[q])
              
              city.shp <- readOGR(dsn=shps, layer=paste(city.groups[q]))
              crs(city.shp) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
              
              city.shp <- crop(city.shp, shp)
              
              city.shp.f <- fortify(city.shp) %>% 
                mutate(id = as.numeric(id))
              
              zone.in <- city.shp
              raster.in <- hia
              
              shp2 <- ZonalPipe(zone.in, raster.in, stat="sum")
              shp2@data <- shp2@data %>% mutate(id = row.names(.))
              shp_df <- fortify(shp2, region = "id")
              shp_df <- shp_df %>% left_join(shp2@data, by = c("id"="id"))
              shp_df <- as.data.frame(shp_df)
              shp_df[,ncol(shp_df)][shp_df[,ncol(shp_df)] == 0] <- NA
              r.min <- min(shp_df[,ncol(shp_df)],na.rm=TRUE)
              r.max <- max(shp_df[,ncol(shp_df)],na.rm=TRUE)
              r.med <- median(shp_df[,ncol(shp_df)],na.rm=TRUE)
              colnames(shp_df)[ncol(shp_df)] <- 'hia.val'
              
              r.mean <- (r.min+r.max)/2
              r.mean.label <- round(r.mean,2)
              r.min.label <- round(r.min,2)
              r.max.label <- round(r.max,2)
              r.med.label <- round(r.med,2)
              
              zone.in <- city.shp
              raster.in <- c
              
              shp3 <- ZonalPipe(zone.in, raster.in, stat="sum")
              shp3@data <- shp3@data %>% mutate(id = row.names(.))
              pop_df <- fortify(shp3, region = "id")
              pop_df <- pop_df %>% left_join(shp3@data, by = c("id"="id"))
              pop_df <- as.data.frame(pop_df)
              pop_df[,ncol(pop_df)][pop_df[,ncol(pop_df)] == 0] <- NA
              colnames(pop_df)[ncol(pop_df)] <- "pop.val"
              
              rate_df <- merge(shp_df,pop_df,by='order')
              rate_df <- as.data.frame(rate_df)
              rate_df$rate <- NA
              rate_df$rate <- (rate_df$hia.val*100000)/rate_df$pop.val
              #rate_df$rate[rate_df$rate==0]<-NA
              rate.min <- min(rate_df[,ncol(rate_df)],na.rm=TRUE)
              rate.max <- max(rate_df[,ncol(rate_df)],na.rm=TRUE)
              rate.med <- median(rate_df[,ncol(rate_df)],na.rm=TRUE)
              rate.min.label <- round(rate.min,2)
              rate.max.label <- round(rate.max,2)
              rate.med.label <- round(rate.med,2)
              rate.mean <- (rate.min+rate.max)/2
              rate.mean.label <- round(rate.mean,2)
              
              write.csv(rate_df, paste(names(clip.groups[m]),' ',names(beta.groups[i]),' ',names(conc.groups[j]),' ',names(pop.groups[h]),' ',names(rate.groups[k]),'city.results.csv'))
              
              # Log z-score
              # Log z-score
              z_df <- shp_df
              z_df$log.z <- shp_df[,ncol(shp_df)]
              z_df$log.z[z_df$log.z == 0] <- NA
              z_df$log.z <- log(z_df$log.z)
              z_df$log.z <- scale(z_df$log.z)
              z_df$log.z <- rescale(z_df$log.z, to=c(-3,3))
              
              # Map of excess per grid cell
              autoplot(base)  +
                geom_polygon(data = shp_df, aes(x = long, y = lat, group = group, fill = shp_df[,ncol(shp_df)]),alpha=0.7)+
                scale_fill_gradient2("Count (n) cases \n per city",
                                     low = "#3ec267", 
                                     mid = "#fff429",  #ff7e29
                                     high = "#fc0339", ##ff1f40
                                     midpoint = r.mean,
                                     na.value='grey50',
                                     breaks=c(r.min,r.mean,r.max),
                                     labels=c(r.min.label,r.mean.label,r.max.label),
                                     limits=c(r.min, r.max),
                                     guide = guide_colourbar(
                                       direction = "horizontal",
                                       label=TRUE,
                                       keyheight = unit(2, units = "mm"),
                                       title.position = 'top',
                                       title.hjust = 0.5,
                                       label.hjust = 0.5,
                                       barwidth = 15,
                                       nrow = 1,
                                       byrow = T,
                                       label.position = "bottom"))+
                theme_map()+
                geom_path(data = city.shp.f, aes(x = long, y = lat, group = group), 
                          color = "grey60", size = 0.1)+
                labs(title=paste0('Excess ',outcome,' attributable to ',pollutant,'.',sep=''),
                     caption=paste0(names(beta.groups[i]),', ',names(conc.groups[j]),', ',names(rate.groups[k]),', \n',names(pop.groups[h]),'.',sep=''),
                     subtitle=paste0('Range: ',r.min.label,' to ',r.max.label,' per city. '),sep='')
              ggsave(paste0(names(clip.groups[m]),' PAF ',outcome,' ',pollutant,' ',names(beta.groups[i]),' ',names(conc.groups[j]),' ',names(pop.groups[h]),' ',names(rate.groups[k]),'.count.city.png',sep=''),dpi=300)
              print('count.city')
              
              autoplot(base)  +
                geom_polygon(data = rate_df, aes(x = long.x, y = lat.x, group = group.x, fill = rate_df$rate),alpha=0.7)+
                scale_fill_gradient2("Rate per 100,000 \n per city",
                                     low = "#3ec267", 
                                     mid = "#fff429",  #ff7e29
                                     high = "#fc0339", ##ff1f40
                                     midpoint = rate.mean,
                                     na.value='grey50',
                                     breaks=c(rate.min,rate.mean,rate.max),
                                     labels=c(rate.min.label,rate.mean.label,rate.max.label),
                                     limits=c(rate.min, rate.max),
                                     guide = guide_colourbar(
                                       direction = "horizontal",
                                       label=TRUE,
                                       keyheight = unit(2, units = "mm"),
                                       title.position = 'top',
                                       title.hjust = 0.5,
                                       label.hjust = 0.5,
                                       barwidth = 15,
                                       nrow = 1,
                                       byrow = T,
                                       label.position = "bottom"))+
                theme_map()+
                geom_path(data = city.shp.f, aes(x = long, y = lat, group = group), 
                          color = "grey60", size = 0.1)+
                labs(title=paste0('Excess ',outcome,' attributable to ',pollutant,'.',sep=''),
                     caption=paste0(names(beta.groups[i]),', ',names(conc.groups[j]),', ',names(rate.groups[k]),', \n',names(pop.groups[h]),'.',sep=''),
                     subtitle=paste0('Range: ',rate.min.label,' to ',rate.max.label,' per 100,000. '),sep='')
              ggsave(paste0(names(clip.groups[m]),' PAF ',outcome,' ',pollutant,' ',names(beta.groups[i]),' ',names(conc.groups[j]),' ',names(pop.groups[h]),' ',names(rate.groups[k]),'.rate.city.png',sep=''),dpi=300)
              print('rate.city')
              
              
              autoplot(base)  +
                geom_polygon(data = z_df, aes(x = long, y = lat, group = group, fill = z_df[,ncol(z_df)]),alpha=0.7)+
                scale_fill_gradient2("Excess cases per city \n log z-score",
                                     low = "blue", #scales::muted()
                                     high = "red",
                                     midpoint = 0,
                                     na.value='grey50',
                                     guide = guide_colourbar(
                                       direction = "horizontal",
                                       label=TRUE,
                                       keyheight = unit(2, units = "mm"),
                                       title.position = 'top',
                                       title.hjust = 0.5,
                                       label.hjust = 0.5,
                                       barwidth = 15,
                                       nrow = 1,
                                       byrow = T,
                                       label.position = "bottom"))+
                theme_map()+
                geom_path(data = city.shp.f, aes(x = long, y = lat, group = group), 
                          color = "white", size = 0.1)+
                labs(title=paste0('Excess ',outcome,' attributable to ',pollutant,'.',sep=''),
                     caption=paste0(names(beta.groups[i]),', ',names(conc.groups[j]),', ',names(rate.groups[k]),',\n',names(pop.groups[h]),'.',sep=''),
                     subtitle=paste0(''),sep='')
              ggsave(paste0(names(clip.groups[m]),' PAF ',outcome,' ',pollutant,' ',names(beta.groups[i]),' ',names(conc.groups[j]),' ',names(pop.groups[h]),' ',names(rate.groups[k]),'.z.city.png',sep=''),dpi=300)
              print('z.city')
              
              
              # It is important to remove the files you created in the order that you created them in the loop. If you don't the results will be incorrect.
            }
          }
        }
        rm(c)
        rm(hia)
      }
      rm(b)
      rm(mr)
    }
    rm(af)
    rm(af2)
  }
  rm(a)
}     
