# This is a script to perform HIA for the Bay area - NO2 and all-cause mortality
# Created: 2019-05-17
#
# All read-in files are rasters with the following characteristics: 
# 2367, 2909, 6885603  (nrow, ncol, ncell)
# -123.6325, -121.2083, 36.8925, 38.865  (xmin, xmax, ymin, ymax)
# 0.0008333333, 0.0008333333  (x, y)
# CRS: +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0
# 
#==================================================================
#Set working directory and load files
setwd('/home/vtinney/run/')
library(raster)
library(sp)
library(rgdal)
# ==================================================================
# Read in shapefiles for cropping full bay extent results
setwd('/home/vtinney/run/clip/')
oak <- readOGR(dsn=getwd(), layer='LandScangrid_AQ2')
ala <- readOGR(dsn=getwd(), layer='alameda')
# ==================================================================
# Specify where the files are
pops <- '/home/vtinney/run/pop/'
rates <- '/home/vtinney/run/rates/'
concs <- '/home/vtinney/run/conc/'
betas <- '/home/vtinney/run/betas/'
popfracs <- '/home/vtinney/run/pop/'
# ==================================================================
# Adult betas 
#beta.atkin.ug <- 0.002078254
#atkin.lower.ug <- 0.000598207
#atkin.upper.ug <- 0.003536714

#Elderly betas
#beta.eum <- 0.001209547		
#eum.lower.ug <- 0.001105454
#eum.upper.ug <- 0.001365306
# ==================================================================
# Adult calc set up
rate.groups <- c('cbg.25', 'ct.25', 'co.25')
conc.groups <- c('larkin.ug', 'NO2')
pop.groups <- c('ls.day.25', 'wp.25', 'ls.night.25', 'wp.night.25')
beta.groups <- c('atkin.ug', 'atkin.lower.ug', 'atkin.upper.ug')
popfrac.groups <- c('ls.day.25', 'wp.25', 'ls.night.25', 'wp.night.25')

# ==================================================================
# Loop Adults for PAF and AF

for (i in 1:length(beta.groups)){
print(beta.groups[i])
n = raster(paste(betas,'beta.',beta.groups[i],'.tif',sep=''))	
	for (j in 1:length(conc.groups)){
		print(conc.groups[j])
		a = raster(paste(concs,'conc.',conc.groups[j],'.tif',sep=''))		
		#Bay area AF
	
		f4 = paste('/home/vtinney/run/results/no2/all.cause/af/bay/bay.',beta.groups[i],'.',conc.groups[j],'.tif',sep='')
		af = overlay(n, a, fun=function(r1, r2){1-exp(-r1*r2)}) 

			if (conc.groups[j] == 'larkin.ug'){
			writeRaster(af, filename=f4, format="GTiff", overwrite=TRUE)}
			else{}

			#Crop to Alameda AF

			f5 = paste('/home/vtinney/run/results/no2/all.cause/af/alameda/alameda.',beta.groups[i],'.',conc.groups[j],'.tif',sep='')
			ala.2 <- crop(af, ala)
			ala.results <- mask(ala.2, ala)

				if (conc.groups[j] == 'larkin.ug'){
				writeRaster(ala.results, filename=f5, format="GTiff", overwrite=TRUE)}
				else{}
			

			#Crop to Oakland AF

			f6 = paste('/home/vtinney/run/results/no2/all.cause/af/oak/oak.',beta.groups[i],'.',conc.groups[j],'.tif',sep='')
			oak.2 <- crop(af, oak)
			oak.results <- mask(oak.2, oak)
			writeRaster(oak.results, filename=f6, format="GTiff", overwrite=TRUE)

		for (k in 1:length(rate.groups)){
		print(rate.groups[k])
		b = raster(paste(rates,'mortality.',rate.groups[k],'.tif',sep=''))	
			
			for (h in 1:length(pop.groups)){
			print(pop.groups[h])
			c = raster(paste(pops,'pop.',pop.groups[h],'.tif',sep=''))
			
				#Bay area PAF

						
				f1 = paste('/home/vtinney/run/results/no2/all.cause/paf/bay/bay.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				hia = overlay(c, b, n, a, fun=function(r1, r2, r3, r4){return(r1*r2*(10^-4)*(1-exp(-r3*r4)))})

					if (conc.groups[j] == 'larkin.ug' & rate.groups[k] == 'co.25'){

					writeRaster(hia, filename=f1, format="GTiff", overwrite=TRUE)}
					else{}	


				f8 = paste('/home/vtinney/run/results/no2/all.cause/paf/bay/rate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				rate100 = overlay(hia, c, fun=function(r1, r2){return((r1*100000)/r2)}) 

					if (conc.groups[j] == 'larkin.ug' & rate.groups[k] == 'co.25'){

					writeRaster(rate100, filename=f8, format="GTiff", overwrite=TRUE)}
					else{}				

				#Crop to Alameda PAF & rate per 100,000

				f2 = paste('/home/vtinney/run/results/no2/all.cause/paf/alameda/alameda.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				f9 = paste('/home/vtinney/run/results/no2/all.cause/paf/alameda/rate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				ala.2 <- crop(hia, ala)
				ala.results <- mask(ala.2, ala)

					if (conc.groups[j] == 'larkin.ug'){
					writeRaster(ala.results, filename=f2, format="GTiff", overwrite=TRUE)}
					else{}


				ala.3 <- crop(rate100, ala)
				ala.results2 <- mask(ala.3, ala)

					if (conc.groups[j] == 'larkin.ug'){
					writeRaster(ala.results2, filename=f9, format="GTiff", overwrite=TRUE)}
					else{}

				#Crop to Oakland PAF& rate per 100,000

				f3 = paste('/home/vtinney/run/results/no2/all.cause/paf/oak/oak.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				f10 = paste('/home/vtinney/run/results/no2/all.cause/paf/oak/rate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				oak.2 <- crop(hia, oak)
				oak.results <- mask(oak.2, oak)
				writeRaster(oak.results, filename=f3, format="GTiff", overwrite=TRUE)

				oak.3 <- crop(rate100, oak)
				oak.results2 <- mask(oak.3, oak)
				writeRaster(oak.results2, filename=f10, format="GTiff", overwrite=TRUE)


					for (m in 1:length(popfrac.groups)){
					print(popfrac.groups[m])
					r = raster(paste(popfracs,'popfrac.',popfrac.groups[m],'.tif',sep=''))

	
					f11 = paste('/home/vtinney/run/results/no2/all.cause/paf/bay/wrate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
					wrate100 = overlay(hia, c, r, fun=function(r1, r2, r3){return(((r1*100000)/r2)*r3)}) 

						if (conc.groups[j] == 'larkin.ug' & rate.groups[k] == 'co.25' & pop.groups[h] == popfrac.groups[m]){

						writeRaster(wrate100, filename=f11, format="GTiff", overwrite=TRUE)}
						else{}	

					f12 = paste('/home/vtinney/run/results/no2/all.cause/paf/alameda/wrate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
					ala.3 <- crop(wrate100, ala)
					ala.results2 <- mask(ala.3, ala)

						if (conc.groups[j] == 'larkin.ug' & pop.groups[h] == popfrac.groups[m]){
						writeRaster(ala.results2, filename=f12, format="GTiff", overwrite=TRUE)}
						else{}

					f13 = paste('/home/vtinney/run/results/no2/all.cause/paf/oak/wrate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
					oak.3 <- crop(wrate100, oak)
					oak.results2 <- mask(oak.3, oak)

						if (pop.groups[h] == popfrac.groups[m]){
						writeRaster(oak.results2, filename=f13, format="GTiff", overwrite=TRUE)}
						else{}

}}}}}
		
# ==================================================================
# All ages calc set up
rate.groups <- c('cbg.all', 'ct.all', 'zip.all', 'co.all')
conc.groups <- c('larkin.ug', 'NO2')
pop.groups <- c('wp.2016', 'bay.day', 'bay.night')
beta.groups <- c('atkin.ug', 'atkin.lower.ug', 'atkin.upper.ug')
popfrac.groups <- c('wp.2016', 'bay.day', 'bay.night')

# ==================================================================

# Loop for all Ages

for (i in 1:length(beta.groups)){
print(beta.groups[i])
n = raster(paste(betas,'beta.',beta.groups[i],'.tif',sep=''))	
	for (j in 1:length(conc.groups)){
		print(conc.groups[j])
		a = raster(paste(concs,'conc.',conc.groups[j],'.tif',sep=''))		
		#Bay area AF
	
		f4 = paste('/home/vtinney/run/results/no2/all.cause/af/bay/bay.',beta.groups[i],'.',conc.groups[j],'.tif',sep='')
		af = overlay(n, a, fun=function(r1, r2){1-exp(-r1*r2)}) 

			if (conc.groups[j] == 'larkin.ug'){
			writeRaster(af, filename=f4, format="GTiff", overwrite=TRUE)}
			else{}

			#Crop to Alameda AF

			f5 = paste('/home/vtinney/run/results/no2/all.cause/af/alameda/alameda.',beta.groups[i],'.',conc.groups[j],'.tif',sep='')
			ala.2 <- crop(af, ala)
			ala.results <- mask(ala.2, ala)

				if (conc.groups[j] == 'larkin.ug'){
				writeRaster(ala.results, filename=f5, format="GTiff", overwrite=TRUE)}
				else{}
			

			#Crop to Oakland AF

			f6 = paste('/home/vtinney/run/results/no2/all.cause/af/oak/oak.',beta.groups[i],'.',conc.groups[j],'.tif',sep='')
			oak.2 <- crop(af, oak)
			oak.results <- mask(oak.2, oak)
			writeRaster(oak.results, filename=f6, format="GTiff", overwrite=TRUE)

		for (k in 1:length(rate.groups)){
		print(rate.groups[k])
		b = raster(paste(rates,'mortality.',rate.groups[k],'.tif',sep=''))	
			
			for (h in 1:length(pop.groups)){
			print(pop.groups[h])
			c = raster(paste(pops,'pop.',pop.groups[h],'.tif',sep=''))
			
				#Bay area PAF

						
				f1 = paste('/home/vtinney/run/results/no2/all.cause/paf/bay/bay.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				hia = overlay(c, b, n, a, fun=function(r1, r2, r3, r4){return(r1*r2*(10^-4)*(1-exp(-r3*r4)))})

					if (conc.groups[j] == 'larkin.ug' & rate.groups[k] == 'co.all' | rate.groups[k] == 'zip.all'){

					writeRaster(hia, filename=f1, format="GTiff", overwrite=TRUE)}
					else{}	


				f8 = paste('/home/vtinney/run/results/no2/all.cause/paf/bay/rate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				rate100 = overlay(hia, c, fun=function(r1, r2){return((r1*100000)/r2)}) 

					if (conc.groups[j] == 'larkin.ug' & rate.groups[k] == 'co.all' | rate.groups[k] == 'zip.all'){

					writeRaster(rate100, filename=f8, format="GTiff", overwrite=TRUE)}
					else{}				

				#Crop to Alameda PAF & rate per 100,000

				f2 = paste('/home/vtinney/run/results/no2/all.cause/paf/alameda/alameda.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				f9 = paste('/home/vtinney/run/results/no2/all.cause/paf/alameda/rate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				ala.2 <- crop(hia, ala)
				ala.results <- mask(ala.2, ala)

					if (conc.groups[j] == 'larkin.ug'){
					writeRaster(ala.results, filename=f2, format="GTiff", overwrite=TRUE)}
					else{}


				ala.3 <- crop(rate100, ala)
				ala.results2 <- mask(ala.3, ala)

					if (conc.groups[j] == 'larkin.ug'){
					writeRaster(ala.results2, filename=f9, format="GTiff", overwrite=TRUE)}
					else{}

				#Crop to Oakland PAF& rate per 100,000

				f3 = paste('/home/vtinney/run/results/no2/all.cause/paf/oak/oak.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				f10 = paste('/home/vtinney/run/results/no2/all.cause/paf/oak/rate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				oak.2 <- crop(hia, oak)
				oak.results <- mask(oak.2, oak)
				writeRaster(oak.results, filename=f3, format="GTiff", overwrite=TRUE)

				oak.3 <- crop(rate100, oak)
				oak.results2 <- mask(oak.3, oak)
				writeRaster(oak.results2, filename=f10, format="GTiff", overwrite=TRUE)


				
					for (m in 1:length(popfrac.groups)){
					print(popfrac.groups[m])
					r = raster(paste(popfracs,'popfrac.',popfrac.groups[m],'.tif',sep=''))

	
					f11 = paste('/home/vtinney/run/results/no2/all.cause/paf/bay/wrate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
					wrate100 = overlay(hia, c, r, fun=function(r1, r2, r3){return(((r1*100000)/r2)*r3)}) 

						if (conc.groups[j] == 'larkin.ug' & rate.groups[k] == 'co.all' | rate.groups[k] == 'zip.all' & pop.groups[h] == popfrac.groups[m]){

						writeRaster(wrate100, filename=f11, format="GTiff", overwrite=TRUE)}
						else{}	

					f12 = paste('/home/vtinney/run/results/no2/all.cause/paf/alameda/wrate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
					ala.3 <- crop(wrate100, ala)
					ala.results2 <- mask(ala.3, ala)

						if (conc.groups[j] == 'larkin.ug' & pop.groups[h] == popfrac.groups[m]){
						writeRaster(ala.results2, filename=f12, format="GTiff", overwrite=TRUE)}
						else{}

					f13 = paste('/home/vtinney/run/results/no2/all.cause/paf/oak/wrate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
					oak.3 <- crop(wrate100, oak)
					oak.results2 <- mask(oak.3, oak)

						if (pop.groups[h] == popfrac.groups[m]){
						writeRaster(oak.results2, filename=f13, format="GTiff", overwrite=TRUE)}
						else{}
}}}}}
	
		
# ==================================================================
# Elderly calc set up
rate.groups <- c('cbg.65', 'ct.65', 'co.65')
conc.groups <- c('larkin.ug', 'NO2')
pop.groups <- c('ls.day.65', 'wp.65', 'ls.night.65', 'wp.night.65')
beta.groups <- c('eum.ug', 'eum.lower.ug', 'eum.upper.ug')
popfrac.groups <- c('ls.day.65', 'wp.65', 'ls.night.65', 'wp.night.65')
# ==================================================================

# Loop Elderly PAF and AF

for (i in 1:length(beta.groups)){
print(beta.groups[i])
n = raster(paste(betas,'beta.',beta.groups[i],'.tif',sep=''))	
	for (j in 1:length(conc.groups)){
		print(conc.groups[j])
		a = raster(paste(concs,'conc.',conc.groups[j],'.tif',sep=''))		
		#Bay area AF
	
		f4 = paste('/home/vtinney/run/results/no2/all.cause/af/bay/bay.',beta.groups[i],'.',conc.groups[j],'.tif',sep='')
		af = overlay(n, a, fun=function(r1, r2){1-exp(-r1*r2)}) 

			if (conc.groups[j] == 'larkin.ug'){
			writeRaster(af, filename=f4, format="GTiff", overwrite=TRUE)}
			else{}

			#Crop to Alameda AF

			f5 = paste('/home/vtinney/run/results/no2/all.cause/af/alameda/alameda.',beta.groups[i],'.',conc.groups[j],'.tif',sep='')
			ala.2 <- crop(af, ala)
			ala.results <- mask(ala.2, ala)

				if (conc.groups[j] == 'larkin.ug'){
				writeRaster(ala.results, filename=f5, format="GTiff", overwrite=TRUE)}
				else{}
			

			#Crop to Oakland AF

			f6 = paste('/home/vtinney/run/results/no2/all.cause/af/oak/oak.',beta.groups[i],'.',conc.groups[j],'.tif',sep='')
			oak.2 <- crop(af, oak)
			oak.results <- mask(oak.2, oak)
			writeRaster(oak.results, filename=f6, format="GTiff", overwrite=TRUE)

		for (k in 1:length(rate.groups)){
		print(rate.groups[k])
		b = raster(paste(rates,'mortality.',rate.groups[k],'.tif',sep=''))	
			
			for (h in 1:length(pop.groups)){
			print(pop.groups[h])
			c = raster(paste(pops,'pop.',pop.groups[h],'.tif',sep=''))
			
				#Bay area PAF

						
				f1 = paste('/home/vtinney/run/results/no2/all.cause/paf/bay/bay.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				hia = overlay(c, b, n, a, fun=function(r1, r2, r3, r4){return(r1*r2*(10^-4)*(1-exp(-r3*r4)))})

					if (conc.groups[j] == 'larkin.ug' & rate.groups[k] == 'co.65'){

					writeRaster(hia, filename=f1, format="GTiff", overwrite=TRUE)}
					else{}	


				f8 = paste('/home/vtinney/run/results/no2/all.cause/paf/bay/rate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				rate100 = overlay(hia, c, fun=function(r1, r2){return((r1*100000)/r2)}) 

					if (conc.groups[j] == 'larkin.ug' & rate.groups[k] == 'co.65'){

					writeRaster(rate100, filename=f8, format="GTiff", overwrite=TRUE)}
					else{}				

				#Crop to Alameda PAF & rate per 100,000

				f2 = paste('/home/vtinney/run/results/no2/all.cause/paf/alameda/alameda.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				f9 = paste('/home/vtinney/run/results/no2/all.cause/paf/alameda/rate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				ala.2 <- crop(hia, ala)
				ala.results <- mask(ala.2, ala)

					if (conc.groups[j] == 'larkin.ug'){
					writeRaster(ala.results, filename=f2, format="GTiff", overwrite=TRUE)}
					else{}


				ala.3 <- crop(rate100, ala)
				ala.results2 <- mask(ala.3, ala)

					if (conc.groups[j] == 'larkin.ug'){
					writeRaster(ala.results2, filename=f9, format="GTiff", overwrite=TRUE)}
					else{}

				#Crop to Oakland PAF& rate per 100,000

				f3 = paste('/home/vtinney/run/results/no2/all.cause/paf/oak/oak.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				f10 = paste('/home/vtinney/run/results/no2/all.cause/paf/oak/rate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				oak.2 <- crop(hia, oak)
				oak.results <- mask(oak.2, oak)
				writeRaster(oak.results, filename=f3, format="GTiff", overwrite=TRUE)

				oak.3 <- crop(rate100, oak)
				oak.results2 <- mask(oak.3, oak)
				writeRaster(oak.results2, filename=f10, format="GTiff", overwrite=TRUE)


				
					for (m in 1:length(popfrac.groups)){
					print(popfrac.groups[m])
					r = raster(paste(popfracs,'popfrac.',popfrac.groups[m],'.tif',sep=''))

	
					f11 = paste('/home/vtinney/run/results/no2/all.cause/paf/bay/wrate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
					wrate100 = overlay(hia, c, r, fun=function(r1, r2, r3){return(((r1*100000)/r2)*r3)}) 

						if (conc.groups[j] == 'larkin.ug' & rate.groups[k] == 'co.65' & pop.groups[h] == popfrac.groups[m]){

						writeRaster(wrate100, filename=f11, format="GTiff", overwrite=TRUE)}
						else{}	

					f12 = paste('/home/vtinney/run/results/no2/all.cause/paf/alameda/wrate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
					ala.3 <- crop(wrate100, ala)
					ala.results2 <- mask(ala.3, ala)

						if (conc.groups[j] == 'larkin.ug' & pop.groups[h] == popfrac.groups[m]){
						writeRaster(ala.results2, filename=f12, format="GTiff", overwrite=TRUE)}
						else{}

					f13 = paste('/home/vtinney/run/results/no2/all.cause/paf/oak/wrate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
					oak.3 <- crop(wrate100, oak)
					oak.results2 <- mask(oak.3, oak)

						if (pop.groups[h] == popfrac.groups[m]){
						writeRaster(oak.results2, filename=f13, format="GTiff", overwrite=TRUE)}
						else{}
}}}}}
	
