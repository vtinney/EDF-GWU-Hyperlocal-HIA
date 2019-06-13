# This is a script to perform HIA for the Bay area - NO2 and all-cause mortality, CVD mortality, asthma ER visits, and asthma incidence
# ==========================================================================================================================================
# Created: 2019-06-13
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
# Specify where the files are
pops <- '/home/vtinney/run/pop/'
rates <- '/home/vtinney/run/rates/'
concs <- '/home/vtinney/run/conc/'
betas <- '/home/vtinney/run/betas/no2/'
poptotal <- '/home/vtinney/run/pop/'

# ==================================================================
# Part 1 - All-cause mortality
# ==================================================================
# Adult, Bay calc set up
rate.groups <- c('co.25')
conc.groups <- c('larkin.ppb')
pop.groups <- c('ls.day.25', 'wp.day.25', 'ls.night.25', 'wp.night.25')
beta.groups <- c('atkin.point.ppb', 'atkin.lower.ppb', 'atkin.upper.ppb')
pop.total <- c('ls.day.25', 'wp.day.25', 'ls.night.25', 'wp.night.25')

# Loop: Adults, Bay

for (i in 1:length(beta.groups)){
print(beta.groups[i])
n = raster(paste(betas,'beta.',beta.groups[i],'.tif',sep=''))	

	for (j in 1:length(conc.groups)){
		print(conc.groups[j])
		a = raster(paste(concs,'conc.',conc.groups[j],'.tif',sep=''))		
		
		#Bay area AF
	
		f1 = paste('/home/vtinney/run/results/no2/all.cause/af/bay/bay.',beta.groups[i],'.',conc.groups[j],'.tif',sep='')
		af = overlay(n, a, fun=function(r1, r2){1-exp(-r1*r2)}, filename=f1, format="GTiff", overwrite=TRUE)

		for (k in 1:length(rate.groups)){
		print(rate.groups[k])
		b = raster(paste(rates,'mortality.',rate.groups[k],'.tif',sep=''))	
			
			for (h in 1:length(pop.groups)){
			print(pop.groups[h])
			c = raster(paste(pops,'bay.pop.',pop.groups[h],'.tif',sep=''))
			
				#Bay area PAF
	
				f2 = paste('/home/vtinney/run/results/no2/all.cause/paf/bay/bay.paf.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				hia = overlay(c, b, n, a, fun=function(r1, r2, r3, r4){return(r1*r2*(10^-4)*(1-exp(-r3*r4)))}, filename=f2, format="GTiff", overwrite=TRUE)

				#Bay area rate
				f3 = paste('/home/vtinney/run/results/no2/all.cause/paf/bay/bay.rate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				rate = overlay(hia, c, fun=function(r1, r2){return((r1/r2)*100000)}, filename=f3, format="GTiff", overwrite=TRUE)                                                                                                                                                                                                                                                                                                                                                                			

					for (m in 1:length(pop.total)){
					print(pop.total[m])
					r = raster(paste(poptotal,'total.bay.pop.',pop.total[m],'.tif',sep=''))

					f4 = paste('/home/vtinney/run/results/no2/all.cause/paf/bay/bay.wrate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
					wrate100 = overlay(hia, r, fun=function(r1, r2){return(r1/r2)})

						if (pop.groups[h] == pop.total[m]){

						writeRaster(wrate100, filename=f4, format="GTiff", overwrite=TRUE)}
						else{}	
}}}}}

# ==================================================================
# Adult, Alameda calc set up
rate.groups <- c('cbg.25', 'ct.25', 'co.25')
conc.groups <- c('larkin.ppb')
pop.groups <- c('ls.day.25', 'wp.day.25', 'ls.night.25', 'wp.night.25')
beta.groups <- c('atkin.point.ppb', 'atkin.lower.ppb', 'atkin.upper.ppb')
pop.total <- c('ls.day.25', 'wp.day.25', 'ls.night.25', 'wp.night.25')

# Loop: Adults, Alameda

for (i in 1:length(beta.groups)){
print(beta.groups[i])
n = raster(paste(betas,'beta.',beta.groups[i],'.tif',sep=''))	

	for (j in 1:length(conc.groups)){
		print(conc.groups[j])
		a = raster(paste(concs,'conc.',conc.groups[j],'.tif',sep=''))	
		
		#Alameda area AF
	
		f1 = paste('/home/vtinney/run/results/no2/all.cause/af/alameda/alameda.',beta.groups[i],'.',conc.groups[j],'.tif',sep='')
		af = overlay(n, a, fun=function(r1, r2){1-exp(-r1*r2)}, filename=f1, format="GTiff", overwrite=TRUE)

		for (k in 1:length(rate.groups)){
		print(rate.groups[k])
		b = raster(paste(rates,'mortality.',rate.groups[k],'.tif',sep=''))

			for (h in 1:length(pop.groups)){
			print(pop.groups[h])
			c = raster(paste(pops,'alameda.pop.',pop.groups[h],'.tif',sep=''))
			
				#Alameda area PAF
	
				f2 = paste('/home/vtinney/run/results/no2/all.cause/paf/alameda/alameda.paf.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				hia = overlay(c, b, n, a, fun=function(r1, r2, r3, r4){return(r1*r2*(10^-4)*(1-exp(-r3*r4)))}, filename=f2, format="GTiff", overwrite=TRUE)

				#Alameda area rate
				f3 = paste('/home/vtinney/run/results/no2/all.cause/paf/alameda/alameda.rate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				rate = overlay(hia, c, fun=function(r1, r2){return((r1/r2)*100000)}, filename=f3, format="GTiff", overwrite=TRUE)                                                                                                                                                                                                                                                                                                                                                                			

					for (m in 1:length(pop.total)){
					print(pop.total[m])
					r = raster(paste(poptotal,'total.alameda.pop.',pop.total[m],'.tif',sep=''))

					f4 = paste('/home/vtinney/run/results/no2/all.cause/paf/alameda/alameda.wrate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
					wrate100 = overlay(hia, r, fun=function(r1, r2){return(r1/r2)})

						if (pop.groups[h] == pop.total[m]){

						writeRaster(wrate100, filename=f4, format="GTiff", overwrite=TRUE)}
						else{}	
}}}}}

# ==================================================================
# Adult, Oakland calc set up
rate.groups <- c('cbg.25', 'ct.25', 'co.25')
conc.groups <- c('larkin.ppb', 'NO2.ppb')
pop.groups <- c('ls.day.25', 'wp.day.25', 'ls.night.25', 'wp.night.25')
beta.groups <- c('atkin.point.ppb', 'atkin.lower.ppb', 'atkin.upper.ppb')
pop.total <- c('ls.day.25', 'wp.day.25', 'ls.night.25', 'wp.night.25')

# Loop: Adults, Oakland

for (i in 1:length(beta.groups)){
print(beta.groups[i])
n = raster(paste(betas,'beta.',beta.groups[i],'.tif',sep=''))
	
	for (j in 1:length(conc.groups)){
		print(conc.groups[j])
		a = raster(paste(concs,'conc.',conc.groups[j],'.tif',sep=''))		
		
		#Oakland area AF
	
		f1 = paste('/home/vtinney/run/results/no2/all.cause/af/oak/oak.',beta.groups[i],'.',conc.groups[j],'.tif',sep='')
		af = overlay(n, a, fun=function(r1, r2){1-exp(-r1*r2)}, filename=f1, format="GTiff", overwrite=TRUE)

		for (k in 1:length(rate.groups)){
		print(rate.groups[k])
		b = raster(paste(rates,'mortality.',rate.groups[k],'.tif',sep=''))	
			
			for (h in 1:length(pop.groups)){
			print(pop.groups[h])
			c = raster(paste(pops,'oak.pop.',pop.groups[h],'.tif',sep=''))
			
				#Oakland area PAF
	
				f2 = paste('/home/vtinney/run/results/no2/all.cause/paf/oak/oak.paf.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				hia = overlay(c, b, n, a, fun=function(r1, r2, r3, r4){return(r1*r2*(10^-4)*(1-exp(-r3*r4)))}, filename=f2, format="GTiff", overwrite=TRUE)

				#Oakland area rate
				f3 = paste('/home/vtinney/run/results/no2/all.cause/paf/oak/oak.rate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				rate = overlay(hia, c, fun=function(r1, r2){return((r1/r2)*100000)}, filename=f3, format="GTiff", overwrite=TRUE)                                                                                                                                                                                                                                                                                                                                                                			

					for (m in 1:length(pop.total)){
					print(pop.total[m])
					r = raster(paste(poptotal,'total.oak.pop.',pop.total[m],'.tif',sep=''))

					f4 = paste('/home/vtinney/run/results/no2/all.cause/paf/oak/oak.wrate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
					wrate100 = overlay(hia, r, fun=function(r1, r2){return(r1/r2)})

						if (pop.groups[h] == pop.total[m]){

						writeRaster(wrate100, filename=f4, format="GTiff", overwrite=TRUE)}
						else{}	
}}}}}

# ==================================================================
# All ages, Bay calc set up
rate.groups <- c('co.all', 'zip.all')
conc.groups <- c('larkin.ppb')
pop.groups <- c('ls.day', 'wp.2016', 'ls.night')
beta.groups <- c('atkin.point.ppb', 'atkin.lower.ppb', 'atkin.upper.ppb')
pop.total <- c('ls.day', 'wp.2016', 'ls.night')

# Loop: All ages, Bay

for (i in 1:length(beta.groups)){
print(beta.groups[i])
n = raster(paste(betas,'beta.',beta.groups[i],'.tif',sep=''))	

	for (j in 1:length(conc.groups)){
		print(conc.groups[j])
		a = raster(paste(concs,'conc.',conc.groups[j],'.tif',sep=''))
		
		for (k in 1:length(rate.groups)){
		print(rate.groups[k])
		b = raster(paste(rates,'mortality.',rate.groups[k],'.tif',sep=''))	

			for (h in 1:length(pop.groups)){
			print(pop.groups[h])
			c = raster(paste(pops,'bay.pop.',pop.groups[h],'.tif',sep=''))

				#Bay area PAF
	
				f2 = paste('/home/vtinney/run/results/no2/all.cause/paf/bay/bay.paf.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				hia = overlay(c, b, n, a, fun=function(r1, r2, r3, r4){return(r1*r2*(10^-4)*(1-exp(-r3*r4)))}, filename=f2, format="GTiff", overwrite=TRUE)

				#Bay area rate
				f3 = paste('/home/vtinney/run/results/no2/all.cause/paf/bay/bay.rate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				rate = overlay(hia, c, fun=function(r1, r2){return((r1/r2)*100000)}, filename=f3, format="GTiff", overwrite=TRUE)                                                                                                                                                                                                                                                                                                                                                                			

					for (m in 1:length(pop.total)){
					print(pop.total[m])
					r = raster(paste(poptotal,'total.bay.pop.',pop.total[m],'.tif',sep=''))

					f4 = paste('/home/vtinney/run/results/no2/all.cause/paf/bay/bay.wrate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
					wrate100 = overlay(hia, r, fun=function(r1, r2){return(r1/r2)})

						if (pop.groups[h] == pop.total[m]){

						writeRaster(wrate100, filename=f4, format="GTiff", overwrite=TRUE)}
						else{}	
}}}}}

# ==================================================================
# All ages, Alameda calc set up
rate.groups <- c('cbg.all', 'ct.all', 'co.all')
conc.groups <- c('larkin.ppb')
pop.groups <- c('ls.day', 'wp.2016', 'ls.night')
beta.groups <- c('atkin.point.ppb', 'atkin.lower.ppb', 'atkin.upper.ppb')
pop.total <- c('ls.day', 'wp.2016', 'ls.night')


# Loop: All ages, Alameda

for (i in 1:length(beta.groups)){
print(beta.groups[i])
n = raster(paste(betas,'beta.',beta.groups[i],'.tif',sep=''))	

	for (j in 1:length(conc.groups)){
		print(conc.groups[j])
		a = raster(paste(concs,'conc.',conc.groups[j],'.tif',sep=''))		
	
		for (k in 1:length(rate.groups)){
		print(rate.groups[k])
		b = raster(paste(rates,'mortality.',rate.groups[k],'.tif',sep=''))
			
			for (h in 1:length(pop.groups)){
			print(pop.groups[h])
			c = raster(paste(pops,'alameda.pop.',pop.groups[h],'.tif',sep=''))

				#Alameda area PAF
	
				f2 = paste('/home/vtinney/run/results/no2/all.cause/paf/alameda/alameda.paf.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				hia = overlay(c, b, n, a, fun=function(r1, r2, r3, r4){return(r1*r2*(10^-4)*(1-exp(-r3*r4)))}, filename=f2, format="GTiff", overwrite=TRUE)

				#Alameda area rate
				f3 = paste('/home/vtinney/run/results/no2/all.cause/paf/alameda/alameda.rate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				rate = overlay(hia, c, fun=function(r1, r2){return((r1/r2)*100000)}, filename=f3, format="GTiff", overwrite=TRUE)                                                                                                                                                                                                                                                                                                                                                                			

					for (m in 1:length(pop.total)){
					print(pop.total[m])
					r = raster(paste(poptotal,'total.alameda.pop.',pop.total[m],'.tif',sep=''))

					f4 = paste('/home/vtinney/run/results/no2/all.cause/paf/alameda/alameda.wrate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
					wrate100 = overlay(hia, r, fun=function(r1, r2){return(r1/r2)})

						if (pop.groups[h] == pop.total[m]){

						writeRaster(wrate100, filename=f4, format="GTiff", overwrite=TRUE)}
						else{}	
}}}}}

# ==================================================================
# All ages, Oakland calc set up
rate.groups <- c('cbg.all', 'ct.all', 'co.all')
conc.groups <- c('larkin.ppb', 'NO2.ppb')
pop.groups <- c('ls.day', 'wp.2016', 'ls.night')
beta.groups <- c('atkin.point.ppb', 'atkin.lower.ppb', 'atkin.upper.ppb')
pop.total <- c('ls.day', 'wp.2016', 'ls.night')

# Loop: All ages, Oakland

for (i in 1:length(beta.groups)){
print(beta.groups[i])
n = raster(paste(betas,'beta.',beta.groups[i],'.tif',sep=''))	

	for (j in 1:length(conc.groups)){
		print(conc.groups[j])
		a = raster(paste(concs,'conc.',conc.groups[j],'.tif',sep=''))		

		for (k in 1:length(rate.groups)){
		print(rate.groups[k])
		b = raster(paste(rates,'mortality.',rate.groups[k],'.tif',sep=''))	
			
			for (h in 1:length(pop.groups)){
			print(pop.groups[h])
			c = raster(paste(pops,'oak.pop.',pop.groups[h],'.tif',sep=''))

				#Oakland area PAF
	
				f2 = paste('/home/vtinney/run/results/no2/all.cause/paf/oak/oak.paf.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				hia = overlay(c, b, n, a, fun=function(r1, r2, r3, r4){return(r1*r2*(10^-4)*(1-exp(-r3*r4)))}, filename=f2, format="GTiff", overwrite=TRUE)

				#Oakland area rate
				f3 = paste('/home/vtinney/run/results/no2/all.cause/paf/oak/oak.rate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				rate = overlay(hia, c, fun=function(r1, r2){return((r1/r2)*100000)}, filename=f3, format="GTiff", overwrite=TRUE)                                                                                                                                                                                                                                                                                                                                                                			

					for (m in 1:length(pop.total)){
					print(pop.total[m])
					r = raster(paste(poptotal,'total.oak.pop.',pop.total[m],'.tif',sep=''))

					f4 = paste('/home/vtinney/run/results/no2/all.cause/paf/oak/oak.wrate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
					wrate100 = overlay(hia, r, fun=function(r1, r2){return(r1/r2)})

						if (pop.groups[h] == pop.total[m]){

						writeRaster(wrate100, filename=f4, format="GTiff", overwrite=TRUE)}
						else{}	
}}}}}

# ==================================================================
# Elderly, Bay calc set up
rate.groups <- c('co.65')
conc.groups <- c('larkin.ppb')
pop.groups <- c('ls.day.65', 'wp.day.65', 'ls.night.65', 'wp.night.65')
beta.groups <- c('eum.point.ppb', 'eum.lower.ppb', 'eum.upper.ppb')
pop.total <- c('ls.day.65', 'wp.day.65', 'ls.night.65', 'wp.night.65')

# Loop: Elderly, Bay

for (i in 1:length(beta.groups)){
print(beta.groups[i])
n = raster(paste(betas,'beta.',beta.groups[i],'.tif',sep=''))	

	for (j in 1:length(conc.groups)){
		print(conc.groups[j])
		a = raster(paste(concs,'conc.',conc.groups[j],'.tif',sep=''))
		
		f1 = paste('/home/vtinney/run/results/no2/all.cause/af/bay/bay.',beta.groups[i],'.',conc.groups[j],'.tif',sep='')
		af = overlay(n, a, fun=function(r1, r2){1-exp(-r1*r2)}, filename=f1, format="GTiff", overwrite=TRUE)


		for (k in 1:length(rate.groups)){
		print(rate.groups[k])
		b = raster(paste(rates,'mortality.',rate.groups[k],'.tif',sep=''))	
			
			for (h in 1:length(pop.groups)){
			print(pop.groups[h])
			c = raster(paste(pops,'bay.pop.',pop.groups[h],'.tif',sep=''))
			
				#Bay area PAF
	
				f2 = paste('/home/vtinney/run/results/no2/all.cause/paf/bay/bay.paf.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				hia = overlay(c, b, n, a, fun=function(r1, r2, r3, r4){return(r1*r2*(10^-4)*(1-exp(-r3*r4)))}, filename=f2, format="GTiff", overwrite=TRUE)

				#Bay area rate
				f3 = paste('/home/vtinney/run/results/no2/all.cause/paf/bay/bay.rate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				rate = overlay(hia, c, fun=function(r1, r2){return((r1/r2)*100000)}, filename=f3, format="GTiff", overwrite=TRUE)                                                                                                                                                                                                                                                                                                                                                                			

					for (m in 1:length(pop.total)){
					print(pop.total[m])
					r = raster(paste(poptotal,'total.bay.pop.',pop.total[m],'.tif',sep=''))

					f4 = paste('/home/vtinney/run/results/no2/all.cause/paf/bay/bay.wrate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
					wrate100 = overlay(hia, r, fun=function(r1, r2){return(r1/r2)})

						if (pop.groups[h] == pop.total[m]){

						writeRaster(wrate100, filename=f4, format="GTiff", overwrite=TRUE)}
						else{}	
}}}}}

# ==================================================================
# Elderly, Alameda calc set up
rate.groups <- c('cbg.65', 'ct.65', 'co.65')
conc.groups <- c('larkin.ppb')
pop.groups <- c('ls.day.65', 'wp.day.65', 'ls.night.65', 'wp.night.65')
beta.groups <- c('eum.point.ppb', 'eum.lower.ppb', 'eum.upper.ppb')
pop.total <- c('ls.day.65', 'wp.day.65', 'ls.night.65', 'wp.night.65')


# Loop: Elderly, Alameda

for (i in 1:length(beta.groups)){
print(beta.groups[i])
n = raster(paste(betas,'beta.',beta.groups[i],'.tif',sep=''))	

	for (j in 1:length(conc.groups)){
		print(conc.groups[j])
		a = raster(paste(concs,'conc.',conc.groups[j],'.tif',sep=''))	

		f1 = paste('/home/vtinney/run/results/no2/all.cause/af/alameda/alameda.',beta.groups[i],'.',conc.groups[j],'.tif',sep='')
		af = overlay(n, a, fun=function(r1, r2){1-exp(-r1*r2)}, filename=f1, format="GTiff", overwrite=TRUE)

		for (k in 1:length(rate.groups)){
		print(rate.groups[k])
		b = raster(paste(rates,'mortality.',rate.groups[k],'.tif',sep=''))	
			
			for (h in 1:length(pop.groups)){
			print(pop.groups[h])
			c = raster(paste(pops,'alameda.pop.',pop.groups[h],'.tif',sep=''))
			
				#Alameda area PAF
	
				f2 = paste('/home/vtinney/run/results/no2/all.cause/paf/alameda/alameda.paf.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				hia = overlay(c, b, n, a, fun=function(r1, r2, r3, r4){return(r1*r2*(10^-4)*(1-exp(-r3*r4)))}, filename=f2, format="GTiff", overwrite=TRUE)

				#Alameda area rate
				f3 = paste('/home/vtinney/run/results/no2/all.cause/paf/alameda/alameda.rate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				rate = overlay(hia, c, fun=function(r1, r2){return((r1/r2)*100000)}, filename=f3, format="GTiff", overwrite=TRUE)                                                                                                                                                                                                                                                                                                                                                                			

					for (m in 1:length(pop.total)){
					print(pop.total[m])
					r = raster(paste(poptotal,'total.alameda.pop.',pop.total[m],'.tif',sep=''))

					f4 = paste('/home/vtinney/run/results/no2/all.cause/paf/alameda/alameda.wrate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
					wrate100 = overlay(hia, r, fun=function(r1, r2){return(r1/r2)})

						if (pop.groups[h] == pop.total[m]){

						writeRaster(wrate100, filename=f4, format="GTiff", overwrite=TRUE)}
						else{}	
}}}}}

# ==================================================================
# Elderly, Oakland calc set up
rate.groups <- c('cbg.65', 'ct.65', 'co.65')
conc.groups <- c('larkin.ppb', 'NO2.ppb')
pop.groups <- c('ls.day.65', 'wp.day.65', 'ls.night.65', 'wp.night.65')
beta.groups <- c('eum.point.ppb', 'eum.lower.ppb', 'eum.upper.ppb')
pop.total <- c('ls.day.65', 'wp.day.65', 'ls.night.65', 'wp.night.65')

# Loop: Elderly, Oakland

for (i in 1:length(beta.groups)){
print(beta.groups[i])
n = raster(paste(betas,'beta.',beta.groups[i],'.tif',sep=''))
	
	for (j in 1:length(conc.groups)){
		print(conc.groups[j])
		a = raster(paste(concs,'conc.',conc.groups[j],'.tif',sep=''))	

		f1 = paste('/home/vtinney/run/results/no2/all.cause/af/oak/oak.',beta.groups[i],'.',conc.groups[j],'.tif',sep='')
		af = overlay(n, a, fun=function(r1, r2){1-exp(-r1*r2)}, filename=f1, format="GTiff", overwrite=TRUE)

		for (k in 1:length(rate.groups)){
		print(rate.groups[k])
		b = raster(paste(rates,'mortality.',rate.groups[k],'.tif',sep=''))	
			
			for (h in 1:length(pop.groups)){
			print(pop.groups[h])
			c = raster(paste(pops,'oak.pop.',pop.groups[h],'.tif',sep=''))
			
				#Oakland area PAF
	
				f2 = paste('/home/vtinney/run/results/no2/all.cause/paf/oak/oak.paf.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				hia = overlay(c, b, n, a, fun=function(r1, r2, r3, r4){return(r1*r2*(10^-4)*(1-exp(-r3*r4)))}, filename=f2, format="GTiff", overwrite=TRUE)

				#Oakland area rate
				f3 = paste('/home/vtinney/run/results/no2/all.cause/paf/oak/oak.rate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				rate = overlay(hia, c, fun=function(r1, r2){return((r1/r2)*100000)}, filename=f3, format="GTiff", overwrite=TRUE)                                                                                                                                                                                                                                                                                                                                                                			

					for (m in 1:length(pop.total)){
					print(pop.total[m])
					r = raster(paste(poptotal,'total.oak.pop.',pop.total[m],'.tif',sep=''))

					f4 = paste('/home/vtinney/run/results/no2/all.cause/paf/oak/oak.wrate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
					wrate100 = overlay(hia, r, fun=function(r1, r2){return(r1/r2)})

						if (pop.groups[h] == pop.total[m]){

						writeRaster(wrate100, filename=f4, format="GTiff", overwrite=TRUE)}
						else{}	
}}}}}

# ==========================================================================================================================================
# Part 2 - NO2 and CVD Mortality 
# ==================================================================
# Specify where the files are
pops <- '/home/vtinney/run/pop/'
rates <- '/home/vtinney/run/rates/'
concs <- '/home/vtinney/run/conc/'
betas <- '/home/vtinney/run/betas/no2/'
poptotal <- '/home/vtinney/run/pop/'

# ==================================================================
# Adult, CVD mortality
rate.groups <- c('co.25')
conc.groups <- c('larkin.ppb')
pop.groups <- c('ls.day.25', 'wp.day.25', 'ls.night.25', 'wp.night.25')
beta.groups <- c('atkin.cvd.point.ppb', 'atkin.cvd.lower.ppb', 'atkin.cvd.upper.ppb')
pop.total <- c('ls.day.25', 'wp.day.25', 'ls.night.25', 'wp.night.25')

# Loop: Adults, Bay

for (i in 1:length(beta.groups)){
print(beta.groups[i])
n = raster(paste(betas,'beta.',beta.groups[i],'.tif',sep=''))	

	for (j in 1:length(conc.groups)){
		print(conc.groups[j])
		a = raster(paste(concs,'conc.',conc.groups[j],'.tif',sep=''))		
		
		#Bay area AF
	
		f1 = paste('/home/vtinney/run/results/no2/cvd.mort/af/bay/bay.',beta.groups[i],'.',conc.groups[j],'.tif',sep='')
		af = overlay(n, a, fun=function(r1, r2){1-exp(-r1*r2)}, filename=f1, format="GTiff", overwrite=TRUE)

		for (k in 1:length(rate.groups)){
		print(rate.groups[k])
		b = raster(paste(rates,'mortality.cvd.',rate.groups[k],'.tif',sep=''))	
			
			for (h in 1:length(pop.groups)){
			print(pop.groups[h])
			c = raster(paste(pops,'bay.pop.',pop.groups[h],'.tif',sep=''))
			
				#Bay area PAF
	
				f2 = paste('/home/vtinney/run/results/no2/cvd.mort/paf/bay/bay.paf.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				hia = overlay(c, b, n, a, fun=function(r1, r2, r3, r4){return(r1*r2*(10^-4)*(1-exp(-r3*r4)))}, filename=f2, format="GTiff", overwrite=TRUE)

				#Bay area rate
				f3 = paste('/home/vtinney/run/results/no2/cvd.mort/paf/bay/bay.rate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				rate = overlay(hia, c, fun=function(r1, r2){return((r1/r2)*100000)}, filename=f3, format="GTiff", overwrite=TRUE)                                                                                                                                                                                                                                                                                                                                                                			

					for (m in 1:length(pop.total)){
					print(pop.total[m])
					r = raster(paste(poptotal,'total.bay.',pop.total[m],'.tif',sep=''))

					f4 = paste('/home/vtinney/run/results/no2/cvd.mort/paf/bay/bay.wrate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
					wrate100 = overlay(hia, r, fun=function(r1, r2){return(r1/r2)})

						if (pop.groups[h] == pop.total[m]){

						writeRaster(wrate100, filename=f4, format="GTiff", overwrite=TRUE)}
						else{}	
}}}}}

# ==================================================================
# Adult, Alameda calc set up
rate.groups <- c('cbg.25', 'ct.25', 'co.25')
conc.groups <- c('larkin.ppb')
pop.groups <- c('ls.day.25', 'wp.day.25', 'ls.night.25', 'wp.night.25')
beta.groups <- c('atkin.cvd.point.ppb', 'atkin.cvd.lower.ppb', 'atkin.cvd.upper.ppb')
pop.total <- c('ls.day.25', 'wp.day.25', 'ls.night.25', 'wp.night.25')

# Loop: Adults, Alameda

for (i in 1:length(beta.groups)){
print(beta.groups[i])
n = raster(paste(betas,'beta.',beta.groups[i],'.tif',sep=''))	

	for (j in 1:length(conc.groups)){
		print(conc.groups[j])
		a = raster(paste(concs,'conc.',conc.groups[j],'.tif',sep=''))	
		
		#Alameda area AF
	
		f1 = paste('/home/vtinney/run/results/no2/cvd.mort/af/alameda/alameda.',beta.groups[i],'.',conc.groups[j],'.tif',sep='')
		af = overlay(n, a, fun=function(r1, r2){1-exp(-r1*r2)}, filename=f1, format="GTiff", overwrite=TRUE)

		for (k in 1:length(rate.groups)){
		print(rate.groups[k])
		b = raster(paste(rates,'mortality.cvd.',rate.groups[k],'.tif',sep=''))

			for (h in 1:length(pop.groups)){
			print(pop.groups[h])
			c = raster(paste(pops,'alameda.pop.',pop.groups[h],'.tif',sep=''))
			
				#Alameda area PAF
	
				f2 = paste('/home/vtinney/run/results/no2/cvd.mort/paf/alameda/alameda.paf.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				hia = overlay(c, b, n, a, fun=function(r1, r2, r3, r4){return(r1*r2*(10^-4)*(1-exp(-r3*r4)))}, filename=f2, format="GTiff", overwrite=TRUE)

				#Alameda area rate
				f3 = paste('/home/vtinney/run/results/no2/cvd.mort/paf/alameda/alameda.rate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				rate = overlay(hia, c, fun=function(r1, r2){return((r1/r2)*100000)}, filename=f3, format="GTiff", overwrite=TRUE)                                                                                                                                                                                                                                                                                                                                                                			

					for (m in 1:length(pop.total)){
					print(pop.total[m])
					r = raster(paste(poptotal,'total.alameda.pop.',pop.total[m],'.tif',sep=''))

					f4 = paste('/home/vtinney/run/results/no2/cvd.mort/paf/alameda/alameda.wrate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
					wrate100 = overlay(hia, r, fun=function(r1, r2){return(r1/r2)})

						if (pop.groups[h] == pop.total[m]){

						writeRaster(wrate100, filename=f4, format="GTiff", overwrite=TRUE)}
						else{}	
}}}}}

# ==================================================================
# Adult, Oakland calc set up
rate.groups <- c('cbg.25', 'ct.25', 'co.25')
conc.groups <- c('larkin.ppb', 'NO2.ppb')
pop.groups <- c('ls.day.25', 'wp.day.25', 'ls.night.25', 'wp.night.25')
beta.groups <- c('atkin.cvd.point.ppb', 'atkin.cvd.lower.ppb', 'atkin.cvd.upper.ppb')
pop.total <- c('ls.day.25', 'wp.day.25', 'ls.night.25', 'wp.night.25')

# Loop: Adults, Oakland

for (i in 1:length(beta.groups)){
print(beta.groups[i])
n = raster(paste(betas,'beta.',beta.groups[i],'.tif',sep=''))
	
	for (j in 1:length(conc.groups)){
		print(conc.groups[j])
		a = raster(paste(concs,'conc.',conc.groups[j],'.tif',sep=''))		
		
		#Oakland area AF
	
		f1 = paste('/home/vtinney/run/results/no2/cvd.mort/af/oak/oak.',beta.groups[i],'.',conc.groups[j],'.tif',sep='')
		af = overlay(n, a, fun=function(r1, r2){1-exp(-r1*r2)}, filename=f1, format="GTiff", overwrite=TRUE)

		for (k in 1:length(rate.groups)){
		print(rate.groups[k])
		b = raster(paste(rates,'mortality.cvd.',rate.groups[k],'.tif',sep=''))	
			
			for (h in 1:length(pop.groups)){
			print(pop.groups[h])
			c = raster(paste(pops,'oak.pop.',pop.groups[h],'.tif',sep=''))
			
				#Oakland area PAF
	
				f2 = paste('/home/vtinney/run/results/no2/cvd.mort/paf/oak/oak.paf.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				hia = overlay(c, b, n, a, fun=function(r1, r2, r3, r4){return(r1*r2*(10^-4)*(1-exp(-r3*r4)))}, filename=f2, format="GTiff", overwrite=TRUE)

				#Oakland area rate
				f3 = paste('/home/vtinney/run/results/no2/cvd.mort/paf/oak/oak.rate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				rate = overlay(hia, c, fun=function(r1, r2){return((r1/r2)*100000)}, filename=f3, format="GTiff", overwrite=TRUE)                                                                                                                                                                                                                                                                                                                                                                			

					for (m in 1:length(pop.total)){
					print(pop.total[m])
					r = raster(paste(poptotal,'total.oak.pop.',pop.total[m],'.tif',sep=''))

					f4 = paste('/home/vtinney/run/results/no2/cvd.mort/paf/oak/oak.wrate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
					wrate100 = overlay(hia, r, fun=function(r1, r2){return(r1/r2)})

						if (pop.groups[h] == pop.total[m]){

						writeRaster(wrate100, filename=f4, format="GTiff", overwrite=TRUE)}
						else{}	
}}}}}

# ==================================================================
# All ages, Bay calc set up
rate.groups <- c('co.all')
conc.groups <- c('larkin.ppb')
pop.groups <- c('ls.day', 'wp.2016', 'ls.night')
beta.groups <- c('atkin.cvd.point.ppb', 'atkin.cvd.lower.ppb', 'atkin.upper.ppb')
pop.total <- c('ls.day', 'wp.2016', 'ls.night')

# Loop: All ages, Bay

for (i in 1:length(beta.groups)){
print(beta.groups[i])
n = raster(paste(betas,'beta.',beta.groups[i],'.tif',sep=''))	

	for (j in 1:length(conc.groups)){
		print(conc.groups[j])
		a = raster(paste(concs,'conc.',conc.groups[j],'.tif',sep=''))
		
		for (k in 1:length(rate.groups)){
		print(rate.groups[k])
		b = raster(paste(rates,'mortality.cvd.',rate.groups[k],'.tif',sep=''))	

			for (h in 1:length(pop.groups)){
			print(pop.groups[h])
			c = raster(paste(pops,'bay.pop.',pop.groups[h],'.tif',sep=''))

				#Bay area PAF
	
				f2 = paste('/home/vtinney/run/results/no2/cvd.mort/paf/bay/bay.paf.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				hia = overlay(c, b, n, a, fun=function(r1, r2, r3, r4){return(r1*r2*(10^-4)*(1-exp(-r3*r4)))}, filename=f2, format="GTiff", overwrite=TRUE)

				#Bay area rate
				f3 = paste('/home/vtinney/run/results/no2/cvd.mort/paf/bay/bay.rate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				rate = overlay(hia, c, fun=function(r1, r2){return((r1/r2)*100000)}, filename=f3, format="GTiff", overwrite=TRUE)                                                                                                                                                                                                                                                                                                                                                                			

					for (m in 1:length(pop.total)){
					print(pop.total[m])
					r = raster(paste(poptotal,'total.bay.',pop.total[m],'.tif',sep=''))

					f4 = paste('/home/vtinney/run/results/no2/cvd.mort/paf/bay/bay.wrate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
					wrate100 = overlay(hia, r, fun=function(r1, r2){return(r1/r2)})

						if (pop.groups[h] == pop.total[m]){

						writeRaster(wrate100, filename=f4, format="GTiff", overwrite=TRUE)}
						else{}	
}}}}}

# ==================================================================
# All ages, Alameda calc set up
rate.groups <- c('co.all')
conc.groups <- c('larkin.ppb')
pop.groups <- c('ls.day', 'wp.2016', 'ls.night')
beta.groups <- c('atkin.cvd.point.ppb', 'atkin.cvd.lower.ppb', 'atkin.cvd.upper.ppb')
pop.total <- c('ls.day', 'wp.2016', 'ls.night')


# Loop: All ages, Alameda

for (i in 1:length(beta.groups)){
print(beta.groups[i])
n = raster(paste(betas,'beta.',beta.groups[i],'.tif',sep=''))	

	for (j in 1:length(conc.groups)){
		print(conc.groups[j])
		a = raster(paste(concs,'conc.',conc.groups[j],'.tif',sep=''))		
	
		for (k in 1:length(rate.groups)){
		print(rate.groups[k])
		b = raster(paste(rates,'mortality.cvd.',rate.groups[k],'.tif',sep=''))
			
			for (h in 1:length(pop.groups)){
			print(pop.groups[h])
			c = raster(paste(pops,'alameda.pop.',pop.groups[h],'.tif',sep=''))

				#Alameda area PAF
	
				f2 = paste('/home/vtinney/run/results/no2/cvd.mort/paf/alameda/alameda.paf.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				hia = overlay(c, b, n, a, fun=function(r1, r2, r3, r4){return(r1*r2*(10^-4)*(1-exp(-r3*r4)))}, filename=f2, format="GTiff", overwrite=TRUE)

				#Alameda area rate
				f3 = paste('/home/vtinney/run/results/no2/cvd.mort/paf/alameda/alameda.rate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				rate = overlay(hia, c, fun=function(r1, r2){return((r1/r2)*100000)}, filename=f3, format="GTiff", overwrite=TRUE)                                                                                                                                                                                                                                                                                                                                                                			

					for (m in 1:length(pop.total)){
					print(pop.total[m])
					r = raster(paste(poptotal,'total.alameda.pop.',pop.total[m],'.tif',sep=''))

					f4 = paste('/home/vtinney/run/results/no2/cvd.mort/paf/alameda/alameda.wrate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
					wrate100 = overlay(hia, r, fun=function(r1, r2){return(r1/r2)})

						if (pop.groups[h] == pop.total[m]){

						writeRaster(wrate100, filename=f4, format="GTiff", overwrite=TRUE)}
						else{}	
}}}}}

# ==================================================================
# All ages, Oakland calc set up
rate.groups <- c('co.all')
conc.groups <- c('larkin.ppb', 'NO2.ppb')
pop.groups <- c('ls.day', 'wp.2016', 'ls.night')
beta.groups <- c('atkin.cvd.point.ppb', 'atkin.cvd.lower.ppb', 'atkin.cvd.upper.ppb')
pop.total <- c('ls.day', 'wp.2016', 'ls.night')

# Loop: All ages, Oakland

for (i in 1:length(beta.groups)){
print(beta.groups[i])
n = raster(paste(betas,'beta.',beta.groups[i],'.tif',sep=''))	

	for (j in 1:length(conc.groups)){
		print(conc.groups[j])
		a = raster(paste(concs,'conc.',conc.groups[j],'.tif',sep=''))		

		for (k in 1:length(rate.groups)){
		print(rate.groups[k])
		b = raster(paste(rates,'mortality.cvd.',rate.groups[k],'.tif',sep=''))	
			
			for (h in 1:length(pop.groups)){
			print(pop.groups[h])
			c = raster(paste(pops,'oak.pop.',pop.groups[h],'.tif',sep=''))

				#Oakland area PAF
	
				f2 = paste('/home/vtinney/run/results/no2/cvd.mort/paf/oak/oak.paf.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				hia = overlay(c, b, n, a, fun=function(r1, r2, r3, r4){return(r1*r2*(10^-4)*(1-exp(-r3*r4)))}, filename=f2, format="GTiff", overwrite=TRUE)

				#Oakland area rate
				f3 = paste('/home/vtinney/run/results/no2/cvd.mort/paf/oak/oak.rate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				rate = overlay(hia, c, fun=function(r1, r2){return((r1/r2)*100000)}, filename=f3, format="GTiff", overwrite=TRUE)                                                                                                                                                                                                                                                                                                                                                                			

					for (m in 1:length(pop.total)){
					print(pop.total[m])
					r = raster(paste(poptotal,'total.oak.pop.',pop.total[m],'.tif',sep=''))

					f4 = paste('/home/vtinney/run/results/no2/cvd.mort/paf/oak/oak.wrate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
					wrate100 = overlay(hia, r, fun=function(r1, r2){return(r1/r2)})

						if (pop.groups[h] == pop.total[m]){

						writeRaster(wrate100, filename=f4, format="GTiff", overwrite=TRUE)}
						else{}	
}}}}}

# ==================================================================
# Elderly, Bay calc set up
rate.groups <- c('co.65')
conc.groups <- c('larkin.ppb')
pop.groups <- c('ls.day.65', 'wp.day.65', 'ls.night.65', 'wp.night.65')
beta.groups <- c('eum.cvd.point.ppb', 'eum.cvd.lower.ppb', 'eum.cvd.upper.ppb')
pop.total <- c('ls.day.65', 'wp.day.65', 'ls.night.65', 'wp.night.65')

# Loop: Elderly, Bay

for (i in 1:length(beta.groups)){
print(beta.groups[i])
n = raster(paste(betas,'beta.',beta.groups[i],'.tif',sep=''))	

	for (j in 1:length(conc.groups)){
		print(conc.groups[j])
		a = raster(paste(concs,'conc.',conc.groups[j],'.tif',sep=''))
		
		f1 = paste('/home/vtinney/run/results/no2/cvd.mort/af/bay/bay.',beta.groups[i],'.',conc.groups[j],'.tif',sep='')
		af = overlay(n, a, fun=function(r1, r2){1-exp(-r1*r2)}, filename=f1, format="GTiff", overwrite=TRUE)


		for (k in 1:length(rate.groups)){
		print(rate.groups[k])
		b = raster(paste(rates,'mortality.cvd.',rate.groups[k],'.tif',sep=''))	
			
			for (h in 1:length(pop.groups)){
			print(pop.groups[h])
			c = raster(paste(pops,'bay.pop.',pop.groups[h],'.tif',sep=''))
			
				#Bay area PAF
	
				f2 = paste('/home/vtinney/run/results/no2/cvd.mort/paf/bay/bay.paf.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				hia = overlay(c, b, n, a, fun=function(r1, r2, r3, r4){return(r1*r2*(10^-4)*(1-exp(-r3*r4)))}, filename=f2, format="GTiff", overwrite=TRUE)

				#Bay area rate
				f3 = paste('/home/vtinney/run/results/no2/cvd.mort/paf/bay/bay.rate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				rate = overlay(hia, c, fun=function(r1, r2){return((r1/r2)*100000)}, filename=f3, format="GTiff", overwrite=TRUE)                                                                                                                                                                                                                                                                                                                                                                			

					for (m in 1:length(pop.total)){
					print(pop.total[m])
					r = raster(paste(poptotal,'total.bay.',pop.total[m],'.tif',sep=''))

					f4 = paste('/home/vtinney/run/results/no2/cvd.mort/paf/bay/bay.wrate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
					wrate100 = overlay(hia, r, fun=function(r1, r2){return(r1/r2)})

						if (pop.groups[h] == pop.total[m]){

						writeRaster(wrate100, filename=f4, format="GTiff", overwrite=TRUE)}
						else{}	
}}}}}

# ==================================================================
# Elderly, Alameda calc set up
rate.groups <- c('cbg.65', 'ct.65', 'co.65')
conc.groups <- c('larkin.ppb')
pop.groups <- c('ls.day.65', 'wp.day.65', 'ls.night.65', 'wp.night.65')
beta.groups <- c('eum.cvd.point.ppb', 'eum.cvd.lower.ppb', 'eum.cvd.upper.ppb')
pop.total <- c('ls.day.65', 'wp.day.65', 'ls.night.65', 'wp.night.65')


# Loop: Elderly, Alameda

for (i in 1:length(beta.groups)){
print(beta.groups[i])
n = raster(paste(betas,'beta.',beta.groups[i],'.tif',sep=''))	

	for (j in 1:length(conc.groups)){
		print(conc.groups[j])
		a = raster(paste(concs,'conc.',conc.groups[j],'.tif',sep=''))	

		f1 = paste('/home/vtinney/run/results/no2/cvd.mort/af/alameda/alameda.',beta.groups[i],'.',conc.groups[j],'.tif',sep='')
		af = overlay(n, a, fun=function(r1, r2){1-exp(-r1*r2)}, filename=f1, format="GTiff", overwrite=TRUE)

		for (k in 1:length(rate.groups)){
		print(rate.groups[k])
		b = raster(paste(rates,'mortality.cvd.',rate.groups[k],'.tif',sep=''))	
			
			for (h in 1:length(pop.groups)){
			print(pop.groups[h])
			c = raster(paste(pops,'alameda.pop.',pop.groups[h],'.tif',sep=''))
			
				#Alameda area PAF
	
				f2 = paste('/home/vtinney/run/results/no2/cvd.mort/paf/alameda/alameda.paf.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				hia = overlay(c, b, n, a, fun=function(r1, r2, r3, r4){return(r1*r2*(10^-4)*(1-exp(-r3*r4)))}, filename=f2, format="GTiff", overwrite=TRUE)

				#Alameda area rate
				f3 = paste('/home/vtinney/run/results/no2/cvd.mort/paf/alameda/alameda.rate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				rate = overlay(hia, c, fun=function(r1, r2){return((r1/r2)*100000)}, filename=f3, format="GTiff", overwrite=TRUE)                                                                                                                                                                                                                                                                                                                                                                			

					for (m in 1:length(pop.total)){
					print(pop.total[m])
					r = raster(paste(poptotal,'total.alameda.pop.',pop.total[m],'.tif',sep=''))

					f4 = paste('/home/vtinney/run/results/no2/cvd.mort/paf/alameda/alameda.wrate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
					wrate100 = overlay(hia, r, fun=function(r1, r2){return(r1/r2)})

						if (pop.groups[h] == pop.total[m]){

						writeRaster(wrate100, filename=f4, format="GTiff", overwrite=TRUE)}
						else{}	
}}}}}

# ==================================================================
# Elderly, Oakland calc set up
rate.groups <- c('cbg.65', 'ct.65', 'co.65')
conc.groups <- c('larkin.ppb', 'NO2.ppb')
pop.groups <- c('ls.day.65', 'wp.day.65', 'ls.night.65', 'wp.night.65')
beta.groups <- c('eum.cvd.point.ppb', 'eum.cvd.lower.ppb', 'eum.cvd.upper.ppb')
pop.total <- c('ls.day.65', 'wp.day.65', 'ls.night.65', 'wp.night.65')

# Loop: Elderly, Oakland

for (i in 1:length(beta.groups)){
print(beta.groups[i])
n = raster(paste(betas,'beta.',beta.groups[i],'.tif',sep=''))
	
	for (j in 1:length(conc.groups)){
		print(conc.groups[j])
		a = raster(paste(concs,'conc.',conc.groups[j],'.tif',sep=''))	

		f1 = paste('/home/vtinney/run/results/no2/cvd.mort/af/oak/oak.',beta.groups[i],'.',conc.groups[j],'.tif',sep='')
		af = overlay(n, a, fun=function(r1, r2){1-exp(-r1*r2)}, filename=f1, format="GTiff", overwrite=TRUE)

		for (k in 1:length(rate.groups)){
		print(rate.groups[k])
		b = raster(paste(rates,'mortality.cvd.',rate.groups[k],'.tif',sep=''))	
			
			for (h in 1:length(pop.groups)){
			print(pop.groups[h])
			c = raster(paste(pops,'oak.pop.',pop.groups[h],'.tif',sep=''))
			
				#Oakland area PAF
	
				f2 = paste('/home/vtinney/run/results/no2/cvd.mort/paf/oak/oak.paf.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				hia = overlay(c, b, n, a, fun=function(r1, r2, r3, r4){return(r1*r2*(10^-4)*(1-exp(-r3*r4)))}, filename=f2, format="GTiff", overwrite=TRUE)

				#Oakland area rate
				f3 = paste('/home/vtinney/run/results/no2/cvd.mort/paf/oak/oak.rate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				rate = overlay(hia, c, fun=function(r1, r2){return((r1/r2)*100000)}, filename=f3, format="GTiff", overwrite=TRUE)                                                                                                                                                                                                                                                                                                                                                                			

					for (m in 1:length(pop.total)){
					print(pop.total[m])
					r = raster(paste(poptotal,'total.oak.pop.',pop.total[m],'.tif',sep=''))

					f4 = paste('/home/vtinney/run/results/no2/cvd.mort/paf/oak/oak.wrate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
					wrate100 = overlay(hia, r, fun=function(r1, r2){return(r1/r2)})

						if (pop.groups[h] == pop.total[m]){

						writeRaster(wrate100, filename=f4, format="GTiff", overwrite=TRUE)}
						else{}	
}}}}}

# ==============================================================================================================================================
# Part 3 - No2 and Asthma ER visits

# ==================================================================
# All ages, Bay calc set up - Asthma ER visits
rate.groups <- c('zip.all')
conc.groups <- c('larkin.ppb')
pop.groups <- c('ls.day', 'wp.2016', 'ls.night')
beta.groups <- c('ore.all.point.ppb', 'ore.all.lower.ppb', 'ore.all.upper.ppb', 'zheng.all.point.ppb', 'zheng.all.lower.ppb', 'zheng.all.upper.ppb')
pop.total <- c('ls.day', 'wp.2016', 'ls.night')

# Loop: All ages, Bay

for (i in 1:length(beta.groups)){
print(beta.groups[i])
n = raster(paste(betas,'beta.',beta.groups[i],'.tif',sep=''))	

	for (j in 1:length(conc.groups)){
		print(conc.groups[j])
		a = raster(paste(concs,'conc.',conc.groups[j],'.tif',sep=''))
		
		for (k in 1:length(rate.groups)){
		print(rate.groups[k])
		b = raster(paste(rates,'asthma.er.',rate.groups[k],'.tif',sep=''))	

			for (h in 1:length(pop.groups)){
			print(pop.groups[h])
			c = raster(paste(pops,'bay.pop.',pop.groups[h],'.tif',sep=''))

				#Bay area PAF
	
				f2 = paste('/home/vtinney/run/results/no2/asthma.er/paf/bay/bay.paf.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				hia = overlay(c, b, n, a, fun=function(r1, r2, r3, r4){return(r1*r2*(10^-4)*(1-exp(-r3*r4)))}, filename=f2, format="GTiff", overwrite=TRUE)

				#Bay area rate
				f3 = paste('/home/vtinney/run/results/no2/asthma.er/paf/bay/bay.rate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				rate = overlay(hia, c, fun=function(r1, r2){return((r1/r2)*100000)}, filename=f3, format="GTiff", overwrite=TRUE)                                                                                                                                                                                                                                                                                                                                                                			

					for (m in 1:length(pop.total)){
					print(pop.total[m])
					r = raster(paste(poptotal,'total.bay.',pop.total[m],'.tif',sep=''))

					f4 = paste('/home/vtinney/run/results/no2/asthma.er/paf/bay/bay.wrate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
					wrate100 = overlay(hia, r, fun=function(r1, r2){return(r1/r2)})

						if (pop.groups[h] == pop.total[m]){

						writeRaster(wrate100, filename=f4, format="GTiff", overwrite=TRUE)}
						else{}	
}}}}}

# ==================================================================
# All ages, Alameda calc set up - Asthma ER visits
rate.groups <- c('zip.all')
conc.groups <- c('larkin.ppb')
pop.groups <- c('ls.day', 'wp.2016', 'ls.night')
beta.groups <- c('ore.all.point.ppb', 'ore.all.lower.ppb', 'ore.all.upper.ppb', 'zheng.all.point.ppb', 'zheng.all.lower.ppb', 'zheng.all.upper.ppb')
pop.total <- c('ls.day', 'wp.2016', 'ls.night')

# Loop: All ages, Alameda

for (i in 1:length(beta.groups)){
print(beta.groups[i])
n = raster(paste(betas,'beta.',beta.groups[i],'.tif',sep=''))	

	for (j in 1:length(conc.groups)){
		print(conc.groups[j])
		a = raster(paste(concs,'conc.',conc.groups[j],'.tif',sep=''))		
	
		for (k in 1:length(rate.groups)){
		print(rate.groups[k])
		b = raster(paste(rates,'asthma.er.',rate.groups[k],'.tif',sep=''))
			
			for (h in 1:length(pop.groups)){
			print(pop.groups[h])
			c = raster(paste(pops,'alameda.pop.',pop.groups[h],'.tif',sep=''))

				#Alameda area PAF
	
				f2 = paste('/home/vtinney/run/results/no2/asthma.er/paf/alameda/alameda.paf.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				hia = overlay(c, b, n, a, fun=function(r1, r2, r3, r4){return(r1*r2*(10^-4)*(1-exp(-r3*r4)))}, filename=f2, format="GTiff", overwrite=TRUE)

				#Alameda area rate
				f3 = paste('/home/vtinney/run/results/no2/asthma.er/paf/alameda/alameda.rate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				rate = overlay(hia, c, fun=function(r1, r2){return((r1/r2)*100000)}, filename=f3, format="GTiff", overwrite=TRUE)                                                                                                                                                                                                                                                                                                                                                                			

					for (m in 1:length(pop.total)){
					print(pop.total[m])
					r = raster(paste(poptotal,'total.alameda.pop.',pop.total[m],'.tif',sep=''))

					f4 = paste('/home/vtinney/run/results/no2/asthma.er/paf/alameda/alameda.wrate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
					wrate100 = overlay(hia, r, fun=function(r1, r2){return(r1/r2)})

						if (pop.groups[h] == pop.total[m]){

						writeRaster(wrate100, filename=f4, format="GTiff", overwrite=TRUE)}
						else{}	
}}}}}

# ==================================================================
# All ages, Oakland calc set up - Asthma ER visits
rate.groups <- c('zip.all')
conc.groups <- c('larkin.ppb', 'NO2.ppb')
pop.groups <- c('ls.day', 'wp.2016', 'ls.night')
beta.groups <- c('ore.all.point.ppb', 'ore.all.lower.ppb', 'ore.all.upper.ppb', 'zheng.all.point.ppb', 'zheng.all.lower.ppb', 'zheng.all.upper.ppb')
pop.total <- c('ls.day', 'wp.2016', 'ls.night')

# Loop: All ages, Oakland

for (i in 1:length(beta.groups)){
print(beta.groups[i])
n = raster(paste(betas,'beta.',beta.groups[i],'.tif',sep=''))	

	for (j in 1:length(conc.groups)){
		print(conc.groups[j])
		a = raster(paste(concs,'conc.',conc.groups[j],'.tif',sep=''))		

		for (k in 1:length(rate.groups)){
		print(rate.groups[k])
		b = raster(paste(rates,'asthma.er.',rate.groups[k],'.tif',sep=''))	
			
			for (h in 1:length(pop.groups)){
			print(pop.groups[h])
			c = raster(paste(pops,'oak.pop.',pop.groups[h],'.tif',sep=''))

				#Oakland area PAF
	
				f2 = paste('/home/vtinney/run/results/no2/asthma.er/paf/oak/oak.paf.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				hia = overlay(c, b, n, a, fun=function(r1, r2, r3, r4){return(r1*r2*(10^-4)*(1-exp(-r3*r4)))}, filename=f2, format="GTiff", overwrite=TRUE)

				#Oakland area rate
				f3 = paste('/home/vtinney/run/results/no2/asthma.er/paf/oak/oak.rate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				rate = overlay(hia, c, fun=function(r1, r2){return((r1/r2)*100000)}, filename=f3, format="GTiff", overwrite=TRUE)                                                                                                                                                                                                                                                                                                                                                                			

					for (m in 1:length(pop.total)){
					print(pop.total[m])
					r = raster(paste(poptotal,'total.oak.pop.',pop.total[m],'.tif',sep=''))

					f4 = paste('/home/vtinney/run/results/no2/asthma.er/paf/oak/oak.wrate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
					wrate100 = overlay(hia, r, fun=function(r1, r2){return(r1/r2)})

						if (pop.groups[h] == pop.total[m]){

						writeRaster(wrate100, filename=f4, format="GTiff", overwrite=TRUE)}
						else{}	
}}}}}

# ==================================================================
# Pediatric, Bay calc set up - Asthma ER visits
rate.groups <- c('zip.17')
conc.groups <- c('larkin.ppb')
pop.groups <- c('ls.day.17', 'wp.day.17', 'ls.night.17', 'wp.night.17')
beta.groups <- c('ore.17.point.ppb', 'ore.17.lower.ppb', 'ore.17.upper.ppb', 'zheng.17.point.ppb', 'zheng.17.lower.ppb', 'zheng.17.upper.ppb')
pop.total <- c('ls.day.17', 'wp.day.17', 'ls.night.17', 'wp.night.17')

# Loop: All ages, Bay

for (i in 1:length(beta.groups)){
print(beta.groups[i])
n = raster(paste(betas,'beta.',beta.groups[i],'.tif',sep=''))	

	for (j in 1:length(conc.groups)){
		print(conc.groups[j])
		a = raster(paste(concs,'conc.',conc.groups[j],'.tif',sep=''))
		
		for (k in 1:length(rate.groups)){
		print(rate.groups[k])
		b = raster(paste(rates,'asthma.er.',rate.groups[k],'.tif',sep=''))	

			for (h in 1:length(pop.groups)){
			print(pop.groups[h])
			c = raster(paste(pops,'bay.pop.',pop.groups[h],'.tif',sep=''))

				#Bay area PAF
	
				f2 = paste('/home/vtinney/run/results/no2/asthma.er/paf/bay/bay.paf.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				hia = overlay(c, b, n, a, fun=function(r1, r2, r3, r4){return(r1*r2*(10^-4)*(1-exp(-r3*r4)))}, filename=f2, format="GTiff", overwrite=TRUE)

				#Bay area rate
				f3 = paste('/home/vtinney/run/results/no2/asthma.er/paf/bay/bay.rate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				rate = overlay(hia, c, fun=function(r1, r2){return((r1/r2)*100000)}, filename=f3, format="GTiff", overwrite=TRUE)                                                                                                                                                                                                                                                                                                                                                                			

					for (m in 1:length(pop.total)){
					print(pop.total[m])
					r = raster(paste(poptotal,'total.bay.pop.',pop.total[m],'.tif',sep=''))

					f4 = paste('/home/vtinney/run/results/no2/asthma.er/paf/bay/bay.wrate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
					wrate100 = overlay(hia, r, fun=function(r1, r2){return(r1/r2)})

						if (pop.groups[h] == pop.total[m]){

						writeRaster(wrate100, filename=f4, format="GTiff", overwrite=TRUE)}
						else{}	
}}}}}

# ==================================================================
# Pediatric ER asthma, Alameda calc set up
rate.groups <- c('zip.17')
conc.groups <- c('larkin.ppb')
pop.groups <- c('ls.day.17', 'wp.day.17', 'ls.night.17', 'wp.night.17')
beta.groups <- c('ore.17.point.ppb', 'ore.17.lower.ppb', 'ore.17.upper.ppb', 'zheng.17.point.ppb', 'zheng.17.lower.ppb', 'zheng.17.upper.ppb')
pop.total <- c('ls.day.17', 'wp.day.17', 'ls.night.17', 'wp.night.17')

# Loop: All ages, Alameda

for (i in 1:length(beta.groups)){
print(beta.groups[i])
n = raster(paste(betas,'beta.',beta.groups[i],'.tif',sep=''))	

	for (j in 1:length(conc.groups)){
		print(conc.groups[j])
		a = raster(paste(concs,'conc.',conc.groups[j],'.tif',sep=''))		
	
		for (k in 1:length(rate.groups)){
		print(rate.groups[k])
		b = raster(paste(rates,'asthma.er.',rate.groups[k],'.tif',sep=''))
			
			for (h in 1:length(pop.groups)){
			print(pop.groups[h])
			c = raster(paste(pops,'alameda.pop.',pop.groups[h],'.tif',sep=''))

				#Alameda area PAF
	
				f2 = paste('/home/vtinney/run/results/no2/asthma.er/paf/alameda/alameda.paf.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				hia = overlay(c, b, n, a, fun=function(r1, r2, r3, r4){return(r1*r2*(10^-4)*(1-exp(-r3*r4)))}, filename=f2, format="GTiff", overwrite=TRUE)

				#Alameda area rate
				f3 = paste('/home/vtinney/run/results/no2/asthma.er/paf/alameda/alameda.rate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				rate = overlay(hia, c, fun=function(r1, r2){return((r1/r2)*100000)}, filename=f3, format="GTiff", overwrite=TRUE)                                                                                                                                                                                                                                                                                                                                                                			

					for (m in 1:length(pop.total)){
					print(pop.total[m])
					r = raster(paste(poptotal,'total.alameda.pop.',pop.total[m],'.tif',sep=''))

					f4 = paste('/home/vtinney/run/results/no2/asthma.er/paf/alameda/alameda.wrate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
					wrate100 = overlay(hia, r, fun=function(r1, r2){return(r1/r2)})

						if (pop.groups[h] == pop.total[m]){

						writeRaster(wrate100, filename=f4, format="GTiff", overwrite=TRUE)}
						else{}	
}}}}}

# ==================================================================
# Pediatric, Oakland calc set up - Asthma ER visits
rate.groups <- c('zip.17')
conc.groups <- c('larkin.ppb', 'NO2.ppb')
pop.groups <- c('ls.day.17', 'wp.day.17', 'ls.night.17', 'wp.night.17')
beta.groups <- c('ore.17.point.ppb', 'ore.17.lower.ppb', 'ore.17.upper.ppb', 'zheng.17.point.ppb', 'zheng.17.lower.ppb', 'zheng.17.upper.ppb')
pop.total <- c('ls.day.17', 'wp.day.17', 'ls.night.17', 'wp.night.17')

# Loop: All ages, Oakland

for (i in 1:length(beta.groups)){
print(beta.groups[i])
n = raster(paste(betas,'beta.',beta.groups[i],'.tif',sep=''))	

	for (j in 1:length(conc.groups)){
		print(conc.groups[j])
		a = raster(paste(concs,'conc.',conc.groups[j],'.tif',sep=''))		

		for (k in 1:length(rate.groups)){
		print(rate.groups[k])
		b = raster(paste(rates,'asthma.er.',rate.groups[k],'.tif',sep=''))	
			
			for (h in 1:length(pop.groups)){
			print(pop.groups[h])
			c = raster(paste(pops,'oak.pop.',pop.groups[h],'.tif',sep=''))

				#Oakland area PAF
	
				f2 = paste('/home/vtinney/run/results/no2/asthma.er/paf/oak/oak.paf.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				hia = overlay(c, b, n, a, fun=function(r1, r2, r3, r4){return(r1*r2*(10^-4)*(1-exp(-r3*r4)))}, filename=f2, format="GTiff", overwrite=TRUE)

				#Oakland area rate
				f3 = paste('/home/vtinney/run/results/no2/asthma.er/paf/oak/oak.rate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				rate = overlay(hia, c, fun=function(r1, r2){return((r1/r2)*100000)}, filename=f3, format="GTiff", overwrite=TRUE)                                                                                                                                                                                                                                                                                                                                                                			

					for (m in 1:length(pop.total)){
					print(pop.total[m])
					r = raster(paste(poptotal,'total.oak.pop.',pop.total[m],'.tif',sep=''))

					f4 = paste('/home/vtinney/run/results/no2/asthma.er/paf/oak/oak.wrate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
					wrate100 = overlay(hia, r, fun=function(r1, r2){return(r1/r2)})

						if (pop.groups[h] == pop.total[m]){

						writeRaster(wrate100, filename=f4, format="GTiff", overwrite=TRUE)}
						else{}	
}}}}}


# ==============================================================================================================================================
# Part 4 - No2 and Asthma incidence (pediatric only)
#===============================================================================================================================================
# Pediatric, Bay calc set up - Asthma incidence
rate.groups <- c('100m.17')
conc.groups <- c('larkin.ppb')
pop.groups <- c('ls.day.17', 'wp.day.17', 'ls.night.17', 'wp.night.17')
beta.groups <- c('khr.17.point.ppb', 'khr.17.lower.ppb', 'khr.17.upper.ppb')
pop.total <- c('ls.day.17', 'wp.day.17', 'ls.night.17', 'wp.night.17')

# Loop: All ages, Bay

for (i in 1:length(beta.groups)){
print(beta.groups[i])
n = raster(paste(betas,'beta.',beta.groups[i],'.tif',sep=''))	

	for (j in 1:length(conc.groups)){
		print(conc.groups[j])
		a = raster(paste(concs,'conc.',conc.groups[j],'.tif',sep=''))
		
		for (k in 1:length(rate.groups)){
		print(rate.groups[k])
		b = raster(paste(rates,'asthma.inc.',rate.groups[k],'.tif',sep=''))	

			for (h in 1:length(pop.groups)){
			print(pop.groups[h])
			c = raster(paste(pops,'bay.pop.',pop.groups[h],'.tif',sep=''))

				#Bay area PAF
	
				f2 = paste('/home/vtinney/run/results/no2/asthma.inc/paf/bay/bay.paf.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				hia = overlay(c, b, n, a, fun=function(r1, r2, r3, r4){return(r1*r2*(10^-4)*(1-exp(-r3*r4)))}, filename=f2, format="GTiff", overwrite=TRUE)

				#Bay area rate
				f3 = paste('/home/vtinney/run/results/no2/asthma.inc/paf/bay/bay.rate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				rate = overlay(hia, c, fun=function(r1, r2){return((r1/r2)*100000)}, filename=f3, format="GTiff", overwrite=TRUE)                                                                                                                                                                                                                                                                                                                                                                			

					for (m in 1:length(pop.total)){
					print(pop.total[m])
					r = raster(paste(poptotal,'total.bay.pop.',pop.total[m],'.tif',sep=''))

					f4 = paste('/home/vtinney/run/results/no2/asthma.inc/paf/bay/bay.wrate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
					wrate100 = overlay(hia, r, fun=function(r1, r2){return(r1/r2)})

						if (pop.groups[h] == pop.total[m]){

						writeRaster(wrate100, filename=f4, format="GTiff", overwrite=TRUE)}
						else{}	
}}}}}

# ==================================================================
# Pediatric incidence asthma, Alameda calc set up
rate.groups <- c('100m.17')
conc.groups <- c('larkin.ppb')
pop.groups <- c('ls.day.17', 'wp.day.17', 'ls.night.17', 'wp.night.17')
beta.groups <- c('khr.17.point.ppb', 'khr.17.lower.ppb', 'khr.17.upper.ppb')
pop.total <- c('ls.day.17', 'wp.day.17', 'ls.night.17', 'wp.night.17')

# Loop: All ages, Alameda

for (i in 1:length(beta.groups)){
print(beta.groups[i])
n = raster(paste(betas,'beta.',beta.groups[i],'.tif',sep=''))	

	for (j in 1:length(conc.groups)){
		print(conc.groups[j])
		a = raster(paste(concs,'conc.',conc.groups[j],'.tif',sep=''))		
	
		for (k in 1:length(rate.groups)){
		print(rate.groups[k])
		b = raster(paste(rates,'asthma.inc.',rate.groups[k],'.tif',sep=''))
			
			for (h in 1:length(pop.groups)){
			print(pop.groups[h])
			c = raster(paste(pops,'alameda.pop.',pop.groups[h],'.tif',sep=''))

				#Alameda area PAF
	
				f2 = paste('/home/vtinney/run/results/no2/asthma.inc/paf/alameda/alameda.paf.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				hia = overlay(c, b, n, a, fun=function(r1, r2, r3, r4){return(r1*r2*(10^-4)*(1-exp(-r3*r4)))}, filename=f2, format="GTiff", overwrite=TRUE)

				#Alameda area rate
				f3 = paste('/home/vtinney/run/results/no2/asthma.inc/paf/alameda/alameda.rate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				rate = overlay(hia, c, fun=function(r1, r2){return((r1/r2)*100000)}, filename=f3, format="GTiff", overwrite=TRUE)                                                                                                                                                                                                                                                                                                                                                                			

					for (m in 1:length(pop.total)){
					print(pop.total[m])
					r = raster(paste(poptotal,'total.alameda.pop.',pop.total[m],'.tif',sep=''))

					f4 = paste('/home/vtinney/run/results/no2/asthma.inc/paf/alameda/alameda.wrate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
					wrate100 = overlay(hia, r, fun=function(r1, r2){return(r1/r2)})

						if (pop.groups[h] == pop.total[m]){

						writeRaster(wrate100, filename=f4, format="GTiff", overwrite=TRUE)}
						else{}	
}}}}}

# ==================================================================
# Pediatric, Oakland calc set up - Asthma incidence
rate.groups <- c('100m.17')
conc.groups <- c('larkin.ppb', 'NO2.ppb')
pop.groups <- c('ls.day.17', 'wp.day.17', 'ls.night.17', 'wp.night.17')
beta.groups <- c('khr.17.point.ppb', 'khr.17.lower.ppb', 'khr.17.upper.ppb')
pop.total <- c('ls.day.17', 'wp.day.17', 'ls.night.17', 'wp.night.17')

# Loop: All ages, Oakland

for (i in 1:length(beta.groups)){
print(beta.groups[i])
n = raster(paste(betas,'beta.',beta.groups[i],'.tif',sep=''))	

	for (j in 1:length(conc.groups)){
		print(conc.groups[j])
		a = raster(paste(concs,'conc.',conc.groups[j],'.tif',sep=''))		

		for (k in 1:length(rate.groups)){
		print(rate.groups[k])
		b = raster(paste(rates,'asthma.inc.',rate.groups[k],'.tif',sep=''))	
			
			for (h in 1:length(pop.groups)){
			print(pop.groups[h])
			c = raster(paste(pops,'oak.pop.',pop.groups[h],'.tif',sep=''))

				#Oakland area PAF
	
				f2 = paste('/home/vtinney/run/results/no2/asthma.inc/paf/oak/oak.paf.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				hia = overlay(c, b, n, a, fun=function(r1, r2, r3, r4){return(r1*r2*(10^-4)*(1-exp(-r3*r4)))}, filename=f2, format="GTiff", overwrite=TRUE)

				#Oakland area rate
				f3 = paste('/home/vtinney/run/results/no2/asthma.inc/paf/oak/oak.rate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
				rate = overlay(hia, c, fun=function(r1, r2){return((r1/r2)*100000)}, filename=f3, format="GTiff", overwrite=TRUE)                                                                                                                                                                                                                                                                                                                                                                			

					for (m in 1:length(pop.total)){
					print(pop.total[m])
					r = raster(paste(poptotal,'total.oak.pop.',pop.total[m],'.tif',sep=''))

					f4 = paste('/home/vtinney/run/results/no2/asthma.inc/paf/oak/oak.wrate.',beta.groups[i],'.',rate.groups[k],'.',pop.groups[h],'.',conc.groups[j],'.tif',sep='')
					wrate100 = overlay(hia, r, fun=function(r1, r2){return(r1/r2)})

						if (pop.groups[h] == pop.total[m]){

						writeRaster(wrate100, filename=f4, format="GTiff", overwrite=TRUE)}
						else{}	
}}}}}
