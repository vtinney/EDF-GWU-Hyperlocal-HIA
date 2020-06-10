
# Bay area local health impact assessment of air pollution
### Environmental Defense Fund (EDF) and the George Washington University (GWU) Milken Institute School of Public Health, Department of Environmental and Occupational Health.

###### Assessing the distribution of air pollution health risks within cities: a neighborhood-scale analysis leveraging high resolution datasets in the Bay area, California. Authors: Veronica A. Southerland, Susan C. Anenberg, Maria Harris, Ananya Roy, Joshua Apte, Perry Hystad, Alina Vodonos, Joel Schwartz. In prep.

Contact for this repository: Veronica Southerland, vtinney@gwu.edu

This repository provides the base code for the health impact calculations for the Bay area health impact assessment for fine particulate matter (PM2.5), nitrogen dioxide (NO2) and black carbon (BC). 

The framework for the code is as follows:

* **Health impact calculations**. Code should be run in the following order:

  * Bay_01.*concentration*.estimates.R - These files provide the health impact function calculation and maps, run in R/3.5.3. Within the code, results are aggregated to the census block group (CBG), city and county level, with results exported to a dataframe (csv).
  
  * Bay_02.*concentration*.df.R - These files first extract summary statistics from all output rasters created in Bay_01 files. They then bring in the output csv files created in Bay_01 and clean the data.
  
  * Bay_03.*concentration*.long.df.R - These files convert the csv files created in Bay_02, further format the data, and spread the estimates across columns, such that they are more easily input into tables.

 
 * **Intermediary Files** (IF)
    * IF files are provided for informational use only. Contact Veronica Southerland (vtinney@gwu.edu) for questions on generating intermediary files.
 
    * Bay_IF_Create asthma ER rate: This file creates the dataframe file for asthma ER visits.
  
    * Bay_IF_Create population fractions based on GPW (25-99 and 65-99 years): This file creates population fraction files for the age groups used in the analysis.
  
    * Bay_IF_Use gdalwarp to put all input files on the same resolution and extent: In order to the run the code, all rasters need to be in the same extent, CRS and resolution. This file reprojects all input files to the same characteristics.
    
### **Input files** (branch)
All files to run the code in steps Bay_01-03 is available in the inputs file branch.
   
All read-in files are rasters with the following characteristics: 
2367, 2909, 6885603  (nrow, ncol, ncell)
-123.6325, -121.2083, 36.8925, 38.865  (xmin, xmax, ymin, ymax)
0.0008333333, 0.0008333333  (x, y)
CRS: +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0

**Baseline disease rates**
* **asthma.er.zip.17.tif**: Asthma ER rates, ages 0-17 years, California Department of Public Health (CDPH) 
* **asthma.er.zip.all.tif**: Asthma ER rates, all ages, California Department of Public Health (CDPH) 
* **cbg.25.tif**: All-cause mortality by census block group (CBG), ages 25-99 years, Alameda County Public Health Department (ACPHD) 
* **cbg.65.tif**: All-cause mortality by census block group (CBG), ages 65-99 years, Alameda County Public Health Department (ACPHD) 
* **cvd.cbg.25.tif**: Cardiovascular (CVD) mortality by census block group (CBG), ages 25-99 years, Alameda County Public Health Department (ACPHD) 
* **cvd.cbg.65.tif**: CVD mortality by census block group (CBG), ages 65-99 years, Alameda County Public Health Department (ACPHD) 
* **co.25.tif**: All-cause mortality by County, ages 25-99 years, CDC Wonder (2016)
* **co.65.tif**: All-cause mortality by County, ages 65-99 years, CDC Wonder (2016)
* **cvd.co.25.tif**: Cardiovascular (CVD) mortality by County, ages 25-99 years, CDC Wonder (2016)
* **cvd.co.65.tif**: CVD mortality by County, ages 65-99 years, CDC Wonder (2016)

**Pollutant Concentrations**
* **conc.BC.ug.tif**: Black carbon from Google Street View (ug/m3)
* **conc.NO2.ppb.tif**: Nitrogen dioxide from Google Street View (ppb)
* **conc.larkin.ppb.tif**: Nitrogen dioxide from Larkin et al. 2017 (ppb)
* **conc.no2.centroids.2.tif**: Nitrogen dioxide from Bechle et al. 2015 (ppb)
* **conc.vd.na.2016.tif: Fine particulate matter from van Donkelaar et al. 2019 (North American concentrations, ug/m3)
* **conc.vd_mean.15.16.tif: Fine particulate matter from van Donkelaar et al. 2016, mean of 2015-2016 (global concentrations, ug/m3)

**Population**
* **pop.ls.night.17.tif**: LandScan USA, 2017, and Gridded Population of the World v4, 2010, ages 0-17 years
* **pop.ls.night.25.tif**: LandScan USA, 2017, and Gridded Population of the World v4, 2010, ages 25-99 years
* **pop.ls.night.65.tif**: LandScan USA, 2017, and Gridded Population of the World v4, 2010, ages 65-99 years
* **pop.ls.night.tif**: LandScan USA, 2017
