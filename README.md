
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
  
 * **Input files**
   * All files to run the code in steps Bay_01-03 is available in the inputs file folder. To run the code, the user needs to download all input files and change the working directories within the code.
 
 * **Intermediary Files** (IF)
    * IF files are provided for informational use only. Contact Veronica Southerland (vtinney@gwu.edu) for questions on generating intermediary files.
 
    * Bay_IF_Create asthma ER rate: This file creates the dataframe file for asthma ER visits.
  
    * Bay_IF_Create population fractions based on GPW (25-99 and 65-99 years): This file creates population fraction files for the age groups used in the analysis.
  
    * Bay_IF_Use gdalwarp to put all input files on the same resolution and extent: In order to the run the code, all rasters need to be in the same extent, CRS and resolution. This file reprojects all input files to the same characteristics.
