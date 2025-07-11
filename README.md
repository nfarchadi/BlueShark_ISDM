# Data and code for "Integrating diverse marine predator data for robust species distribution models in a dynamic ocean"
DOI: 10.1093/icesjms/fsaf110

This repository contains data and code to:

1. Run cross-validation for each integration approach (data pooling, ensemble, integrated SDM with constant spatial effect, and integrated SDM with seasonal spatial effects) and measure performance metrics (predictive skill & computational demand).
2. Develop full models for each integration approach using all the data.
3. Figures from the manuscript.

The data in this repository represent blue shark presence and pseudo-absences throughout the North Atlantic, used to construct the different integration approaches presented in the paper. Each of the datasets have about 1:1 presence:pseudo-absence ratio. 

**NOTE:**  The data provided here are randomly sampled versions of each dataset, equal in size, and are intended as examples. Users should be aware that results will differ from those presented in the manuscript, which used the full and downsampled datasets. 

The full versions of each can be accessed or requested by the data providers. Specifically, marker tag data used in this research are publicly available from the International Commission for the Conservation of Atlantic Tunas (ICCAT) Secretariat tag database at https://iccat.int/en/accesingdb.html, under “BSH” in the “Tagging” section. The fishery dependent observer dataset used in this study are considered confidential under the U.S. Magnuson-Stevens Act: qualified researchers may request these data from the NOAA Pelagic Observer Program office by contacting [popobserver@noaa.gov](mailto:popobserver@noaa.gov); we requested data representing all pelagic longline sets between the years 1993 and 2019. The electronic tag data used in this study belongs to multiple contributing authors. Interested users should contact the corresponding author to facilitate communication with the data contributors.


## Description of the data and file structure

*Data File Details*

Details for: `bsh_data_subset.csv`

* Description: a comma-delimited file containing presence and pseudo-absence locations for blue sharks in the North Atlantic Ocean and concurrent oceanographic data. 
* Format(s): .csv
* Size(s): 650 bytes
* Dimensions: 600 rows by 8 columns
* Variables:
  * long: longitude
  * lat: latitude
  * dataset: character value of the data type
  * pres_abs: a binary integer where 1 indicates presence and 0 indicates absence
  * year_mon: year-month of presence or absence
  * sst: sea surface temperature (degrees C)
  * sst_sd: spatial standard deviation of sst
  * bathy:  bathymetry, bottom depth (in meters)

*Script File Details*

Details for: `ModelPerformanceAnalysis`

* Description: contains all R code to run the cross-validation to evaluate model performance for each integration approach. Scripts in the folder call on functions from the `functions` folder to run analyses.
  * `BRT_Ensemble_Pooling_Analysis.r` - runs cross-validation for BRT data pooling and ensemble model approaches
  * `iSDM_Constant_Analysis.r` - runs cross-validation for iSDM constant model
  * `iSDM_Seasonal_Analysis.r` - runs cross-validation for iSDM seasonal model

Details for: `FullModels`

* Description: contains all the code to run the model with all the data for each integration approach. Scripts in the folder call on functions from the `functions` folder to run
  * `Full_Models.r` - fits each integration approach with all the data. BRT pooling and ensemble functions returns the BRT model. INLA functions for the iSDM constant and seasonal models returns spatial predictions, spatial predictions, Gaussian Markov Random Field (GMRF) values, marginal effect values for covariates, summary outputs, and range and variance parameters for the GMRFs

Details for: `functions`

* Description: contains wrapper functions to execute analysis in the `ModelPerformanceAnalysis` and `FullModels` folder

## Environmental data information

Environmental data was derived from the following sources:

* GLORYS oceanographic data: [https://data.marine.copernicus.eu/product/GLOBAL_MULTIYEAR_PHY_001_030/description](https://data.marine.copernicus.eu/product/GLOBAL_MULTIYEAR_PHY_001_030/description)
* Bathymetry: [https://www.ngdc.noaa.gov/mgg/global/global.html](https://www.ngdc.noaa.gov/mgg/global/global.html)

**NOTE:** Gridded GLORYS and Bathymetry environmental data for the North Atlantic for September 2014 can be found in the data folder

## Code/Software

Model performance analysis was conducted in R version 4.2.3 on the University of California, Davis high-performance computing cluster, Farm ([https://hpc.ucdavis.edu/farm-cluster](https://hpc.ucdavis.edu/farm-cluster)). Model fitting for pooling and ensemble approaches were conducted using default computational specifications in R without explicit user-defined alterations to CPU cores (i.e. 1 CPU core utilized) or RAM. For both iSDMs, model fitting during cross-validation was run in parallel on 20 CPUs with 2 GB of memory per CPU on a single node. 

**NOTE:** These specifications apply to the full datasets; computational demands will differ when using the subsetted datasets provided in this repository.

## Questions and Feedback

If you encounter with any problems or error, please create an issue here. Starting an issue will help others trying to use this code. Any other questions please contact the corresponding author, N. Farchadi (nfarchadi@sdsu.edu)