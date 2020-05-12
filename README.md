# Catalyzing Police Reform with Data 

In 2018, the National Neighborhood Indicators Partnership (NNIP) and Microsoft launched a partnership to spur more data-driven and community-led criminal justice reform efforts, with the goal of building trust with law enforcement and improving public safety. As one of the activities under this partnership, we selected one city – Los Angeles, California – to explore how publicly-available police data can be analyzed to create a comprehensive measure of community-police engagement.  

We used cluster analysis to develop a typology that elucidates the relationship between resident-initiated and police-initiated activity in the City of Los Angeles, as well as how that relationship varies across neighborhoods, by synthesizing data sources on calls for service, stops, arrests, and crime. We find that resident calls for service and police stops and arrests generally increase together, and that neighborhoods with high amounts of activity tend to have a greater proportion of violent crime and serious calls for service. The neighborhoods with high activity also tend to have wider racial disparities in stops and arrests, and more economic hardship.

This repository provides the code behind this analysis, which was published by the Urban Institute in the [*Catalyzing Police Reform with Data: Policing Typology for Los Angeles Neighborhoods*](https://www.urban.org/research/publication/catalyzing-policing-reform-data) brief in May, 2020. 

More information on the methodolgy used in this analysis can be found in the [*Catalyzing Police Reform with Data: Technical Appendix*](https://www.urban.org/research/publication/catalyzing-policing-reform-data-technical-appendix) published by the Urban Institute.

This analysis was sponsored by Microsoft. We collaborated closely in the design, analysis, and interpretation of the findings with the Microsoft Criminal Justice Reform team, the Microsoft Data Science team, and the local NNIP partner at University of Southern California’s Sol Price Center for Social Innovation. 

This analysis was conducted in RStudio using R v 3.6.1. 

## Data Sources

We use the following data sources for this project:

LAPD open data on crime, arrests, calls, and stops:
- [Crime: 2010-2019](https://data.lacity.org/A-Safe-City/Crime-Data-from-2010-to-2019/63jg-8b9z)
- [Arrests: 2010-Present](https://data.lacity.org/A-Safe-City/Arrest-Data-from-2010-to-Present/yru6-6re4)
- [Calls for Service: 2018](https://data.lacity.org/A-Safe-City/LAPD-Calls-for-Service-2018/nayp-w2tw)
- [Stops: 2010-Present](https://data.lacity.org/A-Safe-City/Vehicle-and-Pedestrian-Stop-Data-2010-to-Present/ci25-wgt7)

American Community Survey data on neighborhood demographics:
- USC [Neighborhood Data for Social Change](https://usc.data.socrata.com/stories/s/envs-pjdv/) project. 

HUD USPS data on residential and business addresses:
- [HUD USPS vacancy data](https://www.huduser.gov/portal/datasets/usps.html)

All of the raw source data files that the authors used to create the analytical data set are included in the `data/` directory, with the exception of the HUD USPS data which is restricted access and must be downloaded from the HUD website at the link above. We include these static datasets to enable replication as the LAPD open data sources are frequently updated on the [LA Open Data Portal](https://data.lacity.org/). We observed that these updates can alter historical data so pulling the data on different dates has yielded different 2018 totals. The static dataset file names are:
- Crime: `data/Crime_Data_from_2010_to_Present.csv`
- Arrests: `data/Arrest_Data_from_2010_to_Present.csv`
- Calls for Service: `data/CFS/LAPD_Calls_for_Service_2018.csv`
- Stops: `data/Vehicle_and_Pedestrian_Stop_data_2010_to_Present.csv`
- ACS:  This ACS file represents the reporting district-level demographic variables created by cross-walking the census tract-level raw ACS data to reporting districts based on the methodology described in the technical appendix using ArcGIS. The output file with the tract-to-reporting district weights can be found in `data/rd_tract_xwalk.csv`.
- Reporting District Information: `data/LAPD_Reporting_Districts.csv`
If you want to replicate this analysis with the most up-to-date LAPD data, you can access the data via the LA Open Data Portal API at the source links given above.

## Getting Started

Prior to running the analysis, you will need to install [R](https://cran.rstudio.com/) and [RStudio](https://rstudio.com/products/rstudio/download/) on your computer if you do not have this software installed. 

This analysis requires that the following R packages be installed:

```
"skimr", "lubridate", "janitor", "knitr", "tidyverse", "psych", "cluster", "factoextra", "gridExtra", "here", "rlang", "sf", "devtools", "urbnthemes", "mclust", "dbscan", "data.table", "R.utils"
```

Installation and import for these required packages is handled in the beginning of `01_lapd-data-processing.R` and the first code chunk of `02_final-cluster-analysis.Rmd`. The data visualizations for this analysis are produced using the `ggplot2` package, styled using the [urbnthemes](https://github.com/UrbanInstitute/urbnthemes) package. Note that the Urban Institute uses [Lato](https://fonts.google.com/specimen/Lato) font for publications. After installing `urbnthemes`, submit `urbnthemes::lato_test()` to see if Lato is imported and registered.

If Lato isn’t imported and registered, install [Lato](https://fonts.google.com/specimen/Lato) and then submit
`urbnthemes::lato_install()`. If you are on a Windows, you may need to install [ghostscript](https://www.ghostscript.com/download.html) and
then submit `Sys.setenv(R_GSCMD = "link to the ghostscript .exe")` before running `urbnthemes::lato_install()`. 

Always load `library(urbnthemes)` after `library(ggplot2)` or `library(tidyverse)`. This is already handled in the `02_final-cluster-analysis.Rmd` file.

## Replicating the Analysis 

To run the analysis, perform the following steps:
1. Clone this repository on your local machine. 
2. Download the HUD USPS Vacancy data from the [HUD website](https://www.huduser.gov/portal/datasets/usps.html) and save the data as `usps_data.csv.gz` in the `data/` directory. Note that the HUD USPS data is restricted access and requires registration. From the HUD USPS data [registration page](https://www.huduser.gov/portal/usps/registration.html): 
```
Under the current agreement with the USPS, HUD can only grant access to the vacancy data to governmental and non-profit organizations. In order to be granted access as a user, a requestor must be verified as a member of a non-profit or governmental organization.
```
3. Open the R project `la-policing-typology.Rproj` in RStudio. 
4. Open the `etl/01_lapd-data-processing.R` file in RStudio and run the file to create the analytical dataset `analysis_ready_by_district_final.csv`. 
5. Open the `analysis/02_final-cluster-analysis.Rmd` file in RStudio and knit to html. When kniting, the code will check whether the necessary packages are installed and install any packages that are not already installed. Results tables will be written to the `results/` directory and plots will be written to the `images/` directory. Note that `analysis/02_final-cluster-analysis.Rmd` uses a global `trial_name` variable to enable the researchers to test multiple different models, each defined by a separate `trial_name` during the analysis process. This variable is set to `final` by default. All outputs in the `results/` and `images/` directory will use the `trial_name` as a prefix.

The `etl/` director contains the key file (`01_lapd-data-processing.R`) used for preprocessing the raw data sources to create to create the analytical data set.

The `analysis/` directory contains the key files used for analysis which are:
- `02_final-cluster-analysis.Rmd`: file containing data pre-processing, cluster analysis, and analysis of results.
- `cluster-functions.R`: file containing functions for cluster analysis including variable weighting, cluster modeling, and stability analysis.
- `data_viz_functions.R`: file containing functions for data visualization of cluster analysis results.

## Getting help

Contact [Alena Stern](astern@urban.org) with feedback or questions.

## License

This software is licensed under the terms of the [MIT License](https://github.com/UI-Research/nnip-ms-la-policing/blob/master/license.txt).