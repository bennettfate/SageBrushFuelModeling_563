## ***Abiotic and Biotic Precursors to Oregon's 2024 Record Fire Season*** 

## **Dependencies:**
(dplyr -- version 1.1.4)
(sf -- version 1.0.20)
(mapview -- 2.11.2)
(ggplot2 -- 3.5.2)
(randomForest -- 4.7.1.2)
(pdp -- 0.8.2)
(tidyr -- 1.3.1)
(caret -- 7.0.1)

## **Installation Instructions:**
Clone the files from this repo directly to your local desktop. The analysis is run in the ANALYSIS/OREGON_2024_FIRES_CLEANED/Oregon_Fires_cleaned.rmd file. All the packages to be installed are located in the first block. If these packages are not installed, use install.packages ("package name") to install neccessary packages. The file is set to pull in the data from the data files in the abiotic and biotic sub-directories. Run through the blocks of code in order to run the intial processing and random forest analysis of the data. 

## **Folder Layout:** 
ANALYSIS - R files for analyzing data. Cleaned file is the file to use. The biotic and abiotic subfolders house data that is used in the analysis and is designed to be pulled directly from code without needing to change it. 
  -ABIOTIC - holds data files with fire perimeters and abiotic fire conditions.
  -BIOTIC - holds data files with zonal statistics of biotic land cover in fire perimeters.

DATA - Secondary storage of the data used in the analysis. This is the same data located in the biotic and abiotic subfolders in the ANALYSIS folder. 
  -ABIOTIC - holds data files around fire perimeters and abiotic fire conditions.
  -BIOTIC - holds data files with zonal statistics of biotic land cover in fire perimeters.
  
MISC CODE - Some intial exploration with a wildfire fuel modeling tool.

Oregon_2024_Plots - Exploratory plots analyzing wildfires in 2024 in Eastern Oregon.

## **Objective:**
This project examines biotic and abiotic conditions that preceded the 20 largest wildfires in Oregon during the 2024 fire season. By analyzing annual vegetation and climate data from 2021 through the year of ignition, we aim to identify consistent environmental patterns associated with large fire events.

## **Context and Need:**
The 2024 fire season in Oregon was historically large and destructive. Understanding the conditions that preceded these fires is essential for anticipating future fire risk and informing management decisions. While both vegetation composition and climate are known to influence fire behavior, few studies have systematically evaluated these drivers across multiple large fire events in a single season. This project addresses that need by analyzing pre-fire fuel and climate conditions across the 20 largest fires in Oregon during the 2024 wildfire season, primarily within eastern Oregon. 

## **Data Sources and Methods:**
We analyzed annual data from 2021 through 2024 for each fire perimeter using the following sources:

- Rangeland Analysis Platform (RAP): Annual fractional cover estimates for annual forbs and grasses (AFG), perennial forbs and grasses (PFG), and shrubs (SHR).

- PRISM Climate Data: Annual values for precipitation, mean temperature, and vapor pressure deficit (VPD).

- Fire Perimeters: Boundaries for the 20 largest wildfires in Oregon from the 2024 season, used to spatially summarize all vegetation and climate variables.

For each fire, we extracted and averaged all variables across the full fire perimeter to characterize pre-ignition conditions. We then conducted a Random Forest classification analysis to assess the relative importance of each variable.

## **Anticipated Outcomes and Significance:**
This project contributes to a growing body of work seeking to understand the drivers of extreme wildfire events in the western United States. By analyzing biotic and abiotic conditions across the 20 largest Oregon wildfires of 2024, our results offer insight into the environmental precursors that may elevate large fire risk.

Preliminary findings suggest that areas with high annual grass cover and increased atmospheric moisture stress (e.g., elevated VPD) may be more likely to experience large fires under dry and warm conditions. These patterns, if consistent across years, could inform early warning systems and improve spatial targeting for fuels management, such as targeted grazing, prescribed burning, or vegetation treatments.

Our Random Forest analysis identified potential key predictors of large fire occurrence; however, the unexpectedly high classification accuracy highlights the need for model validation using independent data or additional fire years. Despite this limitation, the analysis provides a useful framework for screening relevant variables and identifying patterns worth investigating further.

Ultimately, this work strengthens the foundation for data-informed fire risk assessments and supports more proactive land management strategies tailored to Oregon’s changing fire regimes.

![image](https://github.com/user-attachments/assets/bc507067-18bc-4d29-a693-eb040b91480c)

Image Couresy of: Boise State Public Radio 

