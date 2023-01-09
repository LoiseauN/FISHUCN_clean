## FISHUCN

Estimation of IUCN classification of species using a consensus of machine and deep learning techniques

## Goal

### Machine learning
Evaluate performance of missForest to fill out mission values in your data, and run the missForest
Evaluate performance of the machine learning model
Estimate missing IUCN status of species based on consensus of 200 machine learning algorithms

### Deep learning

## Content

This repository is structured as follow:

- :file_folder: &nbsp;[**data/**](https://github.com/RaphSeguin/FISHUCN_clean/tree/master/data):
contains all data required to reproduce analyses and figures

- :file_folder: &nbsp;[**R/**](https://github.com/RaphSeguin/FISHUCN_clean/tree/master/R):
contains R functions developed especially for this project

- :file_folder: &nbsp;[**man/**](https://github.com/RaphSeguin/FISHUCN_clean/tree/master/man):
contains documentation of R functions

- :file_folder: &nbsp;[**analysis/**](https://github.com/RaphSeguin/FISHUCN_clean/tree/master/analysis):
contains R scripts to reproduce all the analyses/figures

- :file_folder: &nbsp;[**outputs/**](https://github.com/RaphSeguin/FISHUCN_clean/tree/master/outputs):
contains all the results stored in the `.Rdata` format

- :file_folder: &nbsp;[**figures/**](https://github.com/RaphSeguin/FISHUCN_clean/tree/master/figures):
contains all the figures stored in pdf format

- :page_facing_up: &nbsp;[**make.R**](https://github.com/RaphSeguin/FISHUCN_clean/tree/master/make.R):
master R script to run the entire project by calling each R script stored in the **analysis/** folder

## Notes

- All required packages will be installed (if necessary)
- All required packages and R functions will be loaded
- Figures will be stored in `figures/`
- Prior to run the deep learning part, download the needed libraries using the **requirement.txt** file

## Usage

Clone the repository and run this command in R/RStudio:

```r
source("make.R")
```
 

"adding a line" 
