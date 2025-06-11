# Ultra-Processed Food Consumption and Nutritional Status Among Kenyan Children Aged 6–23 Months

This repository contains R code for analyzing data from the 2022 Kenya Demographic and Health Survey (DHS) to assess ultra-processed food consumption and its relationship with nutritional outcomes among children aged 6–23 months.

## 📁 Structure
- `01_recode.R`: R scripts for cleaning and recoding DHS data
- `02_analysis_tables.R`: R scripts for statistical analysis and table generation

- R 4.4+

## 📦 Required R Packages

```r
library(haven)
library(here)
library(dplyr)
library(labelled)
library(naniar)
library(survey)
library(regclass)
library(car)
library(purrr)
library(tableone)
library(epiDisplay)
library(epitools)
library(ggplot2)

## Author

Nebyu D. Amaha, MPH

**Note**: Access to DHS data requires permission from [https://dhsprogram.com](https://dhsprogram.com).
