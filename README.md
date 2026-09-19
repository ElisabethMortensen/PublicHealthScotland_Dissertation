# Public Health Scotland, A&E Attendance Risk Consultancy Project

This project uses publicly available A&E monthly demographic attendance data for Scotland from Public Health Scotland to investigate possible populations at most risk for attending A&E.

## Data

Three data files are required and should be added to the working directory where you will be running the files, `monthly_attendance_demographics.csv` (monthly A&E attendance count data by demographics), `datazone_pop_estimates.csv`(population estimates for Scotland Data Zones), and `SIMD2020_popweighted.csv` (population weighted Data Zone to Health Board and Deprivation lookup).

Note that, the output of the data prep file, `ae_attendance_rates_postcovid.csv`, is the final merged and pre-processed data set used by all subsequent scripts. This csv has also been provided. More information can be found below on use of this file.

## R Script Descriptions

Scripts must be run in the following order:

1\. `data_prep.R`

2\. `covid_effect.R`

3\. `param_eda.R`

4\. `glm_models.R`

5\. `factor_removal_test.R`

6\. `modeled_rate_interpretation.R`

### `data_prep.R`

Loads monthly attendance data, data zone population estimates, and SIMD lookup. Aggregates population to Year, HBT, Age, Sex, and Deprivation and then merges attendance counts with corresponding populations. Computes attendance rate (attendances per 1,000 population) and log offset required for model. Combines department type and only considers months post COVID-19 Pandemic. Outputs `ae_attendance_rates_postcovid.csv`, the full pre-processed data set used by all subsequent scripts.

### `covid_effect.R`

Exploratory analysis of attendance data to check for potential COVID-19 effect.

### `param_eda.R`

Exploratory analysis of all candidate model predictors and their interactions against attendance rate. Used to justify predictor inclusion before modelling.

### `glm_models.R`

Fits and compares three GLM model specifications.

### `factor_removal_test.R`

Fits and compares reduced models to investigate if removing factors and their interactions has an effect on strength of model fit of attendance rate.

### `modeled_rate_interpretation.R`

Uses final fit model to get modeled attendance rates, mean predicted rates, top/bottom 10 sub populations at risk, and interaction plots for modeled rates.
