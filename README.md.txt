# Residential Units Value Analysis in Lesser Poland

## Project Overview

This project aims to analyze the factors influencing the residential units estate turnover in Lesser Poland counties between 2010 and 2022. The dataset includes quarterly data on various economic and demographic indicators. The objective is to identify key drivers of real estate turnover and assess the effects over time and across regions.

## Data

The data was sourced from **Główny Urząd Statystyczny** (Polish Central Statistical Office) via the **Local Data Bank**. The dataset includes the following variables:

- **Dependent variable**: 
  - `value_of_apartments_sold`: Total value of apartments sold.

- **Independent variables**:
  - `nr_residential_units_sold`: Number of units sold.
  - `average_gross_monthly_wages`: Average monthly wages.
  - `population_density`: Population density.
  - `average_price_per_1m2_apartments_sold`: Average price per square meter.
  - `nr_apartments_put_into_use`: Number of apartments put into use.
  - `area_of_apartments_sold`: Total area of sold apartments.


## Models Tested

1. **OLS Regression**: Initial regression to identify potential multicollinearity and heteroskedasticity issues.
2. **Fixed Effects Model (FE)**: To control for unobserved heterogeneity across counties.
3. **Random Effects Model (RE)**: Tested but rejected based on the Hausman test.
4. **Two-Way Fixed Effects Model**: Incorporates both time and county-specific effects.
5. **Panel Regression with AR(1) and PCSE**: Adjusts for autocorrelation and heteroskedasticity.


## Conclusion

The two-way fixed effects model with corrections for autocorrelation and heteroskedasticity was the most suitable model. The results show that demographic and economic factors strongly influence real estate turnover, with notable temporal variability.
