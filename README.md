# Infectiousness of SARS-CoV-2 breakthrough infections in vaccinated individuals and reinfections during the Omicron wave

This repository contains analytic code for estimating the infectiousness of Omicron SARS-CoV-2 breakthrough infections and reinfections within the context of California state prisons. We estimate the attack rate of Omicron infection in high-density congregate living environments and estimate the relationship between prior history of vaccination or infection in an index case and subsequent infection in a close contact, adjusting for potential confounding.

Data used for this analysis is not publicly available.

## Structure
- `1-cleaning`: contains all code needed for initial cleaning of data 
- `2-analysis`: 
  - `1-study population`: contains code for constructing contact networks and testing inclusions/exclusion criteria
  - `2-primary analysis`: contains code for matching of index cases, main regression analysis, sensitivity analyses for regression, and attributable fraction analysis
  - `3-alternative matching`: contains code for testing alternative matching specifications
  - `4-alternative infectious periods`: contains code for testing alternative infectious period definitions
  - `5-alternative regression model`: contains code for testing logistic regression model
- `figures`: contains figure outputs (jpg)
- `tables`: contains table outputs (csv) 
- `archive`: contains old scripts used for testing and data exploration
