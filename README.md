# Infectiousness of SARS-CoV-2 breakthrough infections in vaccinated individuals and reinfections during the Omicron wave

This repository contains analytic code for estimating the infectiousness of Omicron SARS-CoV-2 breakthrough infections and reinfections within California state prisons. The preprint is available [here](https://www.medrxiv.org/content/10.1101/2022.08.08.22278547v4). We analyze data across 1226 index cases and their close contacts from 35 California state prisons from December 15, 2021 to May 20, 2022. We estimate the attack rate of Omicron infection in close contacts of index cases in high-density congregate living environments and estimate the relationship between prior history of vaccination or infection in an index case and subsequent infection in a close contact, adjusting for potential confounding factors.

Data used for this analysis is not publicly available. Data requests may be made to the California Correctional Health Care Services and are subject to controlled access due to requirements to enhance protection of this vulnerable incarcerated population. Requests for data access for study replication or new analyses can be made [here](http://cdcrdata.miraheze.org/wiki/Request_data). 


## Structure
- `1-cleaning`: contains all code needed for initial cleaning of data 
- `2-analysis`: 
  - `1-study population`: contains code for constructing contact networks and testing inclusions/exclusion criteria
  - `2-primary analysis`: contains code for matching of index cases, main regression analysis, sensitivity analyses for regression, and attributable fraction analysis
  - `3-alternative matching`: contains code for testing alternative matching specifications
  - `4-alternative infectious periods`: contains code for testing alternative infectious period definitions
  - `5-alternative regression model`: contains code for testing logistic regression model
- `figures`: contains figure outputs (jpg and eps)
- `tables`: contains table outputs (csv) 
- `archive`: contains old scripts used for testing and data exploration


