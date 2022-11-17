# Infectiousness of SARS-CoV-2 breakthrough infections in vaccinated individuals and reinfections during the Omicron wave

This repository contains analytic code for estimating the infectiousness of Omicron SARS-CoV-2 breakthrough infections and reinfections within California state prisons. We analyze data across 1226 index cases and their close contacts from 35 California state prisons from December 15, 2021 to May 20, 2022. We estimate the attack rate of Omicron infection in close contacts of index cases in high-density congregate living environments and estimate the relationship between prior history of vaccination or infection in an index case and subsequent infection in a close contact, adjusting for potential confounding factors.

Data used for this analysis is not publicly available. Data requests may be made to the California Correctional Health Care Services and are subject to controlled access due to requirements to enhance protection of this vulnerable incarcerated population. Requests for data access for study replication or new analyses can be made [here](http://cdcrdata.miraheze.org/wiki/Request_data). 

The preprint is available [here](https://www.medrxiv.org/content/10.1101/2022.08.08.22278547v5). Additional methods not included in the main manuscript are available in [Methods.md](https://github.com/sophttan/CDCR-CalProtect/blob/main/Methods.md). The pre-analysis plan developed and posted prior to any analysis can be found in [Analysis Plan Calprotect vaccine study Tan Lo- FINAL.pdf](https://github.com/sophttan/CDCR-CalProtect/blob/main/Analysis%20Plan%20Calprotect%20vaccine%20study%20Tan%20Lo-%20FINAL.pdf). 

## Structure
- `1-cleaning`: contains all code needed for initial cleaning of data 
- `2-analysis`: 
  - `1-study population`: contains code for constructing contact networks and testing inclusions/exclusion criteria
  - `2-primary analysis`: contains code for matching of index cases, main regression analysis, sensitivity analyses for regression, and attributable fraction analysis
  - `3-alternative matching`: contains code for testing alternative matching specifications
  - `4-alternative infectious periods`: contains code for testing alternative infectious period definitions
  - `5-alternative regression model`: contains code for testing logistic regression model
  - `6-alternative exclusion criteria`: contains code for testing robustness of results when relaxing exclusion criteria
  - `7-alternative jj vaccine`: contains code for testing impact of removing less effective J&J vaccine from index case population
  - `8-alternative num doses`: contains code for testing alternative matching procedure for estimating relationship between number of vaccine doses and infectiousness of index cases
- `figures`: contains figure outputs (jpg and eps)
- `tables`: contains table outputs (csv) 
- `archive`: contains old scripts used for testing and data exploration


