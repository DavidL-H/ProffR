# ProffR
Scraping data on companies registered in Denmark, from Proff.dk with R

## ProffR.R
This script contains the functions needed to scrape basic data about companies from [Proff.dk](https://www.proff.dk/)<br/>
This may be turned into a package in the future. You can see the function in use in the file: `CompanyDataGather.R`

## CompanyDataGather.R
Here we use the functions in `Proff.R` to gather data on companies in various specific industries of interest.<br/>
We also clean the data up and add some lattitude and longitude coordinates for each company using the `tmaptools` R-package.<br/>
The outputs from this script can be seen in the `.csv` files included in the repository:
- `CompanyURLandIndustries.csv` - An intermidiate file showing just the links to various company pages on proff.dk and their industries
- `CompanyMetricsRaw` - Output with the raw data. There are some duplicates and other things that we want to clean up
- `CompanyMetricsClean` - Cleaned up version of the output
