
<!-- README.md is generated from README.Rmd. Please edit that file -->
make\_state\_puf
================

<!-- badges: start -->
<!-- badges: end -->
The goal of make\_state\_puf is to explore how to make a state personal income tax public use file (PUF) for an arbitrary state (currently New York) from the [national public use file](https://www.irs.gov/statistics/soi-tax-stats-individual-public-use-microdata-files) available from the IRS Statistics of Income branch. For descriptive information about the IRS PUF see the NBER's web page with [PUF documentation](https://users.nber.org/~taxsim/gdb/), and a [working paper](https://www.taxpolicycenter.org/publications/new-resources-microdata-based-tax-analysis/full) on PUF redesign on the Tax Policy Center website.

Anticipated steps for creating a plain-vanilla state PUF
========================================================

1.  Build a base-year file of state resident taxpayers from the national base-year file, targeting statistics for the state that can be found in [IRS Historical Table 2](https://www.irs.gov/statistics/soi-tax-stats-historic-table-2). I am currently using 2011 as the base year.
    -   Create initial record weights for the file by adjusting the original PUF weights proportionately, by adjusted gross income (AGI) and marital status subgroups, to hit the Table 2 statistics on numbers of returns by AGI range and marital status.
    -   Adjust these initial weights to hit or come close to aggregate statistics in Table 2 for the base year, while minimizing a measure of distortion caused by changing the weights, using a non-linear programming (NLP) approach.
2.  Extrapolate the state base-year file to the most recent year for which IRS Historical Table 2 statistics are available, currently 2016.
    -   Apply growth factors to individual variables in the file to account for known or estimated changes over time.
    -   Adjust weights to hit or come close to aggregate statistics in Table 2 for the extrapolation year, while minimizing a measure of distortion caused by changing the weights, again using an NLP approach.
3.  If summary statistics are available for the state in question, as [they are for New York](https://www.tax.ny.gov/research/stats/statistics/pit-filers-summary-datasets-through-tax-year-2016.htm), further adjust and reweight the file to be consistent with the state's own data. This step may have to be modified, depending upon the data for the state and how it differs from data in SOI Historical Table 2.

4.  Create records for non-resident taxpayers. (The steps above have been focused on creating records that represent a state's resident taxpaying population.)

5.  Forecast the file to future years. To be relevant for state policymakers, forecasts through at least 2020 will be needed, and extrapolations beyond that could be useful.

Optional steps, likely for later in the project
===============================================

1.  Do all of the above, starting with a synthetic national PUF. One advantage of this approach is that we will have more records to start with, which will give greater flexibility in reweighting the file.

2.  If some project participants are allowed access to a confidential New York file from the state tax department, then:
    -   Write a translator to put the file into the input format required by Tax-Calculator and the open-source state tax model, imputing variables if needed.
    -   Do step 5 above. Note that this could allow us to create better targets for files created from the SOI PUF.
3.  The goals of the current phase of the project are focused on income tax filers. In later phases, additional steps could include:
    -   Create records that represent the state-resident non-tax-filers.
    -   Add variables with non-income-tax information, such as expenditures, home ownership, home values, and property tax, by statistical matching and imputation.
