# Seat Belt Usage and Traffic Fatalities: A Statistical Analysis (1983–1997)

This project examines the relationship between seat belt legislation, alcohol regulation, and road traffic fatalities across U.S. states between 1983 and 1997. The analysis was conducted for an undergraduate Statistics course and applies hypothesis testing and linear regression to draw insights with policy implications.

The dataset represents panel data from all 50 U.S. states and the District of Columbia and allows exploration of how seat belt usage rates and legal enforcement types influence traffic death rates.

The goal of this project is to statistically test whether stricter seat belt enforcement laws and the implementation of a 0.08 BAC (blood alcohol content) limit significantly reduce the number of road fatalities per million vehicle miles traveled.


## Methodology

- Exploratory Data Analysis (EDA): Summary statistics, boxplots, bar plots
- Simple Linear Regression (SLR):
  - Seatbelt usage vs. fatalities
  - Enforcement level vs. seatbelt usage
- Multiple Regression Models:
  - Parallel slopes and interaction models
- Hypothesis Testing:
  - Confidence intervals and p-values using bootstrapping
  - t-tests comparing BAC policy groups

  All analysis was performed in R using `ggplot2`, `infer`, and `moderndive`.


## Deliverables

| File | Description |
|------|-------------|
| `seatbelt-enforcement-report.html` | Final project report with full statistical analysis |
| `seatbelt-enforcement-analysis.Rmd` | R Markdown source file |

[Back to portfolio homepage](../README.md)
