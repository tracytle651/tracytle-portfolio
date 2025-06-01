# Gasoline Demand Forecasting and Economic Driver Analysis (2000–2023)

This project investigates the key macroeconomic, energy, transportation, and behavioral factors driving weekly gasoline demand in the United States. Using over 24 years of data from publicly available sources, we built and validated predictive models that capture demand variability, explain key influencing variables, and provide interpretable insights for decision-makers in the energy sector.

The project was completed as part of the BAT-3302 Data Science course.

---

## Objective

To model and forecast U.S. consumer gasoline demand and identify the most influential economic and behavioral variables affecting its fluctuation over time. The analysis focuses on understanding:

- What variables are most predictive of gasoline consumption?
- How do key predictors change over time across economic cycles?
- How can organizations anticipate demand shifts using interpretable machine learning?

---

## Data & Methodology

**Target Variable:**  
- U.S. Weekly Implied Gasoline Demand (from the U.S. Energy Information Administration)

**Predictor Categories Included:**
- Energy market benchmarks (crude oil prices, refinery utilization, fuel exports/imports)
- Macroeconomic indicators (GDP, inflation, earnings, employment)
- Transportation activity (EV sales, VMT, rail and air traffic, freight index)
- Financial markets (S&P 500, USD Index, consumer loans)
- Consumer behavior and policy shifts (credit usage, EV regulation, TSA throughput)

**Preprocessing Techniques:**
- Frequency standardization for aligning daily, monthly, quarterly, and annual predictors to a weekly cadence
- Missing data imputation and forecasting using Facebook Prophet
- One-hot encoding for seasonality (month)
- Time-based train-test split to avoid data leakage (train: 2000–2022; test: Sep 2022–Sep 2023)

---

## Modeling Approach

We used multiple machine learning techniques to model gasoline demand:

- **Facebook Prophet** for seasonality modeling and NA imputation
- **Random Forest** and **XGBoost** for feature importance and prediction
- **Ridge** and **Lasso** regressions for assessing linear relationships
- Extensive hyperparameter tuning using `RandomizedSearchCV`

The final model — XGBoost with Prophet-generated seasonality (`yhat`) as an input — achieved:

| Metric             | Training Set | Test Set (Sep 2022–Sep 2023) |
|--------------------|--------------|-------------------------------|
| Mean Absolute Error| ~198.5       | ~328.8                        |
| R-squared          | ~0.73        | ~0.31                         |

---

## Key Findings

- **Gasoline demand is seasonal**, with consistent dips in winter months (Jan–Mar) and peaks in summer.
- **Top predictors vary over time**. Rolling-decade analysis revealed that economic and behavioral drivers shift across decades.
- **In the 2021–2023 period**, the top demand predictors included:
  - Jobless claims (negative correlation)
  - US Dollar Index (negative correlation)
  - TSA passenger throughput (positive)
  - Pipeline petroleum movement (positive)
  - Fuel imports (positive)

This temporal variation highlights the importance of updating demand models regularly.

---

## Files Included

| File | Description |
|------|-------------|
| `gasoline_demand_modeling.ipynb` | Full modeling notebook with preprocessing, forecasting, and feature importance |
| `gasoline_demand_report.pdf` | Final project report summarizing methods and findings |
| `shap_summary.png` | SHAP plot showing top predictors in recent years |
| `gasoline_demand_predictions.csv` | Model output (optional) |

---

[Back to Portfolio Homepage](../README.md)
