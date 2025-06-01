# College Enrollment Decision Prediction

This project was completed as part of the Machine Learning course at Trinity University and focuses on predicting whether admitted students will choose to enroll. The goal was to help the Office of Admissions better understand the factors influencing student decision-making and improve yield forecasting.

Using admissions data from Fall 2017 to Fall 2021, the project applies a classification modeling approach to predict enrollment decisions and surface meaningful patterns in the data. All modeling and analysis were conducted using R.

The analysis draws from a dataset of over 15,000 admitted applicants and includes a wide range of demographic, academic, and behavioral variables. After extensive data cleaning and feature engineering, several machine learning models were implemented and evaluated, including logistic regression, decision trees, support vector machines, and ensemble methods.

## Research Question

What factors best predict whether an admitted student will choose to enroll at Trinity University?




## Key Insights

Variables with the strongest predictive influence included:

- **Total Event Participation**: Higher engagement in university-hosted events correlated with increased likelihood of enrollment.
- **Decision Plan**: Early action or early decision applicants were more likely to attend.
- **Count of Campus Visits**: Students who visited campus were more inclined to enroll.
- **Athlete Status**: Athletic involvement was a strong indicator of commitment.
- **Second Academic Interest**: Applicants who listed a secondary interest demonstrated more engagement and intent.

Interestingly, some commonly assumed predictors — such as race, religion, GPA, and ACT scores — did not have significant influence in this dataset.

## Deliverables

| File | Description |
|------|-------------|
| `enrollment-data-cleaning-and-modeling.R` | R script for data cleaning, transformation, and model implementation |
| `enrollment-project-report.pdf` | Full report describing methodology, models, and findings |


[Back to portfolio homepage](../README.md)
