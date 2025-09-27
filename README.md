# Restaurant Customer Analytics in R

End-to-end customer analytics in R - EDA, inference, multiple linear regression, PCA, and clustering for marketing segmentation on a restaurant dataset.

## Overview

This repository represents two practical works. It starts with data auditing and feature engineering, then goes through statistical testing, builds and validates regression models, reduces dimensionality with PCA, and finishes with customer segmentation using hierarchical clustering and k-means. The business context is a restaurant (SaborUrbano) aiming to improve acquisition and retention with data-driven actions.

## Key outcomes

* Cleaned and profiled a real-world style customer dataset
* Tested behavioral differences across groups and interpreted effect directions
* Built and validated regression models to explain vegan/vegetarian spend
* Constructed PCA-based indices and interpreted rotated components
* Produced a practical 4-segment customer typology with marketing takeaways

## Methods and workflow

### 1. Exploration and feature engineering

* Structure checks, missingness review, summary statistics
* Engineered features: Age, TotalChildren, MntTotal (overall spend)
* Visual profiling by marital status, children, education, and income using boxplots and histograms

### 2. Statistical inference

* Two-sample t-tests on spend and recency across family vs. no-children groups
* Chi-square test of independence for marital status vs. having children
* Mean comparison of meat/fish vs. vegan/vegetarian spend
* Non-parametric Kruskal–Wallis when normality is violated

### 3. Predictive modeling

* Multiple Linear Regression: MntVeganVegetarian ~ Income + Age + Recency + MntMeatFish
* Extended with categorical drivers: Gender and Response_Cmp4
* Assumptions and diagnostics: residual normality (Shapiro–Wilk) and multicollinearity via performance::check_collinearity

### 4. Dimensionality reduction (PCA)

* Adequacy via KMO on the correlation matrix
* Selection by eigenvalues, Varimax rotation with psych::principal
* Component naming and use of component scores or indices in downstream steps

### 5. Segmentation

* Ward’s hierarchical clustering on PCA-based indices plus Income and Age
* Dendrogram review to select k, then k-means with k = 4
* Cluster centers, sizes, and human-readable profiles with example campaign ideas

### 6. Visualization and reporting

* ggplot2 charts for distributions and group comparisons
* corrplot for correlation structure
* ggdendro for dendrogram visualizations

## Data

A sample of restaurant customers with demographics, spend by product categories, channel behavior, and campaign responses. Example fields: Age, Income, children counts, Recency, category spend (Meat/Fish, Entries, Vegan/Vegetarian, Drinks, Desserts, Additional Requests), purchase channels, and campaign acceptances.

## Skills demonstrated

* Statistical thinking - data quality assessment, assumption checks, effect interpretation
* R programming - wrangling, plotting, modeling, reproducible workflow
* Inference - t-tests, chi-square, Kruskal–Wallis
* Predictive modeling - multiple linear regression, diagnostics, categorical coding
* Dimensionality reduction - PCA with Varimax, component scoring
* Unsupervised learning - hierarchical clustering and k-means, cluster profiling
* Communication - translating findings into marketing actions
