# Ames Housing Data Cleaning & EDA

Comprehensive data cleaning and exploratory data analysis of the Ames Housing dataset using R. The project covers the full preparation pipeline — from raw, incomplete data through to a production-ready, ML-ready output.

## Results Summary

| Metric | Before | After |
|---|---|---|
| Missing Values | 13,997 | 0 |
| Total Columns | 82 | 90 |
| Duplicate Rows | 0 | 0 |
| Numeric Columns | 37 | 43 |
| Categorical Columns | 0 | 47 |

## Pipeline Overview

**1. Data Quality Assessment**
Profiled the dataset using `skimr` and `naniar` to identify 13,997 missing values across 82 columns, zero duplicate rows, and significant right-skew in key numeric variables including `SalePrice` and `Lot Area`.

**2. Data Cleaning**
Missing values in numeric columns were imputed using column medians, with `Garage Yr Blt` handled separately using `Year Built` as a domain-appropriate proxy. Categorical `NA` values were converted to an explicit `"None"` level using `fct_na_value_to_level()`, preserving the structural meaning of absence. Outliers in `SalePrice` were treated via winsorization at the 1st and 99th percentiles.

**3. Exploratory Data Analysis**
Conducted univariate, bivariate, and multivariate analysis across numeric and categorical variables. Key outputs include a full correlation matrix, sale price distributions, and scatter plots examining the relationship between living area, overall quality, and sale price. An automated HTML report was generated via `DataExplorer`.

**4. Feature Engineering**
Eight new features were created to enhance predictive signal:

| Feature | Description |
|---|---|
| `House_Age` | Years since construction |
| `Total_Baths` | Weighted sum of all bathroom types |
| `Total_SF` | Basement + above-grade living area |
| `Has_Garage` | Binary indicator for garage presence |
| `Price_per_SF` | Sale price per square foot (EDA use only) |
| `Is_Remodeled` | Whether the home has been remodeled |
| `Overall_Qual_num` | Numeric encoding of overall quality |
| `Overall_Cond_num` | Numeric encoding of overall condition |

**5. Preprocessing**
Categorical variables encoded via one-hot encoding (`fastDummies`). Numeric features standardized and min-max normalized. Skewed variables identified and log-transformed using `log1p`.

## Key Findings

- Overall Quality is the strongest single correlate of Sale Price (r > 0.7)
- Above-grade living area is the strongest continuous predictor of home value
- Missing data patterns were largely structural — most `NA` values indicate feature absence rather than data entry gaps
- Sale Price is right-skewed with a long upper tail, warranting log transformation for modeling

## Tech Stack

| Category | Libraries |
|---|---|
| Core | R, tidyverse, tidymodels |
| Visualization | ggplot2, corrplot |
| Data Quality | naniar, skimr |
| EDA | DataExplorer |
| Preprocessing | fastDummies, e1071 |
| Reproducibility | here, janitor |

## Project Structure

```
.
├── ames_data_cleaning_eda.R    # Main analysis script
├── ames_cleaned.rds            # Cleaned dataset (R format)
├── ames_cleaned.csv            # Cleaned dataset (CSV format)
└── data_quality_report.csv     # Before/after quality metrics
```

## Visualizations

| Plot | Key Takeaway |
|---|---|
| Sale Price Distribution | Right-skewed with a long upper tail; median ~$160k, outliers above $500k suggest log transformation for modeling |
| Sale Price Outlier Detection | Boxplot confirms upper-end outliers; winsorization applied at 1st and 99th percentiles |
| Density Plots (Gr Liv Area, Lot Area, Sale Price, Year Built) | Most numeric features are right-skewed; Year Built shows a bimodal pattern reflecting two construction booms |
| Correlation Matrix | Overall Quality and Gr Liv Area show the strongest positive correlation with Sale Price |
| Sale Price vs Living Area | Clear positive linear relationship; confirms Gr Liv Area as the strongest individual predictor |
| Sale Price vs Living Area by Quality | Higher quality ratings amplify the price premium on larger homes — a meaningful interaction effect |
| Missing Data Pattern | 5.8% total missingness; concentrated in absence-type features (Pool QC, Alley, Fence) rather than true data gaps |
| Missing Data by Variable | Pool QC (~99.6%), Misc Feature (~96%), and Fence (~80%) account for the majority of missing values |
| Categorical Distributions | Most sales are Normal condition, RL zoning, and paved street — dataset reflects typical residential transactions |

## Getting Started

```r
# Install dependencies
if (!require(pacman)) install.packages("pacman")

pacman::p_load(
  AmesHousing, tidyverse, tidymodels, naniar, skimr,
  DataExplorer, corrplot, fastDummies, e1071, here, janitor
)

# Run the analysis
source("ames_data_cleaning_eda.R")
```

## Skills Demonstrated

- Data quality assessment and profiling
- Missing data analysis and domain-aware imputation
- Outlier detection and winsorization
- Exploratory data analysis (univariate, bivariate, multivariate)
- Feature engineering
- Categorical encoding and numeric scaling
- Reproducible project structure

## Author

Collins Amoo

## License

Open source. Available for educational use.
