# AMAS Data Cleaning & EDA
# The aim of this practice project is to equip myself with the skill in\
#providing clean and quality dataset for future analysis
# In this project, I will be using the AMAS(raw) Dataset

#Main Sections:
# Data Quality Assessment - finding missing values, duplicates, outliers
# Data Cleaning - handling all those issues with multiple strategies
# Exploratory Data Analysis - visualizations and statistical analysis
# Data Preprocessing - feature engineering, encoding, scaling
# Data Quality Report - before/after comparison
# Loading my pack manager



pacman::p_load(
  AmesHousing, #Has the AmesHousing data
  tidyverse, #data manipulation and visualization
  naniar, #Missing data visualiaztion
  skimr, #Quick data summary
  DataExplorer, # Automated EDA reports
  forcats,
  corrplot, # correlation plot
  fastDummies, # One-hot encoding
  e1071, # Identify skewed variables
  tidymodels,
  janitor, # Data cleaning and validation tools
  here
)


#Time to load the data
ames_dirty_data <- AmesHousing::ames_raw
#Its not advisable to overwrite the original
# data so always make a copy to preserve the original
ames_copy <- ames_dirty_data
glimpse(ames_dirty_data) #quick look at the data

#DATA QUALITY ASSESSMENT
#1.understanding your Dataset

#You do this for basic information about your dataset  like
#The structure, the initial dimension, and the names of the variables

dim(ames_dirty_data) #Dimension(calls the number of rows and columns in the dataset)
names(ames_dirty_data) #Views the column(variables) names in the dataset
str(ames_dirty_data) #this is to view the structure of the dataset to see which variable/data types

#Statistical summary of the dataset
summary(ames_dirty_data)

# Ames housing (n = 2930): mixed categorical + numeric features.
# Data quality flags: notable missingness (Lot Frontage has 490 NAs; Garage Yr Blt 159;
# Mas Vnr Area 23; a few basement/garage/bath fields ~1–2 NAs).
# Distribution/outliers: SalePrice is right-skewed (median ~160k, mean ~180.8k); some extreme
# maxima (Lot Area 215,245; Gr Liv Area 5,642).
# Suspicious value to check/clean: Garage Yr Blt max = 2207 (likely invalid/outlier).

#Then I use skimr for quick overview
#COMPREHENSIVE SUMMARY
skim(ames_dirty_data) #Shows data types,missingness,basic statistics,distributions
# Biggest missingness is in “absence-type” features: Alley (2732 missing ~93%), Pool QC (2917 ~99.6%),
# Misc Feature (2824 ~96%), Fence (2358 ~80%), Fireplace Qu (1422 ~48%), plus garage fields (≈159 missing) and Lot Frontage (490 missing).
# Many numeric variables are heavily right-skewed / zero-inflated (e.g., decks/porches/pool area, misc val; lots of zeros with a long right tail).
# Outliers / potential data errors: Lot Area max 215,245; Gr Liv Area max 5,642; and Garage Yr Blt max 2207 (likely invalid year).
# SalePrice looks right-skewed (median 160k < mean 180.8k; max 755k), suggesting potential log-transform for modeling.

#And then I checked for missing Data
colSums(is.na(ames_dirty_data)) #Counts missing values per column
#Pool QC shows thw variable with the most missing data with approx. 2732 misssing data, followed by Misc Feature,
#Fence, Alley...etc  and I have to figure
#out why there's this missingness and the type of missingness

#Now let's chec the percentage of missingness
vis_miss(ames_dirty_data) #There is a %5.8 missing data from the whole data set

#Mising data pattern /Ranks variables by missingness %
#This helps decide strategy:drop?impute?treat NAs as "None"
gg_miss_var(ames_dirty_data, show_pct = T)

#1.4 Checking for Duplicates
#Duplicates can bias models and summaries
sum(duplicated(ames_dirty_data))

#how to view if duplicate really exisits
ames_dirty_data[
  duplicated(ames_dirty_data) | duplicated(ames_dirty_data, fromLast = T),
]

#1.5 Identify Data types Issues
#check if numeric columns contain non-numeric values
#check if categorical variables have unusual levels
#Check unique values in categorical columns
ames_dirty_data |>
  summarise(across(where(is.character), ~ n_distinct(.x, na.rm = TRUE)))


#DATA CLEANING
#Handling Missing values
#Strategy depends on the columns and percentage of missingness
#handling missingness depends on the dataset and the research question you are trying to answwer
#Also analyst must know how the missingness occured

# Impute missing values
ames_clean_step1 <- ames_dirty_data |>
  # Fix Garage Year using House Year
  mutate(
    `Garage Yr Blt` = ifelse(
      is.na(`Garage Yr Blt`),
      `Year Built`,
      `Garage Yr Blt`
    )
  ) |>
  # Apply median to remaining numeric NAs
  mutate(across(
    where(is.numeric),
    ~ ifelse(is.na(.), median(., na.rm = TRUE), .)
  ))

# 2. Categorical Imputation Block (Starting from step 1!)
ames_clean <- ames_clean_step1 |>
  # Change is.factor to is.character to actually catch the text columns
  mutate(across(
    where(is.character),
    ~ fct_na_value_to_level(factor(.), level = "None")
  )) |>
  # Fix Ordinal Ranking
  mutate(
    `Exter Qual` = factor(
      `Exter Qual`,
      levels = c("Ex", "Gd", "TA", "Fa", "Po"),
      ordered = TRUE
    )
  )

# This should be 0 for all character/factor columns now
sum(is.na(select(ames_clean, where(is.factor))))

#Remove duplicates rows
ames_clean_dup <- ames_clean |> get_dupes()
ames_clean_dup <- ames_clean |> distinct()
ames_clean_dup
#2.3 Handling Outliers
#Identify outliers using IQR method
identify_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = T)
  Q3 <- quantile(x, 0.75, na.rm = T)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  x < lower | x > upper
}

# Example: Check Sale_Price for outliers
outliers <- identify_outliers(ames_clean_dup$`SalePrice`)
sum(outliers, na.rm = TRUE)

#visualize outliers
ggplot(ames_clean_dup, aes(y = `SalePrice`)) +
  geom_boxplot() +
  labs(title = "Sale Price - outlier Detection")

# 2. Cap them (winsorization)
winsorize <- function(x, probs = c(0.01, 0.99)) {
  limits <- quantile(x, probs, na.rm = T)
  x[x < limits[1]] <- limits[1]
  x[x > limits[2]] <- limits[2]
  x
}

ames_clean_df <- ames_clean_dup |>
  mutate(`SalePrice` = winsorize(`SalePrice`))

#3 Transform them (log transformation)
ames_model_df <- ames_clean_dup |>
  mutate(
    SalePrice_w = winsorize(SalePrice, probs = c(0.01, 0.99)),
    SalePriceLog = log(SalePrice_w)
  )

#2.4 Standardize text/Categories
#Fix Data Type Issues
#STEP 3: Exploratory Data Analysis(EDA)
#3.1 Univariate Analysis - Numeric variables
# Distribution of Sale Price
ggplot(ames_clean_dup, aes(x = SalePrice)) +
  geom_histogram(bins = 50, fill = "Steelblue", color = "white") +
  labs(title = "Distribution of Sale Price", x = "Sale Price", y = "Count") +
  theme_minimal()

# Summary Statistics for all numeric variables
ames_clean_dup %>%
  select(where(is.numeric)) %>%
  summary()

# Density plots for key numeric variables
ames_clean_dup %>%
  select(SalePrice, `Lot Area`, `Gr Liv Area`, `Year Built`) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "value"
  ) %>%
  ggplot(aes(x = value)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  facet_wrap(~variable, scales = "free") +
  theme_minimal()

#Univariate Analysiss - Categorical Variables
#Frequency tables
table(ames_clean_dup$`MS Zoning`)
prop.table(table(ames_clean_dup$`MS Zoning`))

#bar charts
ggplot(ames_clean_dup, aes(x = `MS Zoning`)) +
  geom_bar(fill = 'coral') +
  labs(
    title = 'Distribution of Zoning Classification',
    x = 'Zoning',
    y = 'Count'
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Multiple Categorical Variables
ames_clean_dup %>%
  select(-PID, -Order) %>% # drop identifiers
  select(where(is.factor)) %>%
  select(
    Alley,
    `MS Zoning`,
    Street,
    `Lot Shape`,
    Neighborhood,
    `Sale Condition`
  ) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_bar() +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))


#3.3 Bivariate Analysis - numeric vs Numeric
#Correlation matrix
numeric_vars <- ames_clean_dup %>% select(where(is.numeric))
cor_matrix <- cor(numeric_vars, use = 'complete.obs')

#visualize correlation
corrplot(
  cor_matrix,
  method = 'color',
  type = 'upper',
  tl.col = 'black',
  tl.srt = 45,
  tl.cex = 0.7
)

#Scatter plots
ggplot(ames_clean_dup, aes(x = `Gr Liv Area`, y = `SalePrice`)) +
  geom_point(alpha = 0.5, color = 'steelblue') +
  geom_smooth(method = 'lm', color = 'red') +
  labs(
    title = "Sale price vs Living Area",
    x = "Above Ground Living Area(sq ft)",
    y = 'Sale Price'
  ) +
  theme_minimal()

#Scatter plot matrix for key variables
pairs(
  ~ `SalePrice` + `Gr Liv Area` + `Year Built` + `Overall Qual`,
  data = ames_clean_dup,
  main = "Scatter Plot Matrix"
)

#3.4 Bivariate Analysis - Categorical vs Numeric
#Box plots
ggplot(
  ames_clean_dup,
  aes(x = `Overall Qual`, y = `SalePrice`, group = `Overall Qual`)
) +
  geom_boxplot(fill = "steelblue", alpha = 0.1) +
  labs(
    title = "Sale Price by Overall Quality",
    x = "Overall Quality",
    y = "Sale Price"
  ) +
  theme_minimal()

# Violin plots
ggplot(
  ames_clean_dup,
  aes(x = `MS Zoning`, y = `SalePrice`, fill = `MS Zoning`)
) +
  geom_violin() +
  labs(
    title = "Sale Price Distribution by Zoning",
    x = "Zoning Classification",
    y = "Sale Price"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# 3.5 Multivariate Analysis
#Colored catter plot
ggplot(
  ames_clean_dup,
  aes(x = `Gr Liv Area`, y = `SalePrice`, color = `Overall Qual`)
) +
  geom_point(alpha = 0.6) +
  scale_color_viridis_c() +
  labs(
    title = "Sale Price vs Living Area by Quality",
    x = "Living Area",
    y = "Sale Price",
    color = "Quality"
  ) +
  theme_minimal()

#Faceted Plots
ggplot(ames_clean_dup, aes(x = `Year Built`, y = `SalePrice`)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", color = "red") +
  facet_wrap(~`MS Zoning`) +
  labs(title = "Sale Price vs Year Built by Zoning") +
  theme_minimal()


#3.6 Automated EDA Report
#Generate comprehensive EDA report
create_report(
  ames_clean_dup,
  output_file = "Ames_eda_report.html",
  output_dir = here::here()
)

#Step4: Data Preprocessing
#4.1 Feature Engineering
#Create new features
ames_processed <- ames_clean_df %>%
  mutate(
    #Age of the house
    House_Age = lubridate::year(Sys.Date()) - `Year Built`,

    #Total bathrooms
    Total_Baths = `Full Bath` +
      0.5 * `Half Bath` +
      `Bsmt Full Bath` +
      0.5 * `Bsmt Half Bath`,

    #Total square footage
    Total_SF = `Total Bsmt SF` + `Gr Liv Area`,

    #Has garage
    Has_Garage = factor(ifelse(`Garage Area` > 0, "Yes", "No")),

    #Price per square foot
    Price_per_SF = ifelse(
      `Gr Liv Area` > 0,
      `SalePrice` / `Gr Liv Area`,
      NA_real_
    ),

    #Is remodeled
    Is_Remodeled = factor(ifelse(`Year Remod/Add` > `Year Built`, "Yes", "No"))
  )


#4.2 Encoding Categorical Variables
ames_encoded <- ames_processed %>%
  dummy_cols(
    select_columns = c("MS Zoning", "Neighborhood"),
    remove_selected_columns = TRUE,
    remove_first_dummy = TRUE
  ) #Avoid multicollinearity


#Label encoding (original variables)
ames_processed <- ames_processed %>%
  mutate(
    Overall_Qual_num = as.numeric(`Overall Qual`),
    Overall_Cond_num = as.numeric(`Overall Cond`)
  )


#4.3 Scaling/Normalization
#Standardization (z-score normalization)
scale_feature <- function(data, vars) {
  data %>%
    mutate(across(all_of(vars), ~ scale(.) %>% as.vector))
}

numeric_cols <- ames_processed %>%
  select(where(is.numeric)) %>%
  select(-any_of(c("Order", "PID", "SalePrice"))) %>%
  names()

#Min-Max normalization
normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

ames_normalized <- ames_processed %>%
  mutate(across(where(is.numeric), normalize))

#handle skewness
skewness_values <- sapply(
  ames_processed %>% select(where(is.numeric)),
  skewness,
  na.rm = TRUE
)
highly_skewed <- names(skewness_values[abs(skewness_values) > 1])
print(highly_skewed)


# Log transformation for right-skewed variables
ames_transformed <- ames_processed %>%
  mutate(across(all_of(highly_skewed), ~ log1p(.), .names = "{.col}_log"))


#Step 5:Data Quality Report
#Create a Final Quality Assessment
# Final quality check
quality_report <- data.frame(
  Metric = c(
    "Total Rows",
    "Total Columns",
    "Missing Values",
    "Duplicate Rows",
    "Numeric Columns",
    "Categorical Columns",
    "Memory Size (MB)"
  ),
  Original_Data = c(
    nrow(ames_raw),
    ncol(ames_raw),
    sum(is.na(ames_raw)),
    sum(duplicated(ames_raw)),
    sum(sapply(ames_raw, is.numeric)),
    sum(sapply(ames_raw, is.factor)),
    format(object.size(ames_raw), units = "MB")
  ),
  Cleaned_Data = c(
    nrow(ames_processed),
    ncol(ames_processed),
    sum(is.na(ames_processed)),
    sum(duplicated(ames_processed)),
    sum(sapply(ames_processed, is.numeric)),
    sum(sapply(ames_processed, is.factor)),
    format(object.size(ames_processed), units = "MB")
  )
)

print(quality_report)

# Windows Desktop path
# Save outputs to the project directory
output_dir <- here::here()

# Save the report
write.csv(quality_report,
          file.path(output_dir, "data_quality_report.csv"),
          row.names = FALSE)

# Save the clean data
saveRDS(ames_processed,
        file.path(output_dir, "ames_cleaned.rds"))

write.csv(ames_processed,
          file.path(output_dir, "ames_cleaned.csv"),
          row.names = FALSE)

# To load later:
# ames_cleaned <- readRDS("ames_cleaned.rds")
