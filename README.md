# Churn Modelling with Exploratory Data Analysis (EDA)

## Project Overview

This project focuses on conducting an Exploratory Data Analysis (EDA) on a bank customer dataset to understand and analyze customer churn. The aim is to identify key characteristics, trends, and potential factors influencing customer churn, which will guide feature selection, preprocessing steps, and model development for predicting customer churn effectively.

## Problem Statement

The primary objective is to perform a thorough EDA on the bank customer dataset to reveal patterns in customer behavior, demographics, and interactions with the bank's services. The analysis aims to highlight any anomalies, missing values, or outliers that may impact the quality of the dataset. The insights derived from EDA will be used to develop actionable strategies to help companies retain their customers effectively.

## Dataset Description

The dataset consists of information about churning customers from the last year and includes the following columns:

- Customer_ID: Unique ID of the customer
- RowNumber: Row number of entry
- Gender: Gender of the customer (Male/Female)
- Geography: Place where the customer belongs
- HasCrcard: Indicates if the customer has a credit card (Yes/No)
- IsActiveMember: Indicates if the customer is active (Yes/No)
- EstimateSalary: Estimated salary of the customer
- Balance: Customer's balance information
- Tenure: Months in data
- Age: Age of the customer
- NumOfProducts: Number of products
- Exited: Indicates if the customer has churned (Yes/No)
- CreditScore: Customer's credit score
- Surname: Surname of the customer

## Steps for Exploratory Data Analysis (EDA)

### Step 1: Installing and Loading Required Libraries

```R
# Required libraries
library(ggplot2)
library(dplyr)
library(plotly)
library(plotrix)
library(cowplot)
library(Hmisc)
library(tidyr)
library(scales)
library(MASS)
library(psych)
library(naniar)
library(grid)
library(corrplot)
```

### Step 2: Loading the Dataset

```R
# Load the CSV file into RStudio
churnData <- read.csv(file.choose(), header = TRUE)
```

### Step 3: Overview of the Data

```R
# View the first and last elements of the data
head(churnData)
tail(churnData)
```

### Step 4: Data Dimensions, Structure, and Statistical Information

```R
# Dimensions of the data
dim(churnData)

# Structure of the data
str(churnData)

# Statistical information
describe(churnData)
summary(churnData)
```

### Step 5: Checking for Null Values

```R
# Check for null values
colSums(is.na(churnData))
```

### Step 6: Adding Null Values to the Data

```R
# Add null values to the data
set.seed(123)
churn2Data <- churnData
churn2Data[sample(seq(NROW(churn2Data)), round(3 / 100 * nrow(churn2Data))), "Age"] <- NA
churn2Data[sample(seq(NROW(churn2Data)), round(5 / 100 * nrow(churn2Data))), "Gender"] <- NA
```

### Step 7: Plotting Graph for Missing Values

```R
# Plot graph for missing values
gg_miss_var(churn2Data) + ggtitle("Missing Values in the Data")
```

### Step 8: Data Cleaning

```R
# Fill null values
churn2Data <- churn2Data %>%
  mutate(Age = ifelse(is.na(Age), median(churn2Data$Age, na.rm = TRUE), Age))

churn2Data <- churn2Data %>%
  mutate(Gender = ifelse(is.na(Gender), Mode(Gender), Gender))

# Check for null values
colSums(is.na(churn2Data))
```

### Step 9: Data Transformation

```R
# Transform data for univariate analysis
churn2Data <- churn2Data %>%
  mutate(Exited = ifelse(Exited == 1, "Yes", "No"),
         IsActiveMember = ifelse(IsActiveMember == 1, "Yes", "No"),
         HasCrCard = ifelse(HasCrCard == 1, "Yes", "No"))

# Create bins for 'Tenure'
labels <- sprintf("%d - %d", seq(0, 11, 2), seq(1, 11, 2))
churn2Data$Tenure_group <- cut(churn2Data$Tenure, breaks = c(seq(0, 11, 2), Inf), right = FALSE, labels = labels)

# Drop unnecessary columns
churn2Data <- subset(churn2Data, select = -c(RowNumber, Customer_ID, Surname))
```

### Step 10: Data Exploration

#### Univariate Analysis

```R
# Countplot for 'Exited'
plot_ly(df_value_counts, x = ~Variable, y = ~Count) %>%
  layout(title = 'No.of customers churned', xaxis = list(title = 'Target variable'), yaxis = list(title = 'count')) %>%
  add_trace(marker = list(color = custom_colors))

# Countplots for other variables
# Gender, HasCrCard, IsActiveMember, NumOfProducts, Age, Tenure_group
# Density plots for CreditScore, EstimatedSalary, Balance, Age
```

### Step 11: Bivariate Analysis

```R
# Divide data into 'Exited' and 'Not Exited' groups
exited <- churn2Data[churn2Data$Exited == 'Yes', ]
not_exited <- churn2Data[churn2Data$Exited == 'No', ]

# Plot graphs for bivariate analysis
# Age and Gender, Card and Gender, Geography and Gender
```

### Step 12: Numerical Analysis

```R
# Heatmap for correlation matrix
numeric_data <- select_if(churn2Data, is.numeric)
correlation_matrix <- cor(numeric_data)
corrplot(correlation_matrix, method = "color", col = c("darkblue", "green", "darkred", 'purple'), scale = "none")
```

## Conclusion

- Females are more likely to churn compared to males.
- Customers with a tenure of six and seven months have a higher churning rate, possibly due to seasonal variations.
- Germany has a higher churning rate compared to France and Spain.
- The number of products is directly proportional to the churning rate; customers with three and four products have a higher churning rate.
- Customers aged between 40-50 are more likely to churn.
- Customers with a credit score between 600-700 are more likely to churn.
- Having a credit card does not significantly impact churn rate.
  
## Future Steps

- Develop predictive models to forecast customer churn.
- Implement targeted marketing strategies to retain at-risk customers.
- Continuously monitor and analyze customer behavior to adapt strategies accordingly.

---

For more details and the full R code, please refer to the project repository.
