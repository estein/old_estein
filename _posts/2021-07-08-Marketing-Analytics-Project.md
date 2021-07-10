---
date: 2021-07-08
layout: post
title: Marketing Analytics Project - Exploratory Data Analysis
subtitle: This is just a practice project to demonstrate my R and data science skills.
image: https://www.publicdomainpictures.net/pictures/280000/velka/digital-marketing.jpg
optimized_image: https://www.publicdomainpictures.net/pictures/280000/velka/digital-marketing.jpg
category: Demonstration
tags:
  - R
  - Data Analytics
  - Marketing
author: evanstein
---

## Introduction

This is a demonstration of using R and statistics to solve the marketing
problem outlined below.

I am using Windows 10, R version 4.1.0, and RStudio version 1.4.1106.

The data for this project can be found here [Marketing Analytics:
Practice Exploratory and Statistical Analysis with Marketing
Data](https://www.kaggle.com/jackdaoud/marketing-data).

I will use the following packages:

``` r
# Data wrangling, data manipulation, & data visualization
library(tidyverse)

# Statistical package for ANOVAs and Linear Mixed Models
library(afex)

# Machine learning package
library(caret)

# Formatting package
library(knitr)
```

### Task Details

You’re a marketing analyst and you’ve been told by the Chief Marketing
Officer that recent marketing campaigns have not been as effective as
they were expected to be. You need to analyze the data set to understand
this problem and propose data-driven solutions.

There are five sections to this task: 1. Understanding the Data 2.
Exploratory Data Analysis 3. Statistical Analysis 4. Data Visualization
5. CMO Recommendations

### Section 01: Understanding the Data

#### Reading and viewing the data

``` r
# Load the data
mkt_data_orig <- read_csv("marketing_data.csv")

# View the data we are using
str(mkt_data_orig, give.attr = FALSE)
```

    ## spec_tbl_df [2,240 x 28] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ ID                 : num [1:2240] 1826 1 10476 1386 5371 ...
    ##  $ Year_Birth         : num [1:2240] 1970 1961 1958 1967 1989 ...
    ##  $ Education          : chr [1:2240] "Graduation" "Graduation" "Graduation" "Graduation" ...
    ##  $ Marital_Status     : chr [1:2240] "Divorced" "Single" "Married" "Together" ...
    ##  $ Income             : chr [1:2240] "$84,835.00" "$57,091.00" "$67,267.00" "$32,474.00" ...
    ##  $ Kidhome            : num [1:2240] 0 0 0 1 1 0 0 0 0 0 ...
    ##  $ Teenhome           : num [1:2240] 0 0 1 1 0 0 0 1 1 1 ...
    ##  $ Dt_Customer        : chr [1:2240] "6/16/14" "6/15/14" "5/13/14" "5/11/14" ...
    ##  $ Recency            : num [1:2240] 0 0 0 0 0 0 0 0 0 0 ...
    ##  $ MntWines           : num [1:2240] 189 464 134 10 6 336 769 78 384 384 ...
    ##  $ MntFruits          : num [1:2240] 104 5 11 0 16 130 80 0 0 0 ...
    ##  $ MntMeatProducts    : num [1:2240] 379 64 59 1 24 411 252 11 102 102 ...
    ##  $ MntFishProducts    : num [1:2240] 111 7 15 0 11 240 15 0 21 21 ...
    ##  $ MntSweetProducts   : num [1:2240] 189 0 2 0 0 32 34 0 32 32 ...
    ##  $ MntGoldProds       : num [1:2240] 218 37 30 0 34 43 65 7 5 5 ...
    ##  $ NumDealsPurchases  : num [1:2240] 1 1 1 1 2 1 1 1 3 3 ...
    ##  $ NumWebPurchases    : num [1:2240] 4 7 3 1 3 4 10 2 6 6 ...
    ##  $ NumCatalogPurchases: num [1:2240] 4 3 2 0 1 7 10 1 2 2 ...
    ##  $ NumStorePurchases  : num [1:2240] 6 7 5 2 2 5 7 3 9 9 ...
    ##  $ NumWebVisitsMonth  : num [1:2240] 1 5 2 7 7 2 6 5 4 4 ...
    ##  $ AcceptedCmp3       : num [1:2240] 0 0 0 0 1 0 1 0 0 0 ...
    ##  $ AcceptedCmp4       : num [1:2240] 0 0 0 0 0 0 0 0 0 0 ...
    ##  $ AcceptedCmp5       : num [1:2240] 0 0 0 0 0 0 0 0 0 0 ...
    ##  $ AcceptedCmp1       : num [1:2240] 0 0 0 0 0 0 0 0 0 0 ...
    ##  $ AcceptedCmp2       : num [1:2240] 0 1 0 0 0 0 0 0 0 0 ...
    ##  $ Response           : num [1:2240] 1 1 0 0 1 1 1 0 0 0 ...
    ##  $ Complain           : num [1:2240] 0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Country            : chr [1:2240] "SP" "CA" "US" "AUS" ...

``` r
# Get the dimensions
mkt_dim <- dim(mkt_data_orig)
```

The data is from 2240 customers with 28 variables per customer.

#### Data Transformation Steps

1.  The Income variable is a character data type and needs to be
    numeric.
2.  The Year\_Birth variable can be converted to an age variable for
    better interpretation.
3.  The Dt\_Customer (Date) variable is a character data type that can
    be turned into a date.

``` r
# Transform the data - Create a new data frame
mkt_data_ana <- mkt_data_orig %>%
  mutate(Income = str_remove_all(Income, pattern = "[$,]"),
         Income = as.numeric(Income), # Change Income to numeric
         Age    = 2021 - Year_Birth, # Create the Age variable from Year_Birth
         Dt_Customer = as.Date(Dt_Customer, "%m/%d/%y")) # Convert to a Date format

# Original data type
class(mkt_data_orig$Income)
```

    ## [1] "character"

``` r
class(mkt_data_orig$Dt_Customer)
```

    ## [1] "character"

``` r
# Check the transformed data
class(mkt_data_ana$Income)
```

    ## [1] "numeric"

``` r
class(mkt_data_ana$Dt_Customer)
```

    ## [1] "Date"

### Section 02: Exploratory Data Analysis

#### Are there any outliers? How will you wrangle/handle them?

To find out what the outliers are, I need to reduce the data set to just
the numeric data. The numeric data are: - Income - Age - Kidhome -
Teenhome - Recency - Mnt\* - Amount spent - Num\*Purchases - Number of
Purchases - NumWebVisitsMonth

``` r
mkt_plot <- mkt_data_ana %>%
  select(ID, Income, Age, ends_with("home"), starts_with("Mnt"), starts_with("Num"))

mkt_plot %>%
  pivot_longer(Income:NumWebVisitsMonth, names_to = "variable", values_to = "response") %>% # Wide -> Long
  ggplot(aes(x = " ", y = response)) + # Set up the aesthetic and plot
  geom_boxplot(width = 0.1) + # Create box plots
  theme_classic() +
  facet_wrap(.~variable, scale = "free", ncol = 3) # Show box plots in a grid
```

![](MarketingEDA_ES_7.8.21_files/figure-gfm/outliers-1.png)<!-- -->
\#\#\#\# Are there any missing data? How will you wrangle/handle them?

``` r
missing_data <- mkt_data_ana %>%
  map(~sum(is.na(.))) %>% # Get the number of NAs for each column
  unlist(.) %>% 
  as.data.frame(.) %>%
  arrange(desc(.)) %>% # Arrange in descending order
  rename(`N Missing` = ".")

knitr::kable(missing_data)
```

|                     | N Missing |
|:--------------------|----------:|
| Income              |        24 |
| ID                  |         0 |
| Year\_Birth         |         0 |
| Education           |         0 |
| Marital\_Status     |         0 |
| Kidhome             |         0 |
| Teenhome            |         0 |
| Dt\_Customer        |         0 |
| Recency             |         0 |
| MntWines            |         0 |
| MntFruits           |         0 |
| MntMeatProducts     |         0 |
| MntFishProducts     |         0 |
| MntSweetProducts    |         0 |
| MntGoldProds        |         0 |
| NumDealsPurchases   |         0 |
| NumWebPurchases     |         0 |
| NumCatalogPurchases |         0 |
| NumStorePurchases   |         0 |
| NumWebVisitsMonth   |         0 |
| AcceptedCmp3        |         0 |
| AcceptedCmp4        |         0 |
| AcceptedCmp5        |         0 |
| AcceptedCmp1        |         0 |
| AcceptedCmp2        |         0 |
| Response            |         0 |
| Complain            |         0 |
| Country             |         0 |
| Age                 |         0 |

We are only missing income data for 24 people. This is only 1.0714286%
of the data. We can do the following: 1. Remove the 24 customers who are
missing Income data 2. Use the median Income to impute the data for the
customers missing data

-   Are there any variables that warrant transformations?
-   Are there any useful variables that you can engineer with the given
    data?
-   Do you notice any patterns or anomalies in the data? Can you plot
    them?

## USE THIS <http://www.sthda.com/english/articles/38-regression-model-validation/157-cross-validation-essentials-in-r/>

## Do a model w/ all and select significant and then do a stepwise model

### Section 03: Statistical Analysis

Please run statistical tests in the form of regressions to answer these
questions & propose data-driven action recommendations to your CMO. Make
sure to interpret your results with non-statistical jargon so your CMO
can understand your findings.

-   What factors are significantly related to the number of store
    purchases?
-   Does US fare significantly better than the Rest of the World in
    terms of total purchases?
-   Your supervisor insists that people who buy gold are more
    conservative. Therefore, people who spent an above average amount on
    gold in the last 2 years would have more in store purchases. Justify
    or refute this statement using an appropriate statistical test.
-   Fish has Omega 3 fatty acids which are good for the brain.
    Accordingly, do “Married PhD candidates” have a significant relation
    with amount spent on fish? What other factors are significantly
    related to amount spent on fish? (Hint: use your knowledge of
    interaction variables/effects).
-   Is there a significant relationship between geographical regional
    and success of a campaign?

### Section 04: Data Visualization

Please plot and visualize the answers to the below questions.

-   Which marketing campaign is most successful?
-   What does the average customer look like for this company?
-   Which products are performing best?
-   Which channels are under performing?

### Section 05: CMO Recommendations

Bring together everything from Sections 01 to 03 and provide data-driven
recommendations/suggestions to your CMO.

### Evaluation

This is not a formal competition, so results won’t be measured using a
strict metric. Rather, what one would like to see is a well-defined
process of exploratory and statistical analysis with insightful
conclusions.

-   Data Exploration - Was the data wrangled properly? How well was the
    data analyzed? Are there any useful visualizations? Does the reader
    learn any new techniques through this submission? A great entry will
    be informative and thought provoking.
-   Statistical Analysis - Were the right statistical tests used? How
    well was the statistical output interpreted? A great entry will
    interpret results without the use of any statistical jargon.
-   Business Recommendation - Were the recommendations tied to your
    analysis in Sections 1-3? Are they data-driven and focused on
    marketing concepts such as targets, channels, or products?
-   Documentation - Are your code, and notebook well documented so a
    reader can understand what you did? Are your sources clearly cited?
    A high quality analysis should be concise and clear at each step so
    the rationale is easy to follow and the process is reproducible.
