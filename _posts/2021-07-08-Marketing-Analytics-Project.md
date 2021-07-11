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

# Introduction

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

# Assumption checks for linear regression
library(gvlma)

# Machine learning package
library(caret)

# Formatting package
library(knitr)

# Knitr Kable
library(kableExtra)
```

## Task Details

You’re a marketing analyst and you’ve been told by the Chief Marketing
Officer that recent marketing campaigns have not been as effective as
they were expected to be. You need to analyze the data set to understand
this problem and propose data-driven solutions.

There are five sections to this task: 1. Understanding the Data 2.
Exploratory Data Analysis 3. Data Visualization 4. Statistical Analysis
5. CMO Recommendations

## Section 01: Understanding the Data

### Reading and viewing the data

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

### Data Transformation Steps

1.  The Income variable is a character data type and needs to be
    numeric.
2.  The Year\_Birth variable can be converted to an age variable for
    better interpretation.
3.  The Dt\_Customer (Date) variable is a character data type that can
    be turned into a date.

``` r
# Transform the data - Create a new data frame
mkt_data <- mkt_data_orig %>%
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
class(mkt_data$Income)
```

    ## [1] "numeric"

``` r
class(mkt_data$Dt_Customer)
```

    ## [1] "Date"

### Are there any variables that warrant transformations?

Similar to transforming the age variable, we can also calculate a
variable that represents the number of years a person was a customer.

We can also create a categorical variable that categorizes people by
generation.

For example: - The Silent Generation: 1925 - 1945 - Baby Boomers: 1946 -
1964 - Gen X: 1965 - 1979 - Millennials: 1980 - 1996

``` r
# Create the number of years a person was a customer
mkt_data <- mkt_data %>%
  mutate(Yr_Customer = 2021 - lubridate::year(Dt_Customer)) # Extract the year from Dt_Cusomter and subtract it from 2021

# Create a variable for the generation
# First get the min and max year date to see where we should start and end
min(mkt_data$Year_Birth) # 1940
```

    ## [1] 1893

``` r
max(mkt_data$Year_Birth) # 1996
```

    ## [1] 1996

``` r
mkt_data <- mkt_data %>%
  mutate(gen = case_when(
    Year_Birth >= 1925 & Year_Birth <= 1945 ~ "silent",
    Year_Birth > 1945 & Year_Birth <= 1964 ~ "boomer",
    Year_Birth > 1965 & Year_Birth <= 1979 ~ "genx",
    Year_Birth > 1979 & Year_Birth <= 1996 ~ "mill"
  ))
```

## Section 02: Exploratory Data Analysis

### Are there any outliers? How will you wrangle/handle them?

To find out what the outliers are, I need to reduce the data set to just
the numeric data. The numeric data are:

-   Income
-   Age
-   Kidhome
-   Teenhome
-   Recency
-   Mnt\* - Amount spent
-   Num\*Purchases - Number of Purchases
-   NumWebVisitsMonth
-   Accepted\* - Number of accepted marketing campaigns

``` r
mkt_plot <- mkt_data %>%
  select_if(is.numeric) %>% # Only select the numeric data
  select(-ID) # Remove ID

mkt_plot %>%
  pivot_longer(everything(), names_to = "variable", values_to = "response") %>% # Wide -> Long
  ggplot(aes(x = variable, y = response)) + # Set up the aesthetic and plot
  geom_boxplot(width = 0.1) + # Create box plots
  facet_wrap(. ~ variable, scale = "free", ncol = 6) + # Show box plots in a grid
  theme_classic() + 
  theme(strip.text.x = element_blank())
```

![](MarketingEDA_ES_7.8.21_files/figure-gfm/outliers-1.png)<!-- -->

There are some outliers in the Age variable (top right boxplot) - there
seems to be a few customers that are over the age of 100. That is a bit
suspicious.

There are some outliers in the Income variable (2nd row, 2nd column) -
there are some customers that have over $600,000 income. However, this
could be true.

``` r
# Get the outliers for Age
Age_out <- boxplot.stats(mkt_data$Age)
```

There are 3 customers who are the ages of 128, 122, 121. We will remove
these customers who are over the age of 100.

### Are there any missing data? How will you wrangle/handle them?

``` r
missing_data <- mkt_data %>%
  map(~sum(is.na(.))) %>% # Get the number of NAs for each column
  unlist(.) %>% # Strip the list formatting
  as.data.frame(.) %>% # Convert to a data frame
  arrange(desc(.)) %>% # Arrange in descending order
  rename(`N Missing` = ".")

knitr::kable(missing_data)
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
N Missing
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
gen
</td>
<td style="text-align:right;">
77
</td>
</tr>
<tr>
<td style="text-align:left;">
Income
</td>
<td style="text-align:right;">
24
</td>
</tr>
<tr>
<td style="text-align:left;">
ID
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Year\_Birth
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Education
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Marital\_Status
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Kidhome
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Teenhome
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Dt\_Customer
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Recency
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
MntWines
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
MntFruits
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
MntMeatProducts
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
MntFishProducts
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
MntSweetProducts
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
MntGoldProds
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
NumDealsPurchases
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
NumWebPurchases
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
NumCatalogPurchases
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
NumStorePurchases
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
NumWebVisitsMonth
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
AcceptedCmp3
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
AcceptedCmp4
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
AcceptedCmp5
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
AcceptedCmp1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
AcceptedCmp2
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Response
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Complain
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Country
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Age
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Yr\_Customer
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

There is only missing income data for 24 people. This is only 1.0714286%
of the data. We can do the following: 1. Remove the 24 customers who are
missing Income data 2. Use the median Income to impute the data for the
customers missing data

I will remove the following from the data: - The 24 customers who are
missing Income data because it is a very small fraction of the data -
The 3 customers who are over the age of 100

### Let’s check out the categorical variables

``` r
mkt_data %>%
  select_if(is.character) # list out all the categorical variables
```

    ## # A tibble: 2,240 x 4
    ##    Education  Marital_Status Country gen   
    ##    <chr>      <chr>          <chr>   <chr> 
    ##  1 Graduation Divorced       SP      genx  
    ##  2 Graduation Single         CA      boomer
    ##  3 Graduation Married        US      boomer
    ##  4 Graduation Together       AUS     genx  
    ##  5 Graduation Single         SP      mill  
    ##  6 PhD        Single         SP      boomer
    ##  7 2n Cycle   Married        GER     boomer
    ##  8 Graduation Together       SP      genx  
    ##  9 PhD        Married        US      boomer
    ## 10 PhD        Married        IND     boomer
    ## # ... with 2,230 more rows

Let’s see the responses and \# of responses in each categorical variable
- Education - Martial Status - Country - gen

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
Education
</th>
<th style="text-align:right;">
Frequency
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
2n Cycle
</td>
<td style="text-align:right;">
203
</td>
</tr>
<tr>
<td style="text-align:left;">
Basic
</td>
<td style="text-align:right;">
54
</td>
</tr>
<tr>
<td style="text-align:left;">
Graduation
</td>
<td style="text-align:right;">
1127
</td>
</tr>
<tr>
<td style="text-align:left;">
Master
</td>
<td style="text-align:right;">
370
</td>
</tr>
<tr>
<td style="text-align:left;">
PhD
</td>
<td style="text-align:right;">
486
</td>
</tr>
</tbody>
</table>

The education variable has 5 different responses and no apparent
outliers.

# Marital Status

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
Marital Status
</th>
<th style="text-align:right;">
Frequency
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Absurd
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Alone
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Divorced
</td>
<td style="text-align:right;">
232
</td>
</tr>
<tr>
<td style="text-align:left;">
Married
</td>
<td style="text-align:right;">
864
</td>
</tr>
<tr>
<td style="text-align:left;">
Single
</td>
<td style="text-align:right;">
480
</td>
</tr>
<tr>
<td style="text-align:left;">
Together
</td>
<td style="text-align:right;">
580
</td>
</tr>
<tr>
<td style="text-align:left;">
Widow
</td>
<td style="text-align:right;">
77
</td>
</tr>
<tr>
<td style="text-align:left;">
YOLO
</td>
<td style="text-align:right;">
2
</td>
</tr>
</tbody>
</table>

The marital status variable has 8 different variables, but there are a
few outliers. They will be removed from the data. - Absurd: 2 responses
- Alone: 3 responses - YOLO: 2 responses

# Country

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
Country
</th>
<th style="text-align:right;">
Frequency
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
AUS
</td>
<td style="text-align:right;">
160
</td>
</tr>
<tr>
<td style="text-align:left;">
CA
</td>
<td style="text-align:right;">
268
</td>
</tr>
<tr>
<td style="text-align:left;">
GER
</td>
<td style="text-align:right;">
120
</td>
</tr>
<tr>
<td style="text-align:left;">
IND
</td>
<td style="text-align:right;">
148
</td>
</tr>
<tr>
<td style="text-align:left;">
ME
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
SA
</td>
<td style="text-align:right;">
337
</td>
</tr>
<tr>
<td style="text-align:left;">
SP
</td>
<td style="text-align:right;">
1095
</td>
</tr>
<tr>
<td style="text-align:left;">
US
</td>
<td style="text-align:right;">
109
</td>
</tr>
</tbody>
</table>

The Country variable looks good, but the ME country has only 3
responses. ME will be removed from the data.

# Gen

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
Generation
</th>
<th style="text-align:right;">
Frequency
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
boomer
</td>
<td style="text-align:right;">
759
</td>
</tr>
<tr>
<td style="text-align:left;">
genx
</td>
<td style="text-align:right;">
956
</td>
</tr>
<tr>
<td style="text-align:left;">
mill
</td>
<td style="text-align:right;">
424
</td>
</tr>
<tr>
<td style="text-align:left;">
silent
</td>
<td style="text-align:right;">
24
</td>
</tr>
</tbody>
</table>

The gen variable also looks OK. Although the silent generation is rather
low, no data will be removed.

### Removing the variables

``` r
mkt_data_ana <- mkt_data %>%
  filter(!is.na(Income), Age < 100) %>% # Remove the NAs from Income and Age
  filter(!(Marital_Status %in% c("Absurd", "Alone", "YOLO"))) %>% # Remove the low variables from Marital Status
  filter(!(Country) %in% c("ME")) # Remove ME from the data
```

We dropped 37 customers from the analysis.

### Factorizing the categorical variables

The categorical variable will transformed into factors to make sure that
statistics are handled correctly.

``` r
mkt_data_ana <- mkt_data_ana %>%
  mutate(Education      = factor(Education),
         Marital_Status = factor(Marital_Status),
         Country        = factor(Country),
         gen            = factor(gen))
```

# Make the plot w/ generation by \# purchases

### Are there any useful variables that you can engineer with the given data?

We can sum the data that is split by the different types of products,
marketing campaigns accepted, purchases, and number of children.

``` r
mkt_data_ana <- mkt_data_ana %>%
  mutate(tot_mnt = MntWines + MntFruits + MntMeatProducts + MntFishProducts + MntSweetProducts + MntGoldProds,
         num_accepted = AcceptedCmp1 + AcceptedCmp2 + AcceptedCmp3 + AcceptedCmp4 + AcceptedCmp5,
         tot_pur = NumDealsPurchases + NumWebPurchases + NumCatalogPurchases + NumStorePurchases,
         num_children = Teenhome + Kidhome)
```

-   Do you notice any patterns or anomalies in the data? Can you plot
    them?

## USE THIS <http://www.sthda.com/english/articles/38-regression-model-validation/157-cross-validation-essentials-in-r/>

# <https://github.com/topepo/caret/issues/876>

## Do a model w/ all and select significant and then do a stepwise model

### Section 03: Data Visualization

Please plot and visualize the answers to the below questions.

-   Which marketing campaign is most successful?
-   What does the average customer look like for this company?
-   Which products are performing best?
-   Which channels are under performing?

### Section 04: Statistical Analysis

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
