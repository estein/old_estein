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

# Machine learning package
library(caret)

# Formatting package
library(knitr)
```

#### Task Details

You’re a marketing analyst and you’ve been told by the Chief Marketing
Officer that recent marketing campaigns have not been as effective as
they were expected to be. You need to analyze the data set to understand
this problem and propose data-driven solutions.

#### Expected Submission

Submit a well documented notebook with these four sections:

#### Section 01: Exploratory Data Analysis

-   Are there any null values or outliers? How will you wrangle/handle
    them?
-   Are there any variables that warrant transformations?
-   Are there any useful variables that you can engineer with the given
    data?
-   Do you notice any patterns or anomalies in the data? Can you plot
    them?

#### Section 02: Statistical Analysis

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

#### Section 03: Data Visualization

Please plot and visualize the answers to the below questions.

-   Which marketing campaign is most successful?
-   What does the average customer look like for this company?
-   Which products are performing best?
-   Which channels are under performing?

#### Section 04: CMO Recommendations

Bring together everything from Sections 01 to 03 and provide data-driven
recommendations/suggestions to your CMO.

#### Evaluation

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

#### Section 01: Exploratory Data Analysis

-   Are there any null values or outliers? How will you wrangle/handle
    them?
-   Are there any variables that warrant transformations?
-   Are there any useful variables that you can engineer with the given
    data?
-   Do you notice any patterns or anomalies in the data? Can you plot
    them?

``` r
summary(cars)
```

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00
