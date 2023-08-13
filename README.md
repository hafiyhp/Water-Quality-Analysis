## R-Scripts-Visualisation-Analysis ##

## Physico-chemical Water Parameter analysis ##

## Introduction and Data Source ##

This script performs data analysis on River Cabrach water quality data from 2018 to 2023. 
The water temperature, depth and electrical conductivity at 25°C used in this case study were 
collected from the River Cabrach monitoring system. The monitoring system is equipped with
sensors that continuously record these physico-chemical parameters at daily intervals 
throughout each year. The data that were recorded on a regular basis from 2018 to 2023 were 
used to examine whether the temporal variations in these parameters over a 6-year period.
This project aims to analyze the water quality data from the River Cabrach to understand how 
climate changes impact the adjacent peatland ecosystem. 
By examining the water quality trends, we can gain insights into the potential effects of
climate change on peatland health and make informed conservation decisions.

## Analysis Steps ##

# 1) Data Preparation

The initial step involves organizing raw dataset using Excel into proper 'csv' format. 
The data is read into R using the `read.csv` function. 
Date columns are converted to the appropriate date format using `as.Date`. 
This is critical for consistent and meaningful time series analysis. 
Time series objects are created using the `ts` function.
This helps to facilitate time-based analysis, as this observation converts the data
into a format that takes the temporal nature of the observations into consideration.
Time series objects are essential for detecting trends, seasonality, and other temporal patterns.

By following these data preparation steps, we establish a robust foundation for
our exploratory data analysis and subsequent modeling.
The refined dataset, containing correctly formatted dates and structured time series
objects, allows us to extract meaningful insights into the dynamics of the River Cabrach watwr quality data.

# 2) Seasonal Analysis
The data is divided into seasons (spring, summer, autumn, and winter) based
on the month of the year using 'lubridate'. ANOVA is employed to examine whether there are
significant differences in water quality parameters across seasons.

# 3) Exploratory Data Analysis
The following R packages were utilized to extract insights from the River Cabrach
water quality dataset:

- lubdriate: Utilized to manage date and time data efficiently, ensuring accuracy in
  time series analysis.
- dplyr: Employed for manipulating and transforming data, allowing for efficient
  data wrangling tasks.
- ggplot2: Utilized for creating visually appealing data in a more informative manner.
- gridExtra and patchwork: Employed for arranging and customising multiple plots for
  thorough insights.
- rstatix: Used for conducting statistical analyses and hypothesis tests to gather
  significant conclusions. 
- mgcv: Utilized for fitting and intepreting general additive models (GAMs), capturing
  non-linear relationships in the data.
- stats: Utilized for traditional statistical methods, including ANOVA, facilitating the
  comparison of means across different groups.

Furthermore, here is a brief overview of each statistical technique used:
a) Polynomial Regression Model: These models increase linear regression via
higher-degree polynomial terms included in the model. This model allows us to capture
more complex relationships between parameters while increasing model complexity.
A second-degree polynomial model was fitted to the time series analysis data to capture
the potential inverted-U shaped patterns found between parameters.
A first-degree polynomial model was, on the other hand, fitted to the more linear
time series analysis data.

b) General Additive Model (GAM): This model was used to examine non-linear 
relationship between variables. GAM is particularly flexible in describing the
complex relationships on smooth functions, capturing underlying intricate patterns.

c) Analysis of Variance (ANOVA): This model was used to analyze the difference between means
across different groups, assessing whether the differences are statistically
significant.

# 4) Residual Analysis

Residuals are calculated to exmaine the deviation of observations from the model's
predictions. These residuals are the used to understand the variance and patterns
in the data i.e. homoscedasticity and normality in the models. 

Homoscedasticity was evaluated by plotting the residuals against predicted values,
examining for any distinguishable variance and patterns. Normality was assessed
by plotting the histogram and Q-Q plot test. These diagnostic checks ensure the
validity of the models and the reliability of the statistical inferences made.

# Usage

1. Clone this repository to your local machine.
2. Open the R script `Water_Quality_2018_to_2023.R` in RStudio.
3. Modify the file paths if needed to point to your local data files.
4. Run the script step by step or all at once to perform the analysis.
5. Refer to the generated plots and results to understand water quality trends.

# Notable Results

1) The time series analysis illustrated fluctuating inverted-U shaped patterns
between each parameter (water temperature, electrical conductivity at 25°C, water
depth) and years. Significant polynomial regression models were employed to analyze
the relationship between water quality parameters and time. The models demonstrated
strong correlations between each parameter and years, with p-values < 0.05,
indicating significant trends in the data.

2) Utilizing GAMs, significant correlations were observed between log-transformed EC
levels and water depth in each year, with robust relationships indicated by low
p-values (all p < 0.05) and consistenly high percentages of deviance explained
(ranging from 91.1% to 95.6%).

Additionally, the log-transformed electrical conductivity followed a general 
fat-tailed distribution across all the observed years. This indicates that the 
residuals do not follow normal distribution and have heavier tails compared to a 
normal distribution. This observation aligns with the Q-Q plot showing a deviation
from the diagonal line in the tails.

3) GAMs also revealed significant relationships between log-transformed EC and
water temperature for the years. The smooth terms of water temperature are highly
significant (p < 2e-16) for each year, indicating nonlinear associations. The models
explain varying percentages of deviance, with adjusted R-squared values ranging
from 0.122 to 0.579. However, the deviance explained percentages are relatively
modest in some cases, suggesting that a portion of the variability remains
unexplained for.

4) Examining multi-year data for water temperature, depth, and electrical conductivity
reveals consistent seasonal trends through ANOVA. In 2023, water temperature increases
in summer and drops in winter, while depth decreases during summer and increases in
winter. Electrical conductivity shows variations in both seasons. Similar trends
were observed in 2022, 2021, and 2020. These trends emphasize the strong seasonal
relationship across these parameters

# License

This project is placed under the Unlicense. It is effectively unlicensed, meaning it
is in the public domain. You are free to use, modify, and distribute the code without 
any restrictions.

# Contact
If you have any questions or suggestions, feel free to contact me at [hafiyhp@gmail.com].

