## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 4. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse.
getwd()
setwd(dir='...') 


library(tidyverse)

# Import the data set.

library(readr)
sales <- read_csv('turtle_sales.csv')

# Print the data frame.

sales
view(sales)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
sales_subset <- subset (sales, select = -c(Ranking, Year, Genre, Publisher))

# View the data frame.

view(sales_subset)

# View the descriptive statistics.

summary(sales_subset)


################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.

qplot(NA_Sales, EU_Sales, data=sales_subset)
qplot(NA_Sales, Global_Sales, data=sales_subset)
qplot(EU_Sales, Global_Sales, data=sales_subset)

## 2b) Histograms
# Create histograms.

qplot(NA_Sales, bins=20, data=sales_subset)
qplot(EU_Sales, bins=20, data=sales_subset)
qplot(Global_Sales, bins=20, data=sales_subset)

## 2c) Boxplots
# Create boxplots.

qplot(NA_Sales, data=sales_subset, colour = I("red"), geom="boxplot")
qplot(EU_Sales, data=sales_subset, colour = I("red"), geom="boxplot")
qplot(Global_Sales, data=sales_subset, colour = I("red"), geom="boxplot")

###############################################################################

# 3. Determine the impact on sales per product_id.

## 3a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.


product_sales <- sales %>% group_by(Product) %>% summarise(across(.cols = c(NA_Sales, EU_Sales, Global_Sales), list(sum = sum)))

# View the data frame.

head(as_tibble(product_sales))
view(product_sales)

# Explore the data frame.

summary(product_sales)


## 3b) Determine which plot is the best to compare game sales.
# Create scatterplots.

qplot(NA_Sales_sum, EU_Sales_sum, data=product_sales)
qplot(NA_Sales_sum, Global_Sales_sum, data=product_sales)
qplot(EU_Sales_sum, Global_Sales_sum, data=product_sales)

# Create histograms.

qplot(NA_Sales, bins=20, data=sales_subset)
qplot(EU_Sales, bins=20, data=sales_subset)
qplot(Global_Sales, bins=20, data=sales_subset)

# Create boxplots.

qplot(NA_Sales, data=sales_subset, colour = I("red"), geom="boxplot")
qplot(EU_Sales, data=sales_subset, colour = I("red"), geom="boxplot")
qplot(Global_Sales, data=sales_subset, colour = I("red"), geom="boxplot")

###############################################################################

# 4. Observations and insights

## Your observations and insights here ......

## Sales between NA, EU and Global. On average there are more 
# NA (2.5) sales than EU (1.6)

## Scatterplots - NA and EU sales visually have a slight positive trend, 
# which likely indicates success in one region of a game translates to another 
# region, with a few succeeding in NA but not EU. Global sales correlate 
# positively stronger with both NA and EU expectantly due to being a 
# sum of both values.


## Histograms
# Producing histograms of each region, it can be seen sales produce a 
# positively skewed graph. This means there are larger quantities of 
#products that have fewer sales than those that produced larger volumes. 


## Boxplots
# The boxplots of sales indicate no outliers for any region below 
# the lower limit, however, supporting the findings of the histograms, 
# there are several above the upper limit for all regions, indicating again
# that most products producing lower volumes of sales with
# few successful outlier sellers. 



###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 3. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 4. Include your insights and observations.

################################################################################

# 1. Load and explore the data



# View data frame created in Week 4.

head(as_tibble(sales_subset))
head(as_tibble(product_sales))


# Check output: Determine the min, max, and mean values.

min(product_sales$NA_Sales_sum)
min(product_sales$EU_Sales_sum)
min(product_sales$Global_Sales_sum)

mean(product_sales$NA_Sales_sum) 
mean(product_sales$EU_Sales_sum)
mean(product_sales$Global_Sales_sum)

max(product_sales$NA_Sales_sum) 
max(product_sales$EU_Sales_sum)
max(product_sales$Global_Sales_sum)

# View the descriptive statistics.

summary(sales_subset)

summary(product_sales)

###############################################################################

# 2. Determine the normality of the data set.

## 2a) Create Q-Q Plots
# Create Q-Q Plots.

qqnorm(product_sales$NA_Sales_sum)
qqline(product_sales$NA_Sales_sum)

qqnorm(product_sales$EU_Sales_sum)
qqline(product_sales$EU_Sales_sum)

qqnorm(product_sales$Global_Sales_sum)
qqline(product_sales$Global_Sales_sum)

## 2b) Perform Shapiro-Wilk test
# Install and import Moments.


library(moments)

# Perform Shapiro-Wilk test.

shapiro.test((product_sales$NA_Sales_sum))
shapiro.test((product_sales$EU_Sales_sum))
shapiro.test((product_sales$Global_Sales_sum))


## 2c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.

skewness(product_sales$NA_Sales_sum) 
kurtosis(product_sales$NA_Sales_sum) 

skewness(product_sales$EU_Sales_sum)
kurtosis(product_sales$EU_Sales_sum)

skewness(product_sales$Global_Sales_sum)
kurtosis(product_sales$Global_Sales_sum)


## 2d) Determine correlation
# Determine correlation.

cor(product_sales$Global_Sales_sum, product_sales$NA_Sales_sum)
cor(product_sales$Global_Sales_sum, product_sales$EU_Sales_sum)
cor(product_sales$EU_Sales_sum, product_sales$NA_Sales_sum)

###############################################################################

# 3. Plot the data
# Create plots to gain insights into data.

# Scatter plots for sale regions

ggplot(data=product_sales, 
       mapping=aes(x=Global_Sales_sum, y=NA_Sales_sum)) + 
  geom_point(color = "red",
             alpha = .5,
             size = 3) + 
  geom_smooth(method = "lm", se=FALSE, size=1.5) +
labs(title= "Global sales vs NA sales by product")



ggplot(data=product_sales, 
       mapping=aes(x=Global_Sales_sum, y=EU_Sales_sum)) + 
  geom_point(color = "green",
             alpha = .5,
             size = 3) + 
  geom_smooth(method = "lm", se=FALSE, size=1.5) +
labs(title= "Global sales vs EU sales by product")


ggplot(data=product_sales, 
       mapping=aes(x=NA_Sales_sum, y=EU_Sales_sum)) + 
  geom_point(color = "orange",
             alpha = .5,
             size = 3) + 
  geom_smooth(method = "lm", se=FALSE, size=1.5) +
labs(title= "EU sales vs NA sales by product")


###############################################################################

# 4. Observations and insights
# Your observations and insights here...

# Viewing data grouped by product id, similar patterns arise in NA selling 
# more on average than EU, and some products still selling close to 0 in those 
# regions. However, the minimum and mean sales for global increase from £0.01M 
# to £4.2M and £5.3M to £10.7M respectively. 

# Q-Q plots - the points begin to deviate from the diagonal line towards the 
# tails for each region, greatly toward the end tail. This signifies the data 
# set is not normally distributed, which was observed on the previously created 
# histograms. To test for normality statistically, a Shapiro test was applied 
# to the sales of each region with NA, EU and Global produce very small p-values 
# (less than 0.05), indicating the data is not normally distributed.

# Skewness values for all regions are above 2.8, as this is above 1 it indicates
# the data is highly positively skewed. Kurtosis values are all above 15, 
# as they are above 3 this indicates a leptokurtic distribution where the data 
# is not normally distributed and there are many outliers which has been seen 
# from the histograms and boxplots. 

# Global sales have a strong correlation of 0.92 and 0.85 with NA and EU sales
# respectively, which is as expected due to being a sum these sales. NA and EU 
# sales has a positive correlation of 0.62 indicating that if a product is
# successful in one continent, it is likely to do well in an another.  

###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explor the data
# View data frame created in Week 5.

head(as_tibble(product_sales))

# Determine a summary of the data frame.

summary(product_sales)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.


model1 <- select(product_sales, NA_Sales_sum, Global_Sales_sum)
cor(model1)

model2 <- select(product_sales, EU_Sales_sum, Global_Sales_sum)
cor(model2)

model3 <- select(product_sales, NA_Sales_sum, EU_Sales_sum)
cor(model3)

## 2b) Create a plot (simple linear regression)
# Basic visualisation.


ggplot(model1, aes(NA_Sales_sum, Global_Sales_sum)) +
  geom_point()

ggplot(model2, aes(EU_Sales_sum, Global_Sales_sum)) +
  geom_point()

ggplot(model3, aes(NA_Sales_sum, EU_Sales_sum)) +
  geom_point()


###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.


# Multiple linear regression model.

# Multiple linear regression model without 'Product' included.

mlr_data <- subset(product_sales, select=-c(Product))
model_mlr <- lm(Global_Sales_sum~., data = mlr_data)
model_mlr
summary(model_mlr)

# Multiple linear regression model with 'Product' included.

model_mlr2 <- lm(Global_Sales_sum~., data = product_sales)
model_mlr2
summary(model_mlr2)

###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.

# Rows are set to be 5 values from the data. Model is used to test predicting
# the actual values.

actual_global <- c(67.85, 8.36, 4.32, 5.60, 23.21)
NA_Sales_sum <- c(34.02, 3.94, 2.73, 2.27, 22.08)
EU_Sales_sum <- c(23.80, 1.28, 0.65, 2.30, 0.52)

predict_data <- data.frame(NA_Sales_sum, EU_Sales_sum)
predictTest = predict(model_mlr, newdata = predict_data, interval='confidence')

Actual_vs_Test = data.frame(actual_global, predictTest)
Actual_vs_Test

# Model including 'Product' is tested predicting actual values.

Product <- c(107, 6715, 6815, 3678, 326)

predict_data <- data.frame(Product, NA_Sales_sum, EU_Sales_sum)
predictTest = predict(model_mlr2, newdata = predict_data, interval='confidence')

Actual_vs_Test = data.frame(actual_global, predictTest)
Actual_vs_Test


###############################################################################

# 5. Observations and insights

# Your observations and insights here...
# the adjusted R-squared value is larger with product id, making that the 
# stronger model. Both were then tested using 5 examples from the data frame, 
# with including product id giving a good prediction of 2 values which fell 
# within the confidence interval, whereas only 1 was good when discarding 
# those values. 

###############################################################################
###############################################################################




