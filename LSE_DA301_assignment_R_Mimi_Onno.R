## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact


###############################################################################

# Install the packages
install.packages('tidyverse')
install.packages('skimr')
install.packages('DataExplorer') 

# Import the libraries
library(tidyverse)

# Create statistical summaries.
library(skimr)

# Create a report as an HTML file.
library(DataExplorer)

###############################################################################

# Determine your working directory
getwd()

# Change your current directory.
setwd(dir='C:/Users/mimis/Desktop/LSE - Course 3 Assignment/LSE_DA301_assignment_files/') 


# Define the colours used for plot
cbp <- c("#000000", "#d14e72", "#56B4E9", "#009E73", "#82206c",
         "#d8e219", "#0072B2", "#D55E00", "#CC79A7", "#999999", "#fde725",
          "#1f968b", "#39558c", "#802582", "#feb47b", "#f1814d", "#0d0887",
          "#98d83e", "#2b748e", "#fcffa4", "#fcb014", "#b7318a","#fecd90")

###############################################################################
# Import data sets
sales <- read.csv('turtle_sales.csv', header = TRUE)

# Print the data frame.
View(sales)

# Create a new data frame from a subset of the sales data frame.
sales1 <- select(sales, -Ranking, -Year, -Genre, -Publisher)

###############################################################################
# View the data sets
View(sales1)

# View the titles or names of the columns in the data set.
colnames(sales1)

# Examine the structure of sales1
str(sales1)

###############################################################################
# Explore the data sets

# Check missing values 
sales1[is.na(sales1)]
# No missing values found

###############################################################################
#  Descriptive statistics

#  Summary sales1
summary(sales1)

# Summary product id
summary(sales1$Product)

# How many products ids
products_count <- sales1 %>% group_by(Product) %>% count(Product)
# 175 unique product ids

# Summary products_count
summary(products_count)
# Max no of platforms by products ids is 9
# On average each products run on 2 different platforms

# How many platforms
platform_types <- sales1 %>% group_by(Platform) %>% count(Platform)
# 22 platforms

# Summary platform_types
# - Max of products across the products is 47
# - On average each platform run across 16 products
summary(platform_types)

# This function creates a downloadable HTML file containing summary stats of
# the data set.
DataExplorer::create_report(sales1)
################################################################################

# 2. Review plots to determine insights into the data set.

# Pairplot sales1
plot(sales1)

## 2a) Scatterplots

# Check if there is any relationship between Platform and Product
qplot(Platform, Product, data=sales1, xlab = "Platform", ylab = "Product")

# Determine EU Sales trends by Platform
p <- qplot(y = EU_Sales, data=sales1, ylab = "Sales in Europe £M", color = factor(Platform))
p + scale_colour_manual(values=cbp)

# Determine NA Sales trends by Platform
p <- qplot(y = NA_Sales, data=sales1, ylab = "Sales in North America £M", color = factor(Platform))
p + scale_colour_manual(values=cbp)

# Determine Global Sales trends by Platform
p <- qplot(y = Global_Sales, data=sales1, ylab = "Global Sales £M",  color = factor(Platform))
p + scale_colour_manual(values=cbp)

## 2b) Histograms

# Identify which Platforms are popular
qplot(Platform, data=sales1, xlab = "Platform")

# Determine the product trend by Platform
p <- qplot(Product, bins = 50, fill = Platform, data=sales1)
p + scale_fill_manual(values=cbp)

# Count product
hist(sales1$Product, breaks = 50)

## 2c) Boxplots

# Determine the global sales trends
qplot(Global_Sales, Platform, data=sales1, xlab = "Global Sales £M", geom='boxplot')

# Determine the NA sales trends
qplot(NA_Sales, Platform, data=sales1, xlab = "North America Sales £M", geom='boxplot')

# Determine the Europe sales trends
qplot(EU_Sales, Platform, data=sales1, xlab = "Europe Sales £M", geom='boxplot')

###############################################################################

# 3. Determine the impact on sales per product_id.

## 3a) Use the group_by and aggregate functions.

# Global sales by product
global_product_sales <- sales1 %>% 
                        group_by(Product) %>% 
                        summarise(global_sum = sum(Global_Sales, na.rm = TRUE), global_no_platforms = n())

#%>% arrange(desc(global_sum))

# View global sales by product
view(global_product_sales)


# summary global sales by product
summary(global_product_sales)

# Europe sales by product
eu_product_sales <- sales1 %>% 
                    group_by(Product) %>% 
                    summarise(eu_sum = sum(EU_Sales, na.rm = TRUE), eu_no_platforms = n()) 
#%>% arrange(desc(eu_sum))

# View eu sales by product
view(eu_product_sales)

# summary eu sales by product
summary(eu_product_sales)

# North America sales by product
na_product_sales <- sales1 %>% 
                    group_by(Product) %>% 
                    summarise(na_sum = sum(NA_Sales, na.rm = TRUE), na_no_platforms = n()) 
#%>% arrange(desc(na_sum))

# View na sales by product
view(na_product_sales)

# summary na sales by product
summary(na_product_sales)


## 3b) Determine which plot is the best to compare game sales.

# Calculate Other sales

others <- global_product_sales$global_sum - (na_product_sales$na_sum + eu_product_sales$eu_sum)

# Prepare the dataframe with sales from EU, NA, other_sales & Global

product <- eu_product_sales$Product
eu_sales <- eu_product_sales$eu_sum
na_sales <- na_product_sales$na_sum
global_sales <- global_product_sales$global_sum

df <- data.frame(product, eu_sales, na_sales, other_sales = round(others, digits = 2), global_sales)

view(df)


# Create boxplots

# Boxplot global sales
qplot(global_sum, data=global_product_sales, colour=I('blue'), xlab = "Global Sales £M", geom='boxplot')


# Boxplot Europe sales
qplot(eu_sum, data=eu_product_sales, colour=I('blue'), xlab = "Europe Sales £M", geom='boxplot')

# Boxplot North America sales
qplot(na_sum, data=na_product_sales, colour=I('blue'), xlab = "North America Sales £M", geom='boxplot')

# Prepare the dataframe with platforms from EU, NA, other_sales & Global

product <- eu_product_sales$Product
eu_platform <- eu_product_sales$eu_no_platforms
na_platform <- na_product_sales$na_no_platforms
global_platform <- global_product_sales$global_no_platforms

df_platform <- data.frame(product, eu_platform, na_platform, global_platform)

view(df_platform)

# Create scatterplots.

# Determine sales in both Europe and North America
qplot(eu_sales, na_sales, data=df, color = factor(product), xlab = "Europe sales £M", ylab = "North America sales £M", main = "Europe sales v North America sales (£M)")

# Determine sales in both Europe and Other
qplot(eu_sales, other_sales, data=df, color = factor(product), xlab = "Europe sales £M", ylab = "Other sales £M",  main = "Europe sales v Other sales (£M)")

# Determine sales in both North America and Other
qplot(na_sales, other_sales, data=df, color = factor(product), xlab = "North America sales £M", ylab = "Other sales £M", main = "North America sales v Other sales (£M)")


# Determine relationship between sales and product - global
qplot(Product, global_sum, color = factor(Product), data=global_product_sales, xlab = "Product", ylab = "Global sales £M")

# Determine relationship between sales and product - Europe
qplot(Product, eu_sum, color = factor(Product), data=eu_product_sales, xlab = "Product", ylab = "Europe sales £M")

# Determine relation between sales and product - North America
qplot(Product, na_sum, color = factor(Product), data=na_product_sales, xlab = "Product", ylab = "North America sales £M")

# Create histograms.

# Historam global sales
global <- mutate(global_product_sales, pid = as.factor(Product))
qplot(global_sum,  bins = 30, fill = pid, data=global, xlab = "Global Sales £M")

# Historam Europe sales
eu <- mutate(eu_product_sales, pid = as.factor(Product))
qplot(eu_sum,  bins = 30, fill = pid, data=eu, xlab = "Europe Sales £M")

# Historam North America sales
na <- mutate(na_product_sales, pid = as.factor(Product))
qplot(na_sum,  bins = 30, fill = pid, data=na, xlab = "North America Sales £M")


###############################################################################

# 4. Observations and insights

# There are over 100 products with over 20 different platforms. 
# A mix of summary tables, scatterplots, histograms and boxplots are used to 
# explore the trends and insights of the game sales at Turtle Games. 
# The visualisations are used to identify the sales trends, at this stage, 
# it is not possible to visualise the individual products by colours.

# With the wide product ranges, scatterplots are best to compare the games sales.
# X360, PS3, PC, Wii and DS are the most popular platforms. 
# Wii is the most popular platform which generate the highest sales in both Europe
# and North America. 
# Product ids 107 and 123 generate the top 5 highest sales in both Europe and 
# North America. 
# In Europe, one of the highest product id sales 3967 run on the maximum of 
# 9 platforms. 
# The product ids with the highest sales in the North America tends to be lower 
# numbers in comparison to Europe.
# On average, the sales in both Europe and North America are between £2M to £3M. 
# There are many interesting outliers in both markets which generate significant
# higher sales. 
# It is important to further analyse the product ids and platforms of these
# outliers to better understand the market trends.

###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and manipulating data using R

################################################################################

# Load and explore the data

# View data frame group by product ids
View(df)

# Determine descriptive statistics of df from week 4 group by products
summary(df)
summary(df$eu_sales)
summary(df$na_sales)
summary(df$other_sales)
summary(df$global_sales)

# Create new data set with individuals with global sales less than £30M
# remove the outliers
df1 <- filter(df, df$global_sales<30.00)  

# View the df1
View(df1)

# View descriptive statistics of df1 after removing outliers
summary(df1)

# Europe sales
# Determine the `min`, `max` and `mean` values of all Europe sales data.
eu_min <- min(df1$eu_sales)  
eu_max <- max(df1$eu_sales) 
eu_mean <- mean(df1$eu_sales)
summary(df1$eu_sales)

# North America sales
# Determine the `min`, `max` and `mean` values of all North America sales data.
na_min <- min(df1$na_sales)  
na_max <- max(df1$na_sales) 
na_mean <- mean(df1$na_sales)
summary(df1$na_sales)

# Other sales
# Determine the `min`, `max` and `mean` values of all Other sales data.
other_min <- min(df1$other_sales)  
other_max <- max(df1$other_sales) 
other_mean <- mean(df1$other_sales)
summary(df1$other_sales)

# Global sales
# Determine the `min`, `max` and `mean` values of all Global sales data.
global_min <- min(df1$global_sales)  
global_max <- max(df1$global_sales) 
global_mean <- mean(df1$global_sales)
summary(df1$global_sales)

# Create a data frame
sales.df <- data.frame(europe = c(eu_min, eu_max, eu_mean),
                        na = c(na_min, na_max, na_mean),
                        global = c(global_min, global_max, global_mean))

# view sales.df
View(sales.df)

###############################################################################

# Determine the normality of the data set.

# Create Q-Q Plots.

# Europe sales
qqnorm(df1$eu_sales)
qqline(df1$eu_sales) 

# North America sales
qqnorm(df1$na_sales)
qqline(df1$na_sales) 

# Other sales
qqnorm(df1$other_sales)
qqline(df1$other_sales) 

# Global sales
qqnorm(df1$global_sales)
qqline(df1$global_sales) 


## Perform Shapiro-Wilk test

# Install and import Moments.
install.packages('moments') 
library(moments)

# Europe sales
shapiro.test(df1$eu_sales)
# assuming the null hypothesis is true(that the data are normally distributed)
# p value is 1.42e-11, the Europe sales is not normally distributed

# North America sales
shapiro.test(df1$na_sales)
# assuming the null hypothesis is true
# p value is 4.618e-14, the North America sales 
# is not normally distributed

# Other sales
shapiro.test(df1$other_sales)
# assuming the null hypothesis is true
# p value is 9.64e-12, the North America sales 
# is not normally distributed

# Global sales
shapiro.test(df1$global_sales)
# assuming the null hypothesis is true
# p value is 8.212e-13, the North America sales 
# is not normally distributed


# p values
p_df <- data.frame( Europe = c(1.42e-11 ),
                    North_America = c(4.618e-14),
                    Global = c(8.212e-13 ))

# View p values
View(p_df)

## Determine Skewness and Kurtosis

# Europe sales
skewness(df1$eu_sales) 
kurtosis(df1$eu_sales)
# 1.247008 positive skew,right-skewed and biased towards higher values
# 3.719443 means Europe sales has a heavier tail, data is leptokurtik

# North America sales
skewness(df1$na_sales) 
kurtosis(df1$na_sales)
#  2.097169 positive skew, right-skewed and biased towards higher values
# 8.877347 means North America sales has a heavier tail, data is leptokurtik

# Other sales
skewness(df1$other_sales) 
kurtosis(df1$other_sales)

# Global sales
skewness(df1$global_sales) 
kurtosis(df1$global_sales)
# 1.352425 positive skew, right-skewed and biased towards higher values
# 4.033009 means Global sales has a heavier tail, data is leptokurtik

## Determine if there are any correlation(s) between the sales data columns.

# Determine the correlation for the whole data frame df1.
round (cor(df1),
       digits=2)

###############################################################################

# Create plots to gain insights into the sales data

##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.

view(df1)

df1$product <- factor(df1$product)

# Compare Europe sales against North America sales
ggplot(data=df1, 
       # Add mapping elements.
       mapping=aes(x = eu_sales, y = na_sales)) + 
  geom_jitter(color="cornflowerblue", width = 0.5, size = 2, alpha=0.8, na.rm = TRUE) +
  geom_smooth(method = "lm", size = 1.5,
              color = "darkgrey",
              se = FALSE) +
  scale_y_continuous(breaks = seq(0, 24, 2),
                     limits = c(0, 24)) +
  scale_x_continuous(breaks = seq(0, 12, 2), 
                     limits=c(0, 12)) +
  labs(x = "Europe sales £M",
       y = "North America sales £M",
       title = "Europe sales(£M) vs. North America sales(£M)",
       subtitle = "Grouped by products")

        
# Compare Europe sales against Other sales
ggplot(data=df1, 
       # Add mapping elements.
       mapping=aes(x = eu_sales, y = other_sales)) + 
  geom_jitter(color="cornflowerblue", width = 0.5, size = 2, alpha=0.8, na.rm = TRUE) +
  geom_smooth(method = "lm", size = 1.5,
              color = "darkgrey",
              se = FALSE) +
  scale_y_continuous(breaks = seq(0, 10, 2),
                     limits = c(0, 10)) +
  scale_x_continuous(breaks = seq(0, 12, 2), 
                     limits=c(0, 12)) +
  labs(x = "Europe sales £M",
       y = "Other sales £M",
       title = "Europe sales(£M) vs. Other sales(£M)",
       subtitle = "Grouped by products")

# Compare North America sales against Other sales
ggplot(data=df1, 
       # Add mapping elements.
       mapping=aes(x = na_sales, y = other_sales)) + 
  geom_jitter(color="cornflowerblue", width = 0.5, size = 2, alpha=0.8, na.rm = TRUE) +
  geom_smooth(method = "lm", size = 1.5,
              color = "darkgrey",
              se = FALSE) +
  scale_y_continuous(breaks = seq(0, 10, 2),
                     limits = c(0, 10)) +
  scale_x_continuous(breaks = seq(0, 24, 2), 
                     limits=c(0, 24)) +
  labs(x = "North America sales £M",
       y = "Other sales £M",
       title = "North America sales(£M) vs. Other sales(£M)",
       subtitle = "Grouped by products")

################################################################################
# Impact of sales by product ids

# Install ggrepel package
install.packages("ggrepel")          
library("ggrepel")   

# Set maximum overlap to infinite
options(ggrepel.max.overlaps = Inf)


# Impact of Europe sales by product id
ggplot(df, aes(x=product, y=eu_sales, label = product)) + 
  geom_jitter(width = 1, size = 3, color="cornflowerblue", na.rm = TRUE ) + 
  geom_text_repel(hjust=1, vjust=1, na.rm = TRUE) +
  scale_y_continuous(breaks = seq(-2, 24, 2),
                     limits = c(-2, 24)) +
  scale_x_discrete(labels = NULL) +
  labs( x = "Products",
        y = "Europe sales £M", 
        title = "Europe sales(£M) by Products") +
  coord_flip() +
  theme_minimal() 
  
# Impact of North America sales by product id
ggplot(df, aes(x=product, y=na_sales, label = product)) + 
  geom_jitter(width = 1, size = 3, color="#33cc00", na.rm = TRUE ) + 
  geom_text_repel(hjust=1, vjust=1, na.rm = TRUE) +
  scale_y_continuous(breaks = seq(0, 36, 2),
                     limits = c(0, 36)) +
  scale_x_discrete(labels = NULL) +
  labs( x = "Products",
        y = "North America sales £M", 
        title = "North America sales(£M) by Products") +
  coord_flip() +
  theme_minimal() 

# Impact of Other sales by product id
ggplot(df, aes(x=product, y=other_sales, label = product)) + 
  geom_jitter(width = 1, size = 3, color="#CCCC33", na.rm = TRUE ) + 
  geom_text_repel(hjust=1, vjust=1, na.rm = TRUE) +
  scale_y_continuous(breaks = seq(0, 12, 0.5),
                     limits = c(0, 12)) +
  scale_x_discrete(labels = NULL) +
  labs( x = "Products",
        y = "Other sales £M", 
        title = "Other sales(£M) by Products") +
  coord_flip() +
  theme_minimal() 



# Impact of Global sales by product id
ggplot(df, aes(x=product, y=global_sales, label = product)) + 
  geom_jitter(width = 1, size = 3, color="#9999FF", na.rm = TRUE ) + 
  geom_text_repel(hjust=1, vjust=0, na.rm = TRUE) +
  scale_y_continuous(breaks = seq(0, 70, 2),
                     limits = c(0, 70)) +
  scale_x_discrete(labels = NULL) +
  labs( x = "Products",
        y = "Global sales £M", 
        title = "Global sales(£M) by Products") +
  coord_flip() +
  theme_minimal() 

###############################################################################

# Week 5 - Observations and insights:
# The means and medians across all markets are not close and similar. 
# Even after removing outliers of over £30M, the patterns are similar. 
# The maximum sales decrease in Europe and North America while other sales maintain. 
# This implies the outliers have a significant effect on the maximum sales in 
# Europe and North America.
# The normality of the data sets are tested using 3 different methods:
# Q-Q plots:
# The data sets of all sales tend to follow the reference line in the middle ranges, 
# and with data points move further away at the bottom and the top. The Q-Q plots 
# cannot suggest normality of the data sets.
# Shapiro-Wilk test:
# The p-values of all market sales are very small less than 5%. 
# These suggest normality is a poor fit for the sales data.
# Skewness and kurtosis:
# The results from all markets suggest data sets are leptokurtic, positive skew, 
# right-skewed and biased towards higher values with heavier tail.
# There results and trends show positive correlations between Europe, 
# North America and Others.
# The correlations suggest stronger positive correlations with sales lower than £6M. 
# For the same product ids, North America tends to generate higher sales compared
# to Europe with the same products. One reason could be that North America is a 
# bigger country with  higher population.
# The product ids have an impact on the sales in all markets. Products with lower
# number ranging from 100 to 1000 generate higher sales in all markets. 
# Product ids 515, 263, 518, 249 and 107  are top products which generate the 
# highest sales. 
# Global trends show product ids with higher number generate more sales but lower.
# They tend to be less than £20M. 

###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

###############################################################################

# Load and explore the data
view(df)

# Drop the other_sales column
sales_df <- subset(df, select = -c(other_sales))

# Determine a summary of the sales data frame.
summary(sales_df)

# View first few rows
head(sales_df)
###############################################################################

# Create a simple linear regression model

# Install GGally for pairplot
install.packages("GGally")
library(GGally)

## Determine the correlation between columns
cor(sales_df)
# Global sales shows very strong positive correlations with Europe sales, 
# North America sales, and very strong negative correlation with product id.

# Pairplot of variables to determine the correlation of the variables
ggpairs(sales_df) + 
  ggtitle("Pairplot of Product, Europe, North America and Global Sales") +
  theme_bw()

## Global sales v Europe sales
# Linear regression model with Europe sales as x and global sales as y
model1 <- lm(global_sales~eu_sales, data=sales_df)

# View the model1 global sale ~ Europe sales
model1
# Coefficient shows global sales will go up by £2.237M with every increment of
# Europe sales

# View full regression table of model1 - global_sales~eu_sales
summary(model1)
# Europe sales is a highly significant value, 
# explaining over 72.01% of the variability of global sales.

# View residuals of model1 on a plot
plot(model1$residuals)
# there is pattern with the residuals around 0 so will perform log transformation

# Create a simple LM plot Global sales v Europe sales
plot(sales_df$eu_sales, sales_df$global_sales)
abline(coefficients(model1))


## Global sales v North America sales
# Linear regression model with NA sales as x and global sales as y
model2 <- lm(global_sales~na_sales, data=sales_df)

# View the model2 global sales ~ North America sales
model2
# Coefficient shows global sales will go up by £1.635M with every increment of
# North America sales

# View full regression table of model1 - global_sales~na_sales
summary(model2)
# North America sales is a highly significant value, 
# explaining over 83.95% of the variability of global sales.

# View residuals of model2 on a plot
plot(model2$residuals)
# there is pattern with the residuals around 0 so will perform log transformation

# Create a simple LM plot
plot(sales_df$na_sales, sales_df$global_sales)
abline(coefficients(model2))


## Global sales v product
# Linear regression model with product as x and global sales as y
model3 <- lm(global_sales~product, data=sales_df)

# View the model3 global sales ~ product
model3
# Coefficient shows global sales will go down by £0.002043M with 
# every increment of product id.

# View full regression table of model3 - global_sales~product
summary(model3)
# Product is a fairly significant value, 
# explaining over 36.74% of the variability of global sales.

# View residuals of model3 on a plot
plot(model3$residuals)
# there is pattern with the residuals around 0 so will perform log transformation


# Create a simple LM plot global sales ~ product
plot(sales_df$product, sales_df$global_sales)
abline(coefficients(model3))

###############################################################################
## Create multiple linear regression models 

# MLR - Global sales ~ Europe Sales and North America Sales
modela = lm(global_sales~eu_sales+na_sales, data=sales_df)

# view modela
modela

# Summary statistics of modela
summary(modela)
# Adjusted R-squared is 0.9664, very high, it means that the model is a good fit 
# with very high correlation. 
# Multiple R-squared of 0.9668 means that 96.68% of the variability observed of 
# the Global sales is explained by Europe Sales and North America sales.


# View residuals of modela on a plot
plot(modela$residuals)
abline(h=0, col="blue")
# positive values for the residual (on the y-axis) mean the prediction was too low,
# and negative values mean the prediction was too high
# 0 means the guess was exactly correct.


# MLR - Global sales ~ product , Europe Sales and North America Sales
modelb = lm(global_sales~product+eu_sales+na_sales, data=sales_df)
# Adjusted R-squared is 0.9709, very high, it means that the model is a good fit 
# with very high correlation. 
# Multiple R-squared of 0.9709 means that 97.09% of the variability observed of 
# the Global sales is explained by Product, Europe Sales and North America sales.

# view modelb
modelb

# Summary statistics of modelb
summary(modelb)

# View residuals of modelb on a plot
plot(modelb$residuals)
abline(h=0, col="blue")
# positive values for the residual (on the y-axis) mean the prediction was too low,
# and negative values mean the prediction was too high
# 0 means the guess was exactly correct.

###############################################################################

# Predict global sales based on provided values using modelb with the
# the highest adjusted R-squared

# Compare your prediction to the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.

## Retrieve observed values to compare against predicted values
a <- subset(sales1, NA_Sales == 34.02 & EU_Sales ==23.80)
b <- subset(sales1, NA_Sales == 3.93 & EU_Sales == 1.56)
c <- subset(sales1, NA_Sales == 2.73 & EU_Sales == 0.65)
d <- subset(sales1, NA_Sales == 2.26 & EU_Sales == 0.97)
e <- subset(sales1, NA_Sales == 22.08 & EU_Sales == 0.52)

observed_data <- rbind(a,b,c,d,e)
view(observed_data)

# prepare the test data
product = observed_data$Product
eu_sales <- c(23.80, 1.56, 0.65, 0.97, 0.52)
na_sales <-  c(34.02, 3.93, 2.73, 2.26, 22.08)

# For modelb global sales ~ product + europe sales + north america sales
newdatab <- data.frame(product, eu_sales, na_sales)

# Predict global sales with modelb
predict2 <- predict(modelb, newdatab, interval='confidence')
set2 <- cbind(observed_data, predict2)
set2

# Plot predicted global sales v observed global sales
ggplot(set2, aes( x=fit, y=Global_Sales, colour=factor(Product))) + 
 geom_point(size = 4) +
  geom_smooth(method = "lm", size = 1.5,
              color = "darkgrey",
              se = FALSE) +
  labs(x = "Predicted Global sales £M",
       y = "Observed Global sales £M",
       title = "Predicted Global sales(£M) vs. Observed Global sales(£M)",
       subtitle = "Grouped by products")

###############################################################################

# Week 6 - Observations and insights

## Both modela and modelb have very high adjusted R-squared of over 0.9 and 
## closer to 1. modelb is slightly higher than modela, and thus a better fit.
## Modelb is chosen to predict the global sales because of the higher adjusted
## R-squared value.
## To further improve the models, it is recommended to perform log transformation 
## on the variables because the data sets have some visible outliers.
## An additional variable Platform could also be included to improve the model.

###############################################################################
###############################################################################




