###################################################
# Econometric Projects, Winter Term 2021/22
# Wealth and Income Inequality Modeling
# Author: Maximilian Hildebrandt
###################################################

# Structure
# 1. Preparatory Steps
#  1.1 Data Collection
#  1.2 Pre-Processing
#  1.3 Descriptive Analysis
# 2. Time Series Model Fitting
#  2.1 Stationarity-Assessment
#  2.2 Lag Selection
#  2.3 Cointegration Test
#  2.4 Model Fitting
#  2.5 Diagnostic Testing
# 3. Interpretative Analyses
#  3.2.Granger Causality
#  3.3 IRFs

# 1. Preparatory Steps
# 1.1 Data Collection
# Install relevant packages
# install.packages("ineq")
# install.packages("vars")
# install.packages("reldist")
# install.packages("svars")
# install.packages("readxl")
# install.packages("MTS")
# install.packages("imputeTS")
# install.packages("tsDyn")
# install.packages("portes")

# Load libraries
library(reldist)
library(tidyverse)
library(haven)
library(tseries)
library(dplyr)
library(vars)
library(forecast)
library(svars)
library(readxl)
library(MTS)
library(imputeTS)
library(urca)
library(tsDyn)
library(parallel)
library(portes)

# Load data of SCF+
setwd("C:/Users/M/Dropbox/05 Statistik-Master/3. Semester/Econometrics Project Seminar/02_Data Analysis/")
data <- read_dta("SCF_plus.dta")
head(data) #print top observations to ensure adequate loading of data

# Load additional macroeconomic data for exogenous variables for VARX model
macroeconomic_data <- read_excel("macroeconomic_data.xlsx")

# 1.2. Pre-Processing
# Choose an imputation
data <- data[data$impnum == "1",]

# Pooling data to follow procedure as outlined in Kuhn et al. (2016) appendix, Table A1
pooled_data <- data %>% 
  mutate(year = factor(yearmerge, levels = c(unique(yearmerge)))) 

# Compute sample size per year to ensure correct pooling procedure
pooled_data[, 2] <- sapply(pooled_data[, 2], as.character) # needs to be transformed into characters, before as.numeric works
pooled_data[, 2] <- sapply(pooled_data[, 2], as.numeric) # turn factors into numerics
sample_size_household_per_year <- pooled_data %>% count(year) # Equal sample sizes as compared to reported values in Kuhn et al. (2016)
sample_size_household_per_year

# Count missing values to check if any missing values require preprocessing
table(is.na(pooled_data$year))
table(is.na(pooled_data$ffanw))
table(is.na(pooled_data$tinc)) # No missing values in income, wealth, and years

# Construct full dataset including missing years and macroeconomic time series

# Count number of negative values
sum(pooled_data$tinc < 0)>0
sum(pooled_data$tinc < 0)
round((sum(pooled_data$tinc < 0)/nrow(pooled_data)*100),2) #0,27% negative values
sum(pooled_data$ffanw < 0)>0
sum(pooled_data$ffanw < 0)
round((sum(pooled_data$ffanw < 0)/nrow(pooled_data)*100),2) #8,18% negative values

# Gini coefficient calculation (due to deviation compared to paper values, the reported Gini index values from the appendix of Kuhn et al., 2016 were used)
#Replace negative values with zero
processed_data <- pooled_data
processed_data$tinc[processed_data$tinc <0] <- 0
sum(processed_data$tinc < 0)
processed_data$ffanw[processed_data$ffanw <0] <- 0
sum(processed_data$ffanw < 0)

# Create Gini coefficient per year
aggregated_data <- processed_data %>%
   group_by(year) %>%
   summarize(gini_income = gini(tinc,hhequiv), gini_wealth = gini(ffanw,hhequiv))

# Compute summary statistics per year for wealth and income
aggregated_data_household <- pooled_data %>%
  group_by(year) %>%
  summarize(median_income = median(tinc), median_wealth = median(ffanw))
aggregated_data <- merge(aggregated_data, aggregated_data_household, by="year")

# Add missing values to the data table to ensure triannular time series
missingrow1 <- list(year=1974, gini_income=NA, gini_wealth=NA, median_income=NA, median_wealth=NA)
missingrow2 <- list(year=1980, gini_income=NA, gini_wealth=NA, median_income=NA, median_wealth=NA)
missingrow3 <- list(year=1986, gini_income=NA, gini_wealth=NA, median_income=NA, median_wealth=NA)
aggregated_data[nrow(aggregated_data) + 1, names(missingrow1)] <- missingrow1
aggregated_data[nrow(aggregated_data) + 1, names(missingrow2)] <- missingrow2
aggregated_data[nrow(aggregated_data) + 1, names(missingrow3)] <- missingrow3

# Order data by year
aggregated_data <- aggregated_data[order(aggregated_data$year),]

# Add gini index from Kuhn et al. (2016) due to unexplainable deviations
aggregated_data$gini_income_paper <- c(44, 43, 46, 44, 44, 44, 43, 43, NA, 42, NA, 46, NA, 53, 49, 52, 52, 54, 53, 55, 54, 56, 58)
aggregated_data$gini_wealth_paper <- c(81, 79, 79, 79, 80, 78, 81, 80, NA, 76, NA, 78, NA, 79, 79, 79, 80, 81, 81, 82, 85, 85, 86)

# Add macroeconomic data to dataset
included_years <- c(1950, 1953, 1956, 1959, 1962, 1965, 1968, 1971, 1974, 1977, 1980, 1983, 1986, 1989, 1992, 1995, 1998, 2001, 2004, 2007, 2010, 2013, 2016)
macroeconomic_data <- macroeconomic_data[macroeconomic_data$year %in% included_years,] # Filter macroeconomic data to the triannual series
total_dataset <- merge(aggregated_data, macroeconomic_data, by="year")

# Impute missing values using linear interpolation
median_inc_ts_withNA <- ts(round(total_dataset[,4]))
median_wealth_ts_withNA <- ts(round(total_dataset[,5]))
g_inc_ts_withNA <- ts(round(total_dataset[,6]))
g_wealth_ts_withNA <- ts(round(total_dataset[,7]))
unemp_ts <- ts(round(total_dataset[,9],2))
gdp_ts <- ts(round(total_dataset[,10],2))
int_rates_ts <- ts(round(total_dataset[,11],2))
gdp_per_capita_ts <- ts(round(total_dataset[,12],2))

median_inc_ts <- na_interpolation(median_inc_ts_withNA) # Replace missing values through linear interpolation
median_wealth_ts <- na_interpolation(median_wealth_ts_withNA) # Replace missing values through linear interpolation
g_inc_ts <- na_interpolation(g_inc_ts_withNA) # Replace missing values through linear interpolation
g_wealth_ts <- na_interpolation(g_wealth_ts_withNA) # Replace missing values through linear interpolation

statsNA(g_inc_ts_withNA) # 3 (13%) missing values
statsNA(g_wealth_ts_withNA) # 3 (13%) missing values
statsNA(g_inc_ts) # all missing values replaced
statsNA(g_wealth_ts) # all missing values replaced

# Basemodel VAR(p) with K = 2 (income & wealth inequality)
dat_bv <- cbind(g_inc_ts, g_wealth_ts)
colnames(dat_bv) <- c("gini_income", "gini_wealth")
dat_bv_diff <- apply(dat_bv, MARGIN = 2, FUN="diff")
head(dat_bv_diff)

# Data for enhanced VAR(p) with K = 4 (income & wealth inequality, GDP, interest rates)
dat_mv <- cbind(g_inc_ts, g_wealth_ts, int_rates_ts, gdp_per_capita_ts)
colnames(dat_mv) <- c("gini_income", "gini_wealth", "interest_rates", "GDP_per_capita")
dat_mv_diff <- apply(dat_mv, MARGIN = 2, FUN="diff")
head(dat_mv_diff)

# Data for VARX K_endo = 3 (income & wealth inequality, interest rates) endogenous variables and K_exo = 1 exogenous variable
y_t = dat_mv_diff[,1:3]
x_t = dat_mv_diff[,4]
head(y_t)
head(x_t)

# 1.3 Descriptive Analysis
# Demographic characteristics of households (based on unprocessed dataset):
mean_age <- mean(processed_data$ageh)
sd_age <- sd(processed_data$ageh)
min_age <- min(processed_data$ageh)
max_age <- max(processed_data$ageh)
mean_adults <- mean(processed_data$adults)
sd_adults <- sd(processed_data$adults)
min_adults <- min(processed_data$adults)
max_adults <- max(processed_data$adults)
mean_children <- mean(processed_data$children)
sd_children <- sd(processed_data$children)
min_children <- min(processed_data$children)
max_children <- max(processed_data$children)
mean_college <- mean(processed_data$collegeh)
sd_college <- NA
min_college <-  NA
max_college <-  NA

# Derive race ratio of households
unique(processed_data$raceh)
prop.table(table(processed_data$raceh))

# Create table of descriptive statistics
tab <- matrix(round(c(mean_age, sd_age, min_age, max_age, mean_adults, sd_adults, min_adults, max_adults, mean_children, sd_children, min_children, max_children, mean_college, sd_college, min_college, max_college),5), ncol = 4, nrow=4, byrow=TRUE)
colnames(tab) <- c("Mean", "SD", "Min", "Max")
rownames(tab) <- c("Household Head Age", "Adults per Household", "Children per Household", "Household Head College Education Share")
tab

# Summary statistics
summary(total_dataset) # Summary stats of endogenous and exogenous variables
summary(median_inc_ts)
M <- mean(median_inc_ts)
SD <- sd(median_inc_ts)
CV <- SD/M*100 # Coefficient of variation
print(SD)
print(CV)

summary(median_wealth_ts)
M <- mean(median_wealth_ts)
SD <- sd(median_wealth_ts)
CV <- SD/M*100 # Coefficient of variation
print(SD)
print(CV)

summary(g_inc_ts)
M <- mean(g_inc_ts)
SD <- sd(g_inc_ts)
CV <- SD/M*100 # Coefficient of variation
print(SD)
print(CV)

summary(g_wealth_ts)
M <- mean(g_wealth_ts)
SD <- sd(g_wealth_ts)
CV <- SD/M*100 # Coefficient of variation
print(SD)
print(CV)

summary(int_rates_ts)
M <- mean(int_rates_ts)
SD <- sd(int_rates_ts)
CV <- SD/M*100 # Coefficient of variation
print(SD)
print(CV)

summary(gdp_per_capita_ts)
  M <- mean(gdp_per_capita_ts)
  SD <- sd(gdp_per_capita_ts)
  CV <- SD/M*100 # Coefficient of variation
  print(SD)
  print(CV)

summary(int_rates_ts)
M <- mean(int_rates_ts)
SD <- sd(int_rates_ts)
CV <- SD/M*100 # Coefficient of variation
print(SD)
print(CV)

# Visual plots of time series
dev.off()
plot.new()
par(mfrow = c(3, 2))
year_labels <- list(1950, 1953, 1956, 1959, 1962, 1965, 1968, 1971, 1974, 1977, 1980, 1983, 1986, 1989, 1992, 1995, 1998, 2001, 2004, 2007, 2010, 2013, 2016)
median_inc_plot <- plot(x=year_labels, y=median_inc_ts, type="o", pch=20, xlab=expression(bold("Year")), ylab=expression(bold("U.S. Dollar")), main=expression(bold("Median U.S. Household Income")), cex.lab=1.8, cex.axis=1.8, cex.main=1.8, cex.sub=1.8)
median_wealth_plot <- plot(x=year_labels, y=median_wealth_ts, type="o", pch=20, xlab=expression(bold("Year")), ylab=expression(bold("U.S. Dollar")), main=expression(bold("Median U.S. Household Wealth")), cex.lab=1.8, cex.axis=1.8, cex.main=1.8, cex.sub=1.8)
gini_income_plot <- plot(x=year_labels, y=g_inc_ts, type="o", pch=20, xlab=expression(bold("Year")), ylab=expression(bold("Gini")), main=expression(bold("Gini Index for U.S. Household Income")), cex.lab=1.8, cex.axis=1.8, cex.main=1.8, cex.sub=1.8)
gini_wealth_plot <- plot(x=year_labels, y=g_wealth_ts, type="o", pch=20, xlab=expression(bold("Year")), ylab=expression(bold("Gini")), main=expression(bold("Gini Index for U.S. Household Wealth")), cex.lab=1.8, cex.axis=1.8, cex.main=1.8, cex.sub=1.8)
int_rates_plot <- plot(x=year_labels, y=int_rates_ts, type="o", pch=20, xlab=expression(bold("Year")), ylab=expression(bold("Percentage")), main=expression(bold("U.S. Interest Rates in %")), cex.lab=1.8, cex.axis=1.8, cex.main=1.8, cex.sub=1.8)
gdp_per_capita_plot <- plot(x=year_labels, y=gdp_per_capita_ts, type="o", pch=20, xlab=expression(bold("Year")), ylab=expression(bold("Dollar")), main=expression(bold("U.S. GDP per Capita")), cex.lab=1.8, cex.axis=1.8, cex.main=1.8, cex.sub=1.8)

# 2. Time Series Modeling
# 2.1 Assessment of Stationarity
# Visual assessment of stationarity using undifferenced, differenced and 2nd order differenced data series
plot.new()
year_labels_diff <- list(1953, 1956, 1959, 1962, 1965, 1968, 1971, 1974, 1977, 1980, 1983, 1986, 1989, 1992, 1995, 1998, 2001, 2004, 2007, 2010, 2013, 2016)
gini_income_diff_plot <- plot(x=year_labels_diff, y=diff(g_inc_ts), type="o", pch=20, xlab="Year", ylab="Differenced Gini Coefficient for U.S. Household Income")
gini_wealth_diff_plot <- plot(x=year_labels_diff, y=diff(g_wealth_ts), type="o", pch=20, xlab="Year", ylab="Differenced Gini Coefficient for U.S. Household Wealth")
unemp_rate_diff_plot <- plot(x=year_labels_diff, y=diff(unemp_ts), type="o", pch=20, xlab="Year", ylab="Differenced U.S. Unemployment Rate in %")
gdp_diff_plot <- plot(x=year_labels_diff, y=diff(gdp_ts), type="o", pch=20, xlab="Year", ylab="Differenced U.S. GDP")
gdp_per_capita_diff_plot <- plot(x=year_labels_diff, y=diff(gdp_per_capita_ts), type="o", pch=20, xlab="Year", ylab="Differenced U.S. GDP per Capita")

# Use autocorrelation function to consider degree of persistence in data
dev.off()
plot.new()
par(mfrow = c(4, 2))
gini_income_acf <- acf(g_inc_ts, plot=TRUE, , cex.lab=1.8, cex.axis=1.8, cex.main=1.8, cex.sub=1.8) # Indicates unit root
gini_wealth_acf <- acf(g_wealth_ts, plot=TRUE, cex.lab=1.8, cex.axis=1.8, cex.main=1.8, cex.sub=1.8) # Indicates unit root
int_rates_acf <- acf(int_rates_ts, plot=TRUE, cex.lab=1.8, cex.axis=1.8, cex.main=1.8, cex.sub=1.8) # Indicates unit root
GDP_per_capita_acf <- acf(gdp_per_capita_ts, plot=TRUE, cex.lab=1.8, cex.axis=1.8, cex.main=1.8, cex.sub=1.8) # Indicates unit root

gini_income_pcf <- acf(g_inc_ts, plot=TRUE, type="partial", cex.lab=1.8, cex.axis=1.8, cex.main=1.8, cex.sub=1.8) # No significant lags after the first one, but 10th lag close
gini_wealth_pcf <- acf(g_wealth_ts, plot=TRUE, type="partial", cex.lab=1.8, cex.axis=1.8, cex.main=1.8, cex.sub=1.8) # No significant lags after the first one
int_rates_pcf <- acf(int_rates_ts, plot=TRUE, type="partial", cex.lab=1.8, cex.axis=1.8, cex.main=1.8, cex.sub=1.8) # No significant lags after the first one, but 10th lag close
GDP_per_capita_pcf <- acf(gdp_per_capita_ts, plot=TRUE, type="partial", cex.lab=1.8, cex.axis=1.8, cex.main=1.8, cex.sub=1.8) # No significant lags after the first one

# Test for stationarity using ADF
# Undifferenced data
adf_test_g_inc_ts_trend = ur.df(g_inc_ts, type = "trend", lags = 5, selectlags = "AIC")
adf_test_g_inc_ts_level = ur.df(g_inc_ts, type = "drift", lags = 5, selectlags = "AIC")
adf_test_g_inc_ts_none = ur.df(g_inc_ts, type = "none", lags = 5, selectlags = "AIC")
adf_test_g_wealth_ts_trend = ur.df(g_wealth_ts, type = "trend", lags = 5, selectlags = "AIC")
adf_test_g_wealth_ts_level = ur.df(g_wealth_ts, type = "drift", lags = 5, selectlags = "AIC")
adf_test_g_wealth_ts_none = ur.df(g_wealth_ts, type = "none", lags = 5, selectlags = "AIC")
adf_test_int_rates_ts_trend = ur.df(int_rates_ts, type = "trend", lags = 5, selectlags = "AIC")
adf_test_int_rates_ts_level = ur.df(int_rates_ts, type = "drift", lags = 5, selectlags = "AIC")
adf_test_int_rates_ts_none = ur.df(int_rates_ts, type = "none", lags = 5, selectlags = "AIC")
adf_test_gdp_ts_trend = ur.df(gdp_ts, type = "trend", lags = 5, selectlags = "AIC")
adf_test_gdp_ts_level = ur.df(gdp_ts, type = "drift", lags = 5, selectlags = "AIC")
adf_test_gdp_ts_none = ur.df(gdp_ts, type = "none", lags = 5, selectlags = "AIC")
adf_test_gdp_per_capita_ts_trend = ur.df(gdp_per_capita_ts, type = "trend", lags = 5, selectlags = "AIC")
adf_test_gdp_per_capita_ts_level = ur.df(gdp_per_capita_ts, type = "drift", lags = 5, selectlags = "AIC")
adf_test_gdp_per_capita_ts_none = ur.df(gdp_per_capita_ts, type = "none", lags = 5, selectlags = "AIC")

summary(adf_test_g_inc_ts_trend) 
summary(adf_test_g_inc_ts_level) 
summary(adf_test_g_inc_ts_none) 
summary(adf_test_g_wealth_ts_trend) 
summary(adf_test_g_wealth_ts_level) 
summary(adf_test_g_wealth_ts_none) 
summary(adf_test_int_rates_ts_trend) 
summary(adf_test_int_rates_ts_level) 
summary(adf_test_int_rates_ts_none) 
summary(adf_test_gdp_ts_trend) 
summary(adf_test_gdp_ts_level) 
summary(adf_test_gdp_ts_none) 
summary(adf_test_gdp_per_capita_ts_trend) 
summary(adf_test_gdp_per_capita_ts_level) 
summary(adf_test_gdp_per_capita_ts_none) 

# differenced data
adf_test_g_inc_diff_ts_trend = ur.df(diff(g_inc_ts), type = "trend", lags = 5, selectlags = "AIC")
adf_test_g_inc_diff_ts_level = ur.df(diff(g_inc_ts), type = "drift", lags = 5, selectlags = "AIC")
adf_test_g_inc_diff_ts_none = ur.df(diff(g_inc_ts), type = "none", lags = 5, selectlags = "AIC")
adf_test_g_wealth_diff_ts_trend = ur.df(diff(g_wealth_ts), type = "trend", lags = 5, selectlags = "AIC")
adf_test_g_wealth_diff_ts_level = ur.df(diff(g_wealth_ts), type = "drift", lags = 5, selectlags = "AIC")
adf_test_g_wealth_diff_ts_none = ur.df(diff(g_wealth_ts), type = "none", lags = 5, selectlags = "AIC")
adf_test_int_rates_diff_ts_trend = ur.df(diff(int_rates_ts), type = "trend", lags = 5, selectlags = "AIC")
adf_test_int_rates_diff_ts_level = ur.df(diff(int_rates_ts), type = "drift", lags = 5, selectlags = "AIC")
adf_test_int_rates_diff_ts_none = ur.df(diff(int_rates_ts), type = "none", lags = 5, selectlags = "AIC")
adf_test_gdp_diff_ts_trend = ur.df(diff(gdp_ts), type = "trend", lags = 5, selectlags = "AIC")
adf_test_gdp_diff_ts_level = ur.df(diff(gdp_ts), type = "drift", lags = 5, selectlags = "AIC")
adf_test_gdp_diff_ts_none = ur.df(diff(gdp_ts), type = "none", lags = 5, selectlags = "AIC")
adf_test_gdp_per_capita_diff_ts_trend = ur.df(diff(gdp_per_capita_ts), type = "trend", lags = 5, selectlags = "AIC")
adf_test_gdp_per_capita_diff_ts_level = ur.df(diff(gdp_per_capita_ts), type = "drift", lags = 5, selectlags = "AIC")
adf_test_gdp_per_capita_diff_ts_none = ur.df(diff(gdp_per_capita_ts), type = "none", lags = 5, selectlags = "AIC")

summary(adf_test_g_inc_diff_ts_trend) 
summary(adf_test_g_inc_diff_ts_level) 
summary(adf_test_g_inc_diff_ts_none) 
summary(adf_test_g_wealth_diff_ts_trend) 
summary(adf_test_g_wealth_diff_ts_level) 
summary(adf_test_g_wealth_diff_ts_none) 
summary(adf_test_int_rates_diff_ts_trend) 
summary(adf_test_int_rates_diff_ts_level) 
summary(adf_test_int_rates_diff_ts_none) 
summary(adf_test_gdp_diff_ts_trend) 
summary(adf_test_gdp_diff_ts_level) 
summary(adf_test_gdp_diff_ts_none) 
summary(adf_test_gdp_per_capita_diff_ts_trend) 
summary(adf_test_gdp_per_capita_diff_ts_level) 
summary(adf_test_gdp_per_capita_diff_ts_none) 

# 2.2 Lag Selection
# Determine optimal lags through sequential testing
options(scipen=999)
VARselect(dat_bv_diff, lag.max=6, type="both") # p = 6 for bivariate model
VARselect(dat_mv_diff, lag.max=4, type="both") # AIC: p = 3, SC: 3
VARX_order <-VARXorder(x=y_t, exog=x_t, maxp = 3, maxm=3) # AIC: (p, s) = (3, 3), SC: (0, 2)

# 2.3 Cointegration Test: 
# Assess if pairs of time series have a shared stochastic trend
mean(diff(g_inc_ts)) #unequal to zero
mean(diff(g_wealth_ts)) #unequal to zero
mean(diff(gdp_ts)) # unequal to zero
mean(diff(int_rates_ts)) #unequal to zero
cointegration_test_trend_bv <- ca.jo(dat_bv_diff, ecdet = "trend", type="eigen", K=6, spec="longrun")
cointegration_test_const_bv <- ca.jo(dat_bv_diff, ecdet = "const", type="eigen", K=6, spec="longrun")
cointegration_test_none_bv <- ca.jo(dat_bv_diff, ecdet = "none", type="eigen", K=6, spec="longrun")

print(cbind(capture.output(summary(cointegration_test_trend_bv))[11:18])) #cointegration rank 2, equal to K
print(cbind(capture.output(summary(cointegration_test_const_bv))[11:18])) #cointegration rank 2
print(cbind(capture.output(summary(cointegration_test_none_bv))[11:18])) #cointegration rank 2

cointegration_test_trend_mv <- ca.jo(dat_mv_diff, ecdet = "trend", type="eigen", K=3, spec="longrun")
cointegration_test_const_mv <- ca.jo(dat_mv_diff, ecdet = "const", type="eigen", K=3, spec="longrun")
cointegration_test_none_mv <- ca.jo(dat_mv_diff, ecdet = "none", type="eigen", K=3, spec="longrun")

print(cbind(capture.output(summary(cointegration_test_trend_mv))[11:18])) #cointegration rank 2
print(cbind(capture.output(summary(cointegration_test_const_mv))[11:18])) #cointegration rank 2
print(cbind(capture.output(summary(cointegration_test_none_mv))[11:18])) #cointegration rank 2

# 2.4 Model Fitting: 

#Bivariate model with K = 2 dimensions
VAR_bv <- vars::VAR(dat_bv_diff, p = 6, type = "both")
summary(VAR_bv)
VAR_bv$varresult
VAR_bv_restr <- restrict(VAR_bv, method = "ser", thresh = 2)
summary(VAR_bv_restr)
VAR_bv_restr$varresult

# Multivariate model with K = 3 dimensions (for comparison of residual matrices of VARX model)
VAR_mv <- vars::VAR(dat_mv_diff[,1:3], p = 3, type = "trend") # shifted to trend since polynomial roots were very close to 1, with trend they are lower
summary(VAR_mv)
VAR_mv$varresult
VAR_mv_restr <- restrict(VAR_mv, method = "ser", thresh = 1.6) # threshold above 1.6 leads to error since no significant regressors for gini wealth remain
summary(VAR_mv_restr)

# VARX model
x_t_small <- x_t/1000 # reduce scaling of GDP per capita to make coefficients readable
VARX_mv <- VARX(zt=y_t,3,xt=x_t_small,2)
VARX_mv_restr <- refVARX(VARX_mv,thres=2)

# 2.5 Diagnostic Testing
# for baseline VAR model
# Multivariate tests
VAR_bv_resid <- residuals(VAR_bv_restr)#
VAR_bv_resid
serial.test(VAR_bv, type = "BG") # Breusch-Godfrey test for serial correlation, p below 5% would indicate presence of autocorrelation
normality.test(VAR_bv, multivariate.only=TRUE) # Test multivariate normality, p above 5% would indicate normal distribution
arch.test(VAR_bv) # multivariate ARCH Lagrange-Multiplier test to assess heteroskedasticity, p above 5% would indicate absence of heteroskedascicity
roots(VAR_bv_restr) # Stability test: the moduli of the eigenvalues of the companion matrix are below 1, indicating stability

# Univariate Plots for residuals of gini income
gini_income_e <- residuals(VAR_bv_restr)[,1] # Store residuals for custom plot
gini_wealth_e <- residuals(VAR_bv_restr)[,2] # Store residuals
acf_income <- Acf(gini_income_e, 12)
acf_wealth <- Acf(gini_wealth_e, 12)

dev.off()
plot.new()
par(mfrow = c(4, 2))
plot(gini_income_e, type="l", ylab=expression(bold("Residuals")), xlab = expression(bold("Observation")), main = "Residual Plot for Income Inequality (Differenced)", cex.lab=1.8, cex.axis=1.8, cex.main=1.8, cex.sub=1.8) # Generate set of diagnostic plots
plot(acf_income, ylab=expression(bold("ACF")), xlab=expression(bold("Lag")), main = "", cex.lab=1.8, cex.axis=1.8, cex.main=1.8, cex.sub=1.8) 
title(main="Residual ACF Plot for Income Inequality (Differenced)", cex.main=1.8)
plot (gini_income_e^2, type="l", ylab=expression(bold("Residuals^2")), xlab = expression(bold("Observation")), main="Residual Plot for Income Inequality (Differenced)", , cex.lab=1.8, cex.axis=1.8, cex.main=1.8, cex.sub=1.8) # Squared residual plot for assessment of heteroskedasticity
hist(gini_income_e, ylab=expression(bold("Frequency")), xlab=expression(bold("Residual")), main="Histogram for Income Inequality (Differenced)", , cex.lab=1.8, cex.axis=1.8, cex.main=1.8, cex.sub=1.8) # Histogram to assess normality of residuals (easier to read than the plot() output)
plot(gini_wealth_e, type="l", ylab=expression(bold("Residuals")), xlab = expression(bold("Observation")), main = "Residual Plot for Wealth Inequality (Differenced)", cex.lab=1.8, cex.axis=1.8, cex.main=1.8, cex.sub=1.8) # Generate set of diagnostic plots
plot(acf_wealth, ylab=expression(bold("ACF")), xlab=expression(bold("Lag")), main = "", cex.lab=1.8, cex.axis=1.8, cex.main=1.8, cex.sub=1.8) 
title(main="Residual ACF Plot for Wealth Inequality (Differenced)", cex.main=1.8)
plot (gini_wealth_e^2, type="l", ylab=expression(bold("Residuals^2")), xlab = expression(bold("Observation")), main="Residual Plot for Wealth Inequality (Differenced)", , cex.lab=1.8, cex.axis=1.8, cex.main=1.8, cex.sub=1.8) # Squared residual plot for assessment of heteroskedasticity
hist(gini_wealth_e, ylab=expression(bold("Frequency")), xlab=expression(bold("Residual")), main="Histogram for Wealth Inequality (Differenced)", , cex.lab=1.8, cex.axis=1.8, cex.main=1.8, cex.sub=1.8) # Histogram to assess normality of residuals (easier to read than the plot() output)

# Enhanced VARX model residual checks
roots(VARX_mv_restr)
VARX_mv_restr_resid <- VARX_mv_restr$residuals
varx_mv_model_arch <- MarchTest(VARX_mv_restr_resid, lag = 3) # multivariate ARCH Lagrange-Multiplier test to assess heteroskedasticity, p above 5% would indicate absence of heteroskedascicity
varx_mv_model_arch
varx_mv_model_norm1 <- jarque.bera.test(VARX_mv_restr_resid[,1]) # Test multivariate normality, p above 5% would indicate normal distribution
varx_mv_model_norm1 # no deviance from multivariate normal distribution
varx_mv_model_norm2 <- jarque.bera.test(VARX_mv_restr_resid[,2]) # Test multivariate normality, p above 5% would indicate normal distribution
varx_mv_model_norm2 # no deviance from multivariate normal distribution
varx_mv_model_norm3 <- jarque.bera.test(VARX_mv_restr_resid[,3]) # Test multivariate normality, p above 5% would indicate normal distribution
varx_mv_model_norm3 # no deviance from multivariate normal distribution
varx_mv_model <- Hosking(VARX_mv_restr$residuals, lags=12) # autocorrelation test
varx_mv_model

# Visual residual checks for VARX model
gini_income_e <- VARX_mv_restr_resid[,1] # Store residuals for custom plot
gini_wealth_e <- VARX_mv_restr_resid[,2] 
int_rates_e <- VARX_mv_restr_resid[,3]
acf_income <- Acf(gini_income_e, 12)
acf_wealth <- Acf(gini_wealth_e, 12)
acf_int_rates <- Acf(int_rates_e, 12)

dev.off()
plot.new()
par(mfrow = c(6, 2))
plot(gini_income_e, type="l", ylab=expression(bold("Residuals")), xlab = expression(bold("Observation")), main = "Residual Plot for Income Inequality (Differenced)", cex.lab=1.8, cex.axis=1.8, cex.main=1.8, cex.sub=1.8) # Generate set of diagnostic plots
plot(acf_income, ylab=expression(bold("ACF")), xlab=expression(bold("Lag")), main = "", cex.lab=1.8, cex.axis=1.8, cex.main=1.8, cex.sub=1.8)
title(main="Residual ACF Plot for Income Inequality (Differenced)", cex.main=1.8)
plot (gini_income_e^2, type="l", ylab=expression(bold("Residuals^2")), xlab = expression(bold("Observation")), main="Residual Plot for Income Inequality (Differenced)", , cex.lab=1.8, cex.axis=1.8, cex.main=1.8, cex.sub=1.8) # Squared residual plot for assessment of heteroskedasticity
hist(gini_income_e, ylab=expression(bold("Frequency")), xlab=expression(bold("Residual")), main="Histogram for Income Inequality (Differenced)", , cex.lab=1.8, cex.axis=1.8, cex.main=1.8, cex.sub=1.8) # Histogram to assess normality of residuals (easier to read than the plot() output)
plot(gini_wealth_e, type="l", ylab=expression(bold("Residuals")), xlab = expression(bold("Observation")), main = "Residual Plot for Wealth Inequality (Differenced)", cex.lab=1.8, cex.axis=1.8, cex.main=1.8, cex.sub=1.8) # Generate set of diagnostic plots
plot(acf_wealth, ylab=expression(bold("ACF")), xlab=expression(bold("Lag")), main = "", cex.lab=1.8, cex.axis=1.8, cex.main=1.8, cex.sub=1.8)
title(main="Residual ACF Plot for Wealth Inequality (Differenced)", cex.main=1.8)
plot (gini_wealth_e^2, type="l", ylab=expression(bold("Residuals^2")), xlab = expression(bold("Observation")), main="Residual Plot for Wealth Inequality (Differenced)", , cex.lab=1.8, cex.axis=1.8, cex.main=1.8, cex.sub=1.8) # Squared residual plot for assessment of heteroskedasticity
hist(gini_wealth_e, ylab=expression(bold("Frequency")), xlab=expression(bold("Residual")), main="Histogram for Wealth Inequality (Differenced)", , cex.lab=1.8, cex.axis=1.8, cex.main=1.8, cex.sub=1.8) # Histogram to assess normality of residuals (easier to read than the plot() output)
plot(int_rates_e, type="l", ylab=expression(bold("Residuals")), xlab = expression(bold("Observation")), main = "Residual Plot for Interest Rates (Differenced)", cex.lab=1.8, cex.axis=1.8, cex.main=1.8, cex.sub=1.8) # Generate set of diagnostic plots
plot(acf_int_rates, ylab=expression(bold("ACF")), xlab=expression(bold("Lag")), main = "", cex.lab=1.8, cex.axis=1.8, cex.main=1.8, cex.sub=1.8)
title(main="Residual ACF Plot for Wealth Inequality (Differenced)", cex.main=1.8)
plot (int_rates_e^2, type="l", ylab=expression(bold("Residuals^2")), xlab = expression(bold("Observation")), main="Residual Plot for Interest Rates (Differenced)", , cex.lab=1.8, cex.axis=1.8, cex.main=1.8, cex.sub=1.8) # Squared residual plot for assessment of heteroskedasticity
hist(int_rates_e, ylab=expression(bold("Frequency")), xlab=expression(bold("Residual")), main="Histogram for Interest Rates (Differenced)", , cex.lab=1.8, cex.axis=1.8, cex.main=1.8, cex.sub=1.8) # Histogram to assess normality of residuals (easier to read than the plot() output)

# 3. Interpretative Analyses:
# 3.1 Granger Causality
# test null hypothesis of no Granger causality in both directions
library(lmtest)
causality(VAR_bv_restr, cause ="gini_income") # no granger causality
causality(VAR_bv_restr, cause ="gini_wealth") # no granger causality


# 3.2. Impulse Response Analysis
# Impulse Response Function
irf1 <- irf(VAR_bv, impulse = "gini_income", response = "gini_wealth")
irf2 <- irf(VAR_bv, impulse = "gini_wealth", response = "gini_income")
plot(irf1, xlab = "Observation Time", ylab = "Response", main = expression(bold("Response of Wealth Inequality by Income Inequality")), cex.lab=1.8, cex.axis=1.8, cex.main=1.8, cex.sub=1.8)
plot(irf2, xlab = "Observation Time", ylab = "Response", main =  expression(bold("Response of Income Inequality by Wealth Inequality")), cex.lab=1.8, cex.axis=1.8, cex.main=1.8, cex.sub=1.8)
