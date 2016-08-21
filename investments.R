# Install packages
install.packages('dplyr')
install.packages('ggplot2')
install.packages('kknn')
install.packages('data.table')
install.packages('kernlab')
install.packages('e1071')
# Load libraries
library(dplyr)
library(ggplot2)
library(kknn)
library(data.table)
library(kernlab)
library(e1071)
# Set WD
setwd('~/projects/crunchbase.study/')
df <- read.csv("investments.csv", header = TRUE)
# Print general data
head(df)
# Some nice colors for plots
c1 <- '#4289db'
c2 <- '#363636'
# Filter only Mountain View companies
df[,'funded_at'] <- as.Date(df[,'funded_at'], '%Y-%m-%d')
df <- filter(df, company_city == 'Mountain View', funded_at > '2015-01-01')
df <- df[with(df, order(funded_at)), ]
df <- na.omit(df)
head(df)
# Plot Prelim Data
companies <- df[,2]
ggplot() + aes(companies) + geom_bar(fill=c1, color=c1) + coord_flip() + labs(title='# of investments in each Company Plot in Mountain View') + ggsave(file = "num_investments_mv.png")
funding_round <- df[,16]
ggplot() + aes(funding_round) + geom_bar(fill=c1, color=c1) + coord_flip() + labs(title='Amount of investments by Round in Mountain View') + ggsave(file = "amount_USD_investments_mv.png")
country <- df[,10]
ggplot() + aes(country) + geom_bar(fill=c1, color=c1) + coord_flip() + labs(title='Where the investors are located by Country') + ggsave(file = "investors_by_country.png")
amounts <- as.numeric(df[,18])
ggplot() + aes(amounts) + geom_density(fill=c1, color=c1) + labs(title='Amounts Raised') + ggsave(file = "amounts_raised_density.png")
# Model the data
# Make ranges
df_len <- length(df[,1])
min <- 1
max <- (df_len / 2) + 1
train_min <- (df_len / 2) + 2
train_max <- df_len
# Prepare Data  
X <- df[,17][min:train_max]
Y <- df[,18][min:train_max]
# Create Model
model <- svm(Y ~ X, df, kernel="radial", cost=100, gamma=10)
pred <- predict(model)
# Plot Data
ggplot() + geom_point(aes(color='Original Data', x = X, y = Y)) + geom_point(aes(color = 'Predicted Data', x = X, y = pred)) + geom_smooth(aes(x = X, y = pred)) + labs(title='Support Vector Machine of Amounts Raised') + ggsave(file = "SVM_Fitted.png")
# Get RMSE
rmse <- function(error) {
  sqrt(mean(error^2))
}
error <- rmse(Y - pred)
print(error)
# Predict Future
future_dates <- X[1]
future_dates <- seq.Date( future_dates, length=length(X) * 2, by='1 day' )
print(last_date)
# Plot Data
ggplot() + geom_point(aes(color = 'Predicted Data', x = future_dates, y = c(Y, pred) )) + geom_smooth(aes(x = future_dates, y = c(Y, pred)  )) + labs(title='SVM Forecasts') + ggsave(file = "SVM_Forceast.png")
