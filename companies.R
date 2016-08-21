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
df <- read.csv("companies.csv", header = TRUE)
# Print general data
head(df)
# Some nice colors for plots
c1 <- '#4289db'
c2 <- '#363636'
# Plot Prelim Data
funding <- as.numeric(df[,5])
ggplot() + aes(funding) + geom_density(fill=c1, color=c1) + labs(title='Funding Density Graph')
country <- df[,7]
ggplot() + aes(country) + geom_density(fill=c1, color=c1) + coord_flip() + labs(title='Country Density Graph')
operating <- df[,6]
ggplot() + aes(operating) + geom_bar(fill=c1, color=c1) + labs(title='Company Status Bar Plot')
