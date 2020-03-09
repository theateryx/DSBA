setwd("C:/Users/Ping/Documents/GitHub/dsba")
library(lubridate)
library(tidyverse)
library(readxl)
library(dplyr)
library(arulesViz)
library(arules)
library(lattice)
library(DataExplorer)


data <- read_xlsx("Nov.xlsx")
View(data)
str(data)
summary(data)

data$Date <- ymd(data$Date)
data$Day <- wday(data$Date, label = T)

# Get column names
colnames(data)
data1 <- data[, c(1,13,2,3,4,5,6,7,8,9,10,11,12)]
data1

# Set factors
cols <- c("Date","Transaction","CategoryID","Category", "ClassID","Class", "ID","Menu","Day")
sun <-data1 %>% mutate_at(cols, factor)
head(sun)
sun$Day <- as.factor(sun$Day)

# check NA
anyNA(sun)
colSums(is.na(sun))

summary(sun)

# Subset out free items



# EDA ---------------------------------------------------------------------

#univariate
attach(sun)
plot(Date)
plot(Day)
plot(Transaction)
plot(CategoryID)
plot(Category)
plot(ClassID)
plot(ID)
plot(Menu)
plot(Qty)
boxplot(Price)
qplot(CostAmt)
plot(CostPC)
plot_histogram(sun, title ="Univariate Analysis for Continuous Data")
plot_bar(sun, title ="Univariate Analysis for Factored Data")



which.max(Price)

# datamining --------------------------------------------------------------

# split txn
sun.agg <- split(sun$Menu, sun$Transaction)
head(sun.agg)

# remove dupes in each txn
sun.agg1 <- list()
 for (i in 1:length(sun.agg)) {
   sun.agg1[[i]] <- as.character(sun.agg[[i]][!duplicated(sun.agg[[i]])])
 }


??arules
