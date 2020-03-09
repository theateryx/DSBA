setwd("C:/x")
library(lubridate)
library(tidyverse)
library(readxl)
library(arulez)
library(dplyr)
library(xlsx)
library(arulezviz)
library(arules)
library(lattice)

data <- read_xlsx("sundata.xlsx")
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
summary(sun)

# split txn
sun.agg <- split(sun$Menu, sun$Transaction)
head(sun.agg)

class(sun.agg)
sun.agg

# remove dupes in each txn
sun.agg1 <- list()
 for (i in 1:length(sun.agg)) {
   sun.agg1[[i]] <- as.character(sun.agg[[i]][!duplicated(sun.agg[[i]])])
 }

Txns <- as(sun.agg1, "Menu1")

summary(Txns)
