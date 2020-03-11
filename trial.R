setwd("C:/Users/Ping/Documents/GitHub/dsba")
library(lubridate)
library(tidyverse)
library(readxl)
library(dplyr)
library(arulesViz)
library(arules)
library(lattice)
library(DataExplorer)

data <- read_xlsx("sole.xlsx")
nrow(data)
str(data)
summary(data)

data$Transaction <- as.factor(data$Transaction)

?split
Agg.RTxn <- split(data$Menu,data$Transaction)
class(Agg.RTxn)
Agg.RTxn
## To see specific row number transaction
Agg.RTxn [1:5]


## logic to remove duplicate items from the list
Agg.RTxn_DD <- list()
for (i in 1:length(Agg.RTxn)) {
  Agg.RTxn_DD[[i]] <- as.character(Agg.RTxn[[i]][!duplicated(Agg.RTxn[[i]])])
}
## converting transaction items from list format to transaction format
Txns <- as(Agg.RTxn_DD, "transactions")

summary(Txns)
inspect(Txns[1:5])

freq <- itemFrequency(Txns)
freq <- freq[order(-freq)]
freq["Chawanmushi"]
qplot(freq)

barplot(freq[1:10])

?itemFrequencyPlot

itemFrequencyPlot(Txns, support = 0.03)
itemFrequencyPlot(Txns, topN = 10)

library("arulesViz")

?apriori

arules1 <- apriori(data = Txns, parameter=list(supp= 0.001, conf = 0.05, minlen = 2))
summary(arules1)
inspect(arules1)
inspect(sort(arules1,by="lift"))


arules2 <- apriori(data = Txns, parameter = list(support = 0.003, confidence = 0.1))
inspect(arules2)


library(RColorBrewer)
plot ( arules2,control=list(
  col = brewer.pal(11,"Spectral")
),
main="Association Rules Plot"
)


subrules2 <- head(sort(arules2, by="support"), 20)
plot(subrules2, method="grouped" , engine = "interactive" )

subrules2 <- head(sort(arules2, by="support"), 10)
plot(subrules2, method = "graph" )

subrules2 <- head(sort(arules2, by="confidence"), 20)
plot(subrules2, method = "graph" )

rules_df <- as(arules2,"data.frame")
rules_df$lhs_support <- rules_df$support/ rules_df$confidence;
rules_df$rhs_support <- rules_df$confidence / rules_df$lift;
View(rules_df)
write.table(rules_df, file = "~/new.csv", sep = "," , append = F, row.names = F)
unlink("new.csv")
