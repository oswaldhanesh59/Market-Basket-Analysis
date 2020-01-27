install.packages("tidyverse")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("arules")
install.packages("arulesViz")
install.packages("dplyr")

library(tidyverse)
library(ggplot2)
library(lubridate)
library(arules)
library(arulesViz)
library(dplyr)

transactions_data <- read.csv(file.choose(),header=T)


#check if there are no missing value

transactions_data <- transactions_data[complete.cases(transactions_data), ]

#structure of the data

str(transactions_data)

#sample data
head(transactions_data)

#names
names(transactions_data)

#summary
summary(transactions_data)

#Unique customers,transactions, items and countries 

n_distinct(transactions_data$product_id)
n_distinct(transactions_data$Order.ID)

n_distinct(transactions_data$product_name)

# sorting the data to get sense of all the transactions of the customer
transactions_data_sorted <- transactions_data[order(transactions_data$Order.ID),]

View(transactions_data_sorted)

#restructuring to get the data in required format using ddply function in dplyr

colnames(transactions_data_sorted)

library(plyr)
library(dplyr)
itemList <- ddply(transactions_data_sorted,c("Order.ID"), 
                       function(df1)paste(df1$product_name, 
                       collapse = ","))


head(itemList)


#removing volumns
itemList$Order.ID <- NULL
colnames(itemList) <- c("items")


# checking the final data
str(itemList)
View(itemList)
names(itemList)


setwd("C:/Users/OSIS/Desktop/Market Basket")
write.csv(itemList,"market_basketdata.csv", quote = FALSE, row.names = FALSE)


# Importing the comma separated values. As these values are comma seperated they can be imported in the transaction format. Please ignore the warning (they are because of #default import options of read.transactions)

setwd("C:/Users/OSIS/Desktop/Market Basket")
tran_data_modified <- read.transactions('market_basketdata.csv', format = 'basket', sep=',')

#checking the type of data 
class(tran_data_modified) 


#Item frequency plot to check the top items

itemFrequencyPlot(tran_data_modified,topN=10)

```

#Step 4- Creating rules. We will use the apriori function in the arulez package

```{r}

# For the apriori function there are default values for support, confidence and lenght. It is advisable to provide your own cutoffs and start with large values as it is a #memory consuming operation

rules <- apriori(tran_data_modified, parameter = list(supp=0.0001, conf=0.8))

# getting info about all the rules generated

summary(rules)


# sorting rules by a parameter- lift
rules <- sort(rules, by='lift', decreasing = TRUE)

# Viewing the sorted rules
inspect(rules[1:20])

##Finding Duplicate rules
Dup_rules <- is.redundant(rules)

summary(Dup_rules)
rules <- rules[!Dup_rules]
rules


plotting=rules[1:5]

plot(plotting,method="graph", engine ='interactive')

df=data.frame(
  lhs=labels(lhs(rules)),
  rhs=labels(rhs(rules)),
  rules@quality
)

write.csv(df,"output_data.csv",row.names = FALSE)
