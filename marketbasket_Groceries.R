install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)

data("Groceries")

class(Groceries)

dt <- Groceries
inspect(dt)

itemFrequencyPlot(dt, topN=10)

dt_rules <- apriori(dt,parameter = list(supp=0.01,conf=0.8))

#Fine tuning since 0 rules
dt_rules <- apriori(dt,parameter = list(supp=0.001,conf=0.8,minlen=2,maxlen=4))

#Inspect top 5 rules
inspect(dt_rules[1:5])

##sorted rules
rules <- sort(dt_rules, by="support", decreasing=T)

inspect(dt_rules[1:6])

##Finding Duplicate rules
Dup_rules <- is.redundant(rules)


summary(Dup_rules)
dt_rules <- dt_rules[!Dup_rules]
dt_rules
inspect(dt_rules[1:5])

##default item to lhs
dt_rules_milk <- apriori(dt,parameter = list(supp=0.001,conf=0.1),
                         appearance=list(default="rhs",lhs="whole milk"))

inspect(dt_rules_milk[1:20])

##plot
plot(dt_rules,method="graph")

##Interactive
plot(dt_rules,method="graph", interactive = T)
