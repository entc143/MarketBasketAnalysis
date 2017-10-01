####### Association Rule ##########
library(arules)
####### To convert data into TRANSACTION Type ##########
tdata <- read.transactions("transactions_data.txt", sep="\t") 
tData <- as (myDataFrame, "transactions") # convert dataframes into 'transactions' class
frequentItems <- eclat (Groceries, parameter = list(supp = 0.07, maxlen = 15)) ## Most frequent Data
inspect(frequentItems)
itemFrequencyPlot(Groceries, topN=10, type="absolute", main="Item Frequency") # plot frequent items
rules <- apriori (Groceries, parameter = list(supp = 0.001, conf = 0.5))  ### Product Recommendation Rules
rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
rules_lift <- sort (rules, by="lift", decreasing=TRUE) # 'high-lift' rules.
subsetRules <- which(colSums(is.subset(rules, rules)) > 1) # get subset rules in vector
length(subsetRules)  #> length of users
rules <- rules[-subsetRules] # remove subset rules.
rules <- apriori (data=Groceries, parameter=list (supp=0.001,conf = 0.08), appearance = list (default="lhs",rhs="whole milk"), control = list (verbose=F)) # get rules that lead to buying 'whole milk'
rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf))

rules <- apriori (data=Groceries, parameter=list (supp=0.001,conf = 0.15,minlen=2), appearance = list(default="rhs",lhs="whole milk"), control = list (verbose=F)) # those who bought 'milk' also bought..
rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf))

