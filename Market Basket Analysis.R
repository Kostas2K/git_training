######################
####MACHINE LEARNING BY EXAMPLE - CH.3
######################
data=read.csv("~/Documents/LIBRARY/DATA SCIENCE/PACKT BOOKS/
              R Machine Learning by Example/Code - R Machine Learning by Example/Code/
              Chapter 3 code files/top_supermarket_transactions.csv")

#Assign row names same to column names
row.names(data)=data[[1]]
data=subset(data,select=c(-1))

##Analyzing and visualizing data, p.92
sort(data['milk',],decreasing=TRUE) #Finding the top products with milk
sort(data['bread',],decreasing=TRUE)

mosaicplot(as.matrix(data),color=TRUE,las=5)

#Global recommendations, p.94
cat("Recommendations based on global products contingency matrix")
items <- names(data)
for (item in items){
  cat(paste("Top 2 recommended items to buy with", item, "are: "))
  item.data <- subset(data[item,], select=names(data)[!names(data) %in%
                                                        item])
  cat(names(item.data[order(item.data, decreasing = TRUE)][c(1,2)]))
  cat("\n")
}

  
#Advanced contingency matrices - p.95
install.packages("arules")
library(arules)
data(Groceries)
inspect(Groceries[1:3]) #inspecting the first 3 transactions
ct=crossTable(Groceries)
,measure="count",sort=TRUE)
ct[1:5,1:5]
ct=crossTable(Groceries,measure="support",sort=TRUE) #not working
ct=crossTable(Groceries,measure="lift",sort=TRUE) #not working

##Frequent itemset generation, p.97
library(dplyr)
library(gridExtra)
list.append <- function (mylist, ...){
  mylist <- c(mylist, list(...))
  return(mylist)
}

##Data Retrieval and transformation

# Step 1: Function to read the dataset into memory from file
get_transaction_dataset <- function(filename){
  df <- read.csv(filename, header = FALSE)
  dataset <- list()
  for (index in seq(nrow(df))){
    transaction.set <- as.vector(unlist(df[index,]))
    transaction.set <- transaction.set[transaction.set != ""]
    dataset <- list.append(dataset, transaction.set)
  }
  return(dataset)
}

#Step 2: Function to convert dataset into a data frame
get_item_freq_table <- function(dataset){
  item.freq.table <- unlist(dataset) %>% table %>% data.frame
  return (item.freq.table)
}
#Step 3: Function to prune items based on minimum frequency as specified by the user.
#Here min freq <- item.min.freq
prune_item_freq_table <- function(item.freq.table, item.min.freq){
  pruned.item.table <- item.freq.table[item.freq.table$Freq >=
                                         item.min.freq,]
  return (pruned.item.table)
}

#Building an association matrix, p.99 -STOPPED HERE







