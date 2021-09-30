
#-----------------------------------------------

#DATA SET READ IN:
#DATA SET CONTAINS PAIRS OF ORDER AND PRODUCTS
#data source: https://www.kaggle.com/c/instacart-market-basket-analysis/data
#-----------------------------------------------
setwd("~/R_Projects/Network_Analysis")
data <-read.csv('order_products__prior.csv')
head(data, 10)

library('arules')

transactions.obj <- read.transactions(file = 'order_products__prior.csv',
                                      header = TRUE,
                                      format = "single",
                                      sep = "," ,
                                      cols = c("order_id", "product_id"),
                                      rm.duplicates = FALSE,
                                      quote = "",
                                      skip=0,
                                      encoding = "unknown")
transactions.obj

#using apriori to find the frequent itemsets
support <- 0.015
parameters = list(
  support =support,
  minlen =2, 
  maxlen =2,#need only product pairs
  target= "frequent itemsets"
)

freq.items <- apriori(transactions.obj, parameter = parameters)

#put the items sets into a dataframe

freq.items.df <- data.frame(item_set = labels(freq.items),
                            support = freq.items@quality)
freq.items.df$item_set <-as.character(freq.items.df$item_set)

#get the product pairs out as individual columns
library(tidyr)
freq.items.df <- separate(data = freq.items.df,
                          col = item_set,
                          into = c("item.1", "item.2"),
                          sep = ",")
#freq.items.df

freq.items.df[] <-lapply(freq.items.df, 
                         gsub,
                         pattern ='\\{',
                         replacement='')
freq.items.df[] <-lapply(freq.items.df, 
                         gsub,
                         pattern ='\\}',
                         replacement='')
#freq.items.df

#get the final data for creating the network
network.data <- freq.items.df[, c('item.1','item.2', 'support.count')]
names(network.data) <- c("from", "to", "weight")

network.data #created the network data for analysis
#------------------------------------------------------------------------------
#CREATE THE NETWORK
#------------------------------------------------------------------------------
library(igraph, quietly = TRUE)
set.seed(1)
my.graph <-graph_from_data_frame(network.data)
plot.igraph(my.graph,
            layout = layout.fruchterman.reingold,
            vertex.label.cex = 0.5,
            edge.arrow.size = 0.1)

random.cluster <- walktrap.community(my.graph)
random.cluster

groupings_df <- data.frame(products = random.cluster$names, 
                           group =random.cluster$membership)
groupings_df

plot(random.cluster, my.graph,
            layout = layout.fruchterman.reingold,
            vertex.label.cex = 0.5,
            edge.arrow.size = 0.1)
  
