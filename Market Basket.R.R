library(arules)
library(arulesViz)
library(tidyverse)
library(repr)
library(igraph)
transaksi <- read.transactions(file="F:/MySQL/R/COBACOBA/Market Basket/Data.tsv",format="single", sep="\t", cols=c(1,2), skip=1)
summary(transaksi)
itemFrequency(transaksi)
itemFrequencyPlot(transaksi,topN=20,support=0.05,col="lightblue", xlab="item", ylab="Frequency(relative)",
                  main="top 20 item frequency with support 5 %")
itemFrequencyPlot(transaksi,topN=20,support=0.05,col="lightblue",type="absolute", xlab="item", ylab="Frequency",
                  main="top 20 absolute item frequency with 5 % support")
data_item <- itemFrequency(transaksi,type="absolute")
data_item <- sort(data_item,decreasing=TRUE)
data_item <- data.frame("Nama_produk"=names(data_item),"Jumlah"=data_item,row.names=NULL)
top_10_item_retail <- data_item[1:10,]
bottom10_item_retail <- data_item[1:10,]
write.csv(bottom10_item_retail,file="bottom10_item_retail.txt", sep=",")
write.csv(top10_item_retail,file="top10_item_retail.txt", sep=",")
transaksi_rules <- apriori(transaksi,parameter = list(support=10/length(transaksi),confidence=0.1,minlen=2, maxlen=3))
write(head(sort(transaksi_rules,by="lift"),10),file="kombinasi_retail.txt")
A <- sort(subset(transaksi_rules,rhs %in% "Tas Makeup"), by="lift")
A <- A[1:3,]
B <- sort(subset(transaksi_rules,rhs %in% "Baju Renang Pria Anak-anak"), by="lift")
B <- B[1:3,]
kombinasi <- c(A,B)
write(kombinasi,file="kombinasi_retail_slow_moving.txt")
plot(transaksi_rules, method="graph")
plot(transaksi_rules, method="grouped")
plot(transaksi_rules, method="graph", control=list(layout=igraph::in_circle()))
