#read the file
#The dataset can be downloaded from: https://snap.stanford.edu/data/amazon-meta.html
#The project on github https://github.com/malnapolya/IncubatorProjectProposal
filename="amazon-meta.txt"
setwd("Incubator")
library(data.table)
amazon<-read.table(filename,nrows=3000000,sep="\n",skip=2)


library(plyr)
library(dplyr)
library(tidyr)

#create a clean items table
itemstemp<-amazon %>% filter(grepl("^Id:",V1) | grepl("^ASIN:",V1)| grepl("^? ? ? ? ?title:",V1)| grepl("^? ? ? ? group:",V1)| grepl("^? ? ? ? salesrank:",V1))
items<- itemstemp%>% separate(V1, c("descr", "value"), ":",extra="merge")

newitems<- data.frame(ID=integer(),
                      ASIN=integer(),
                      title=character(),
                      group=character(),
                      salesrank=integer(),
                      stringsAsFactors = FALSE)

for (cell in 1:nrow(items)) {
        if (items[cell,1] %like% "*Id") {
                newitems[dim(newitems)[1]+1,1]<-items[cell,2]
        }
        else if (items[cell,1] %like% "*ASIN") {
                if (items[cell-1,1] %like% "*Id") {
                        newitems[dim(newitems)[1],2]<-items[cell,2]
                }
        }
        else if (items[cell,1] %like% "*title") {
                if (items[cell-2,1] %like% "*Id") {
                        newitems[dim(newitems)[1],3]<-items[cell,2]
                }
        }
        else if (items[cell,1] %like% "*group") {
                if (items[cell-3,1] %like% "*Id") {
                        newitems[dim(newitems)[1],4]<-items[cell,2]
                }
        }
        else if (items[cell,1] %like% "*salesrank") {
                if (items[cell-4,1] %like% "*Id") {
                        newitems[dim(newitems)[1],5]<-items[cell,2]
                }
        }
}

#Create a clean reviews table
reviewstemp<-amazon %>% filter(grepl("^Id:",V1) | grepl("^? ? ? reviews:",V1))

reviews<- reviewstemp%>% separate(V1, c("descr", "value","totalvalue","downvalue","avratingvalue"), ":",extra="merge",fill="right")

newreviews<- data.frame(ID=integer(),
                        totalnumber=character(),
                        rating=character(),
                        stringsAsFactors = FALSE)
for (cell in 1:nrow(reviews)) {
        if (reviews[cell,1] %like% "*Id") {
                newreviews[dim(newreviews)[1]+1,1]<-reviews[cell,2]
        }
        else if (reviews[cell,1] %like% "*reviews") {
                if (reviews[cell-1,1] %like% "*Id") {
                        newreviews[dim(newreviews)[1],2]<-as.integer(trimws(unlist(strsplit(reviews[cell,3], split=' downloaded', fixed=TRUE))[1]))
                        newreviews[dim(newreviews)[1],3]<-reviews[cell,5]
                }
         }
        
}

#Create the graphs
library(ggplot2)

merged<-merge(newreviews,newitems,by="ID")

#FREQUENCY OF RATINGS BY PRODUCT CATEGORY
qplot(as.integer(rating),data=merged[merged$totalnumber!=0 & !is.na(merged$totalnumber),],geom="histogram",binwidth=0.5,fill=group,xlab="Rating",ylab="Frequency of Rating",main="Frequency of ratings by product category")


#TOTAL NUMBER OF RATINGS ~ AVERAGE RATINGS
g<-ggplot(merged[merged$totalnumber!=0 & !is.na(merged$totalnumber),],aes(as.integer(totalnumber),as.integer(rating)))
g+geom_point()+geom_smooth() + coord_cartesian(xlim = c(0, 200)) +labs(x="Total Number of ratings",y="Average rating")

#Other plots (experimenting)
g1<-ggplot(merged[merged$totalnumber!=0 & !is.na(merged$totalnumber),],aes(as.integer(totalnumber),fill=rating))

g1+geom_histogram(binwidth = 5) + coord_cartesian(xlim = c(0, 100)) + labs(x="Total number of votes")

qplot(totalnumber,data=merged[merged$totalnumber!=0 & !is.na(merged$totalnumber),],geom="histogram")

