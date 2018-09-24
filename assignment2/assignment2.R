rm(list=ls())

setwd("C:/Users/oyeda/Desktop/R_COURSE/assignment2")
getwd()
?read.table
data<-read.table("puudata_20.txt", header = TRUE, sep="\t")
summary(data) #summary of the data

#calculate the mean diameter of the trees
meanDiameter <- mean(data$LPM) 
meanDiameter

#calulate the mean height of the trees
meanHeight <- mean(data$PITUUS) 
meanHeight

#Create a matrix from treedata columns LPM-, PITUUS- and PUULAJI.
#DBH is diameter, h is height and s is the species
?matrix
dbh <- data$LPM
h <- data$PITUUS
s <- data$PUULAJI
mat <- matrix(c(dbh, h, s), nrow=length(dbh))
mat

#Calculate basal area (BA) for every tree and 
#add the results in the matrix as a new column. BA is basal area.
data$BA <- with(data, (pi*h^2)/4)

#joining the BA column to the matrix created earlier
mat <- cbind(mat, data$BA)
mat

colnames(mat) <- c("DBH", "H", "S", "BA")
mat

?write.table
write.table(mat, file = "treeData.txt", sep="\t", col.names = TRUE, row.names = FALSE)
######

data300 <- read.table("puudata_300.txt", sep = "\t", header = TRUE)
?xtabs

#s.tab<-table(data300$LATVKERROS, data300$PUULAJI)
ddd <-xtabs(~LATVKERROS+PUULAJI, data300)
#Which is the most common tree species in crown layer 1? 
#answer: the most common tree species in the crown layer 1 is speacies 1

#How many trees of this species
#can be found in the whole data set?
#Answer: there are 91 of these trees in the whole data set.


#d) Which tree species' relative portion in second crown layer 
#is the highest (amount in 2.layer/amount in layer 1 and 2)? 

#dd<-rowSums (ddd, na.rm = FALSE, dims = 1)
dd<-rowSums(ddd) #sum the values in the rows
dd

#divide the total in layer 2 by the sum of the total in layer 1 and 2
dd[2]/(dd[2] + dd[1]) 
  
#Consider only those species that are present in both crown layers.
