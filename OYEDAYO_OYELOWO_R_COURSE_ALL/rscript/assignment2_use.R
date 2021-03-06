
#clear memory list
rm(list=ls())

#set working directory
setwd("C:/Users/oyeda/Desktop/R_COURSE/assignment2")

#get working directory
getwd()
?read.table

#read the first table
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

#create column names
colnames(mat) <- c("DBH", "H", "S", "BA")
mat

#save the data
?write.table
write.table(mat, file = "treeData20.txt", sep="\t", col.names = TRUE, row.names = FALSE)
######


#load the data "puudata_300.txt" from the directory.
data300 <- read.table("puudata_300.txt", sep = "\t", header = TRUE)
?xtabs

#s.tab<-table(data300$LATVKERROS, data300$PUULAJI)
layerSpp <-xtabs(~LATVKERROS+PUULAJI, data300)
#Which is the most common tree species in crown layer 1? 
#answer: the most common tree species in the crown layer 1 is speacies 1

#How many trees of this species
#can be found in the whole data set?
#Answer: there are 91 of these trees in the whole data set.


#d) Which tree species' relative portion in second crown layer 
#is the highest (amount in 2.layer/amount in layer 1 and 2)? 
#divide the total in layer 2 by the sum of the total in layer 1 and 2
#Consider only those species that are present in both crown layers.
highSpp2 <- layerSpp[2,] / (layerSpp[2,] + layerSpp[1,])
highSpp2

#answer: species 2 has the highest relative portion of crown layer2 in layers 1 and 2 
#crown layers, considering those that have both layers present.


#Exercise 4
#Create the following subsets from file puudata_300.txt:
#a) Trees that are measured from plot (KOEALA) 865
treesA <- subset(data300, data300$KOEALA==865)
treesA

#b) Trees that are measured from plots 865 and 490
treesB<- subset(data300, data300$KOEALA==865 | data300$KOEALA==490)
treesB

#c) Those spruce trees (PUULAJI=2) that belong to second crown layer 
#and are over 10 meters tall (the height is given in decimeters).
treesC <- subset(data300, data300$PUULAJI==2 & data300$LATVKERROS==2 & data300$PITUUS>100)
treesC                  

#d) Those trees in the first crown layer, whose diameter is over 150 mm and 
#that are not pines or spruces (species 1 and 2)
treesD<- subset(data300, data300$LATVKERROS==1 & data300$LPM>150 & data300$PUULAJI!=1
                & data300$PUULAJI!=2)
treesD

