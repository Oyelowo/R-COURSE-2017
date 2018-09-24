
rm(list=ls())
setwd("C:/Users/oyeda/Desktop/R_COURSE/assignment3")
getwd()
?read.table
read.csv()

#Modify puudata_rikki.xls (found in Moodle) data into text file and
#import it into R with read.table function. 
#Set the parameters so that the first two rows are left unread (the rows
#contain random letters) and only the first 20 rows are imported into R. 
data <- read.table("puudata_rikki.txt", nrows = 20, header = T, skip = 2, sep = ";")
data

#calculate the mean height of the trees
mean.height<-mean(data$PITUUS)
mean.height

#read the data "puudata" in my working directory
data2<- read.table("puudata.txt", header=T, sep = "\t")
data2
summary(data2)f
 
#Define the correlation between diameter and height with cor function
#a) for spruces (PUULAJI = 2) in the first crown layer (LATVKERROS = 1)
spruceLayer1 <- subset(data2, data2$PUULAJI==2 & data2$LATVKERROS==1)
#cor.test(spruceLayer1$LPM, spruceLayer1$PITUUS, method = c("pearson", "kendall", "spearman"))

#correlation between diameter and the height for birches in the 
#first crown layer (PUULAJI = 3). 
cor(spruceLayer1$LPM, spruceLayer1$PITUUS, 
    method = c("pearson", "kendall", "spearman"))

#b) for birches in the first crown layer (PUULAJI = 3). 
birchLayer1<-subset(data2, data2$PUULAJI==3 & data2$LATVKERROS==1)
cor(birchLayer1$LPM, birchLayer1$PITUUS,
    method = "pearson")

#c) For which of the previous species the diameter explains 
#greater amount of variation in tree height
#Answer: the diameter of spruce explains greater amount of variation
#in the treee height.

#Using command function, construct your own function that
#a) calculates the product of three inserted parameters
prod3<- function(x,y,z){
  pr<-x*y*z
  return(pr)
}
prod3(74,11,22)  #test function
?prod
#b) function  prints letter " R" x times when x is functions parameter
printR<-function(x){
  rprint<- rep("R", x)
  return(rprint)
}
printR(21)
