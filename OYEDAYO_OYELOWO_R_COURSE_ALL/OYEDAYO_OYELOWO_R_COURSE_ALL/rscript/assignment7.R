#FOR 215

#Assignment 7, Submit in Moodle by Thursday 10.10. 10 am
#Exercise 1
rm(list = ls())

setwd("C:/Users/oyeda/Desktop/R_COURSE/assignment7")

#load the data
data <- read.table("puudata.txt", header = T, sep = "\t")

#Write an if-statement that compares numbers x and y.

#- if x is bigger, R prints out for example "x was bigger"
#- if x and y are equal, R prints: "x and y are equal"
#- if y is bigger, R prints "y is bigger than x"

x <- 33
y <- 11
{
  if (x > y) {
    print("x was bigger")
  } else if (x == y) {
    print("x and y are equal")
  } else
    print("y is bigger than x")
}



#Exercise 2
#Generate two vectors from normal distribution
#(function rnorm()) which both contain hundred entries.
a = c(rnorm(100))
b = c(rnorm(100))

#Sort both vectors with sort() function.
sorted_a <- sort(a, decreasing = F)
sorted_b <- sort(b, decreasing = F)

#- Plot the vectors (first vector as x-value and second as y-value)

#- Define colors so that negative y-values are drawn as red and
#positive values are drawn as
#green (use for- and if statements inside the plot() function)
plot(sorted_a, sorted_b, col = ifelse(sorted_b < 0, "red", "green"))
#?ifelse



#Exercise 3
#Plot the height of all downy birches (PUULAJI=4) that have
#a diameter over 30 cm (x=height,
#y=diameter). Use while() and if() statements.
#METHOD 1
{
  h <- d <- c()
  i <- 0
  while (i < nrow(data)) {
    i = i + 1
    if (data$PUULAJI[i] == 4 & data$LPM[i] > 30) {
      h <- append(h, data$PITUUS[i])
      d <- append(d, data$LPM[i])
      
    }
    
  }
  plot(h,
       d,
       col = "red",
       ylab = "Diameter",
       xlab = "Height")
}

par(new = T)

#METHOD2
{
  h <- d <- c()
  for (i in 1:nrow(data)) {
    if (data$PUULAJI[i] == 4 & data$LPM[i] > 30) {
      h <- append(h, data$PITUUS[i])
      d <- append(d, data$LPM[i])
    }
    
  }
  plot(h,
       d,
       col = "green",
       ylab = "Diameter",
       xlab = "Height")
}


#dim(data)
#levels(data$PUULAJI)


#METHOD 3
#another way of subsetting
dbirch30 = data[data$PUULAJI == 4 & data$LPM > 30, ]
plot(
  dbirch30$PITUUS,
  dbirch30$LPM,
  col = "blue",
  ylab = "Diameter",
  xlab = "Height"
)



#Exercise 4
#Collect (into a vector) the numbers of those plots that have
#aspen (PUULAJI=5) growing on them.
aspen <- c(data[data$PUULAJI == 5, "TUNNISTE"])
length(aspen)
print(aspen)
#sdd<- subset(data, data$PUULAJI==5)[,"TUNNISTE"]



#Exercise 5
#Determine tree-wise basal areas (in cm2) of pines (PUULAJI=1)
#in the puudata. Use for- and
#ifstatements. Place the results in a new column.
#Some help:
#  - Diameter is given in millimeters
#- Basal area can be calculated (pi*d^2)/4
#- First create an empty vector: for example ba<- c()
#- Add the calculated values to the vector with
#append() function (check R-help for help)
pines <- data[data$PUULAJI == 1,]
#pines_ <- subset(data, data$PUULAJI==1)

#create a new column and convert the basal area into it
#notice that the 0.1 is meant to convert mm to cm
#finally, round off to 2dp
pines$ba <- round((pi * ((pines$LPM * 0.1) ^ 2)) / 4, 2)


#ANOTHER METHOD
{
  ba <- c()
  for (i in 1:nrow(data)) {
    if (data$PUULAJI[i] == 1) {
      ba <- append(ba, round((((data$LPM[i] * 0.1) ^ 2
      ) * pi) / 4, 2))
      
    }
  }
  pines1 <- data[data$PUULAJI == 1,]
  pines_ba <- cbind2(pines1, ba)
}


#ANOTHER METHOD USING THE WHILE STATEMENT
{
  ba2 <- c()
  i <- 0
  while (i < nrow(data)) {
    i = i + 1
    if (data$PUULAJI[i] == 1) {
      ba2 <- append(ba2, round((((data$LPM[i] * 0.1) ^ 2
      ) * pi) / 4, 2))
    }
  }
  pines2 <- data[data$PUULAJI == 1,]
  pines_ba2 <- cbind2(pines2, ba2)
}
