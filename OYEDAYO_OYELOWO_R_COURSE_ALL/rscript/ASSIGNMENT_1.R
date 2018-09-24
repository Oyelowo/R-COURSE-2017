

##//Exercise 1


##a) Form three separate vectors from scalars 4,5 and 33; -48, 0 and 45; 7, 3 and 1
a<-c(4, 5, 33)
b<-c(-48, 0, 45)
c<-c(7, 3, 1)


##b) Combine vectors into one 3x3 matrix
m=matrix(c(a,b,c), ncol=3, nrow = 3, byrow = TRUE)
m
##c) Name rows of the matrix as r1, r2 and r3. Name columns of the matrix as A, B and C
rownames(m) <- c("r1","r2","r3")
colnames(m) <- c("A", "B", "C")
m


##Exercise 2
##Generate the following vectors with functions rep() and seq()
##a) [0 0 0 0 0 1 1 1 1 1 2 2 2 2 2 3 3 3 3 3 4 4 4 4 4]
rep(0:4, rep(5,5))
rep(0:4, each=5)


##b) [1 2 3 4 5 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5]
##y <- seq(0:4)
##y
rep(1:5, rep(5,5), length.out=25)
##or
rep(seq(0:4), rep(5,5), length.out=25)


#Exercise 3
#Solar radiation was measured in a greenhouse at eight different times. Observations were 11.1, 10.6, 6.3,
#8.8, 10.7, 11.2, 8.9 and 12.2 units.
#a) Save measurements as variable solar.radiation (set value with a vector containing all
#                                                 measurements)
solar.radiation <- c(11.1, 10.6, 6.3, 8.8, 10.7, 11.2, 8.9, 12.2)

#b) Define mean, median and variance of variable solar.radiation (with pre-determined R-functions)
mean(solar.radiation)
median(solar.radiation)
var(solar.radiation)

#c) Add value 10 to every observation and name the new vector as sr10. Define mean, median and
#variance of modified vector. Which of the determined values changed from the previous and how
#much?
sr10 = solar.radiation + 10
sr10
mean(sr10)
median(sr10)
var(sr10)

#the mean and the median changed and increased by 10
#from the previous values while the variance remained the same at 3.525


#d) Multiply the original observations by -2 and name the resulting vector as srm2. What happened to
#mean, median and variance?
srm2=solar.radiation * -2
srm2
mean(srm2)
median(srm2)
var(srm2)

#the mean became a negative value of -19.95, the median also became 
#a negative value of -21.3 while the variance remained positive value but higher at 14.1


#e) Variance is generally determined with following formulas:



#Which of the previous two is used in R-function var()?

#copying the formula didnt work well. showed strange characters.
# I checked the answer online and the second is the correct one i.e b.


#Exercise 4
#Generate 25 random integers from interval [1,100] and place them in row-wise in 5x5 matrice.
rand=ceiling(runif(25, 1, 100))
rand
mat=matrix(rand, ncol=5, nrow=5, byrow=T)
mat


#a) Name the rows as r1-r5 and the columns as c1-c5
rownames(mat) <- c("r1","r2","r3","r4","r5")
colnames(mat) <- c("c1","c2","c3","c4","c5")
mat
#b) Determine the sum of numbers in the matrice
sum(mat)
#c) Substract the minimum of the matrice from every other figure in the Matrix
mat2=mat - min(mat)
mat2
#d) Print the figure that is in column 3 of row 4
mat2[4,3]


# Exercise 5
# Search the internet for information on function getEventProb.
# a) To which R-package does the function belong?
#  It belongs to the package "dice"

#   b) What does the function actually do?
# It calculates the probability of a specified set of dice-rolling events

#   c) With which parameters can you control the function?
#getEventProb(nrolls, ndicePerRoll, nsidesPerDie, eventList, orderMatters = FALSE)
# Arguments
# nrolls A single positive integer representing the number of dice rolls to make
# ndicePerRoll A single positive integer representing the number of dice to use in each dice roll
# nsidesPerDie A single positive integer representing the number of sides on each die (getEventProb's
#dice-rolling process involves only one type of die per call)

# eventList A list object, each element of which is a vector that constrains a single dice
# roll in the dice-rolling process (see Details below)
# orderMatters A logical flag indicating whether the order of the elements of eventList should
# constrain the event space; if TRUE, eventList must specify constraints for
# every dice roll-i.e., it must contain exactly nrolls elements (some of which
#may be "empty" constraints listing all possible outcomes of a dice roll, i.e., a
#vector from ndicePerRoll to (ndicePerRoll * nsidesPerDie))



#   Download the package to your computer and activate it from the R-library. Then, answer to following
# questions with help of the tools found in the package.
install.packages("dice")
library(dice)
?getEventProb

# d) What are the odds that a throw with two 6-sided dice results sum of 7?
getEventProb(nrolls = 1, ndicePerRoll = 2, nsidesPerDie = 6, eventList = list(7), orderMatters = F)

#   e) What are the odds that a throw with two 10-sided dice results two tens? (the sum of dice is 20)?
getEventProb(nrolls = 1, ndicePerRoll = 2, nsidesPerDie = 10, eventList = list(20))

# ##Exercise 5
# -  **Search the internet for information on function getEventProb.**
#   -  a) To which R-package does the function belong?
#   -  answer: It belongs to the package "dice"_**
#   
#   -  b) What does the function actually do?
#   -  answer: It calculates the probability of a specified set of dice-rolling events
#   
#   -  c) With which parameters can you control the function?
#   -  You can control the function with the following parameters:
#   -  nrolls, ndicePerRoll, nsidesPerDie, eventList, orderMatters = FALSE)
#   
#   -   Arguments
#   -   nrolls: A single positive integer representing the number of dice rolls to make
# -   ndicePerRoll: A single positive integer representing the number of dice to use in each dice roll
# -   nsidesPerDie: A single positive integer representing the number of sides on each die (getEventProb's dice-rolling process involves only one type of die per call)
# -  eventList: A list object, each element of which is a vector that constrains a single dice roll in the dice-rolling process (see Details below)
# -  orderMatters: A logical flag indicating whether the order of the elements of eventList should constrain the event space; if TRUE, eventList must specify constraints for every dice roll-i.e., it must contain exactly nrolls elements (some of which may be "empty" constraints listing all possible outcomes of a dice roll, i.e., a vector from ndicePerRoll to (ndicePerRoll * nsidesPerDie))
# 
# -  Download the package to your computer and activate it from the R-library. Then, answer to following questions with help of the tools found in the package.**
# 
# -  d) What are the odds that a throw with two 6-sided dice results sum of 7?

#install.packages("dice")
library(dice)
odds1<-getEventProb(nrolls = 1, ndicePerRoll = 2, nsidesPerDie = 6, eventList = list(7), orderMatters = F)
print(paste("The odds that a throw with two 6-sided dice results sum of 7 is", round(odds1,4)))


#-  e) What are the odds that a throw with two 10-sided dice results two tens? (the sum of dice is 20)?

odds2<-getEventProb(nrolls = 1, ndicePerRoll = 2, nsidesPerDie = 10, eventList = list(20))
print(paste("the odds that a throw with two 10-sided dice results two tens (the sum of dice is 20) is",  round(odds2,4)))


