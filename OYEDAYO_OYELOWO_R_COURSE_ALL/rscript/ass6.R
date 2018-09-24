#FOR 215
#Assignment 6 (turn in via Moodle by Thursday 6.10. 10:00 am)
#puudata.xls is used in every exercise
#clear memory
rm(list = ls())

setwd("C:/Users/oyeda/Desktop/R_COURSE/assignment6")

#load the data
data<-read.table("puudata.txt", header = T, sep = "\t")

#Exercise 1
#Construct a linear height model for spruces (PUULAJI = 2) in the 
#first canopy cover layer. Use
#diameter (LPM) as an explanatory variable. Report the model's 
#parameters, coefficient of
#determination, and residual error. Report also the p-value 
#of the explanatory variable.
spruceCanopy1 <- subset(data, data$PUULAJI==2 & data$LATVKERROS==1)
lm_fit = lm(formula= PITUUS~LPM, data=spruceCanopy1)
summary(lm_fit)
names(lm_fit)

#Parameters:

#"LPM" and "intercept"


#[1] "coefficients"  "residuals"     "effects"       "rank"         
#[5] "fitted.values" "assign"        "qr"            "df.residual"  
#[9] "xlevels"       "call"          "terms"         "model
lm_fit$effects
#residuals(lm_fit) # residuals
#residual error: 27.77
coef(lm_fit) # model coefficients
#coefficient of determination: 0.8534
#p-value: < 2.2e-16



#Exercise 2
#Exercise 1 continues.
#Plot the model's residual error as a function of diameter. 
#Does the variance change as diameter grows?
res =residuals(lm_fit)
plot(res~ spruceCanopy1$LPM)
#The variance does not change aas the diameter grows

# {oldpar <- par(mfrow = c(2, 2))
# plot(lm_fit)
# par(oldpar)}



#Exercise 3
#Exercise 1 continues.
#Plot the height of spruces in the first canopy 
#layer as a function of diameter. Add a red curve 
#to the picture that illustrates the values 
#predicted with the model.
spr1<- data[data["LATVKERROS"]==1,]
plot(PITUUS~LPM, spr1)
lines(lowess(spr1$LPM, spr1$PITUUS), col="red")

#abline(lm_fit, col="purple")
#or
#abline(coef = coef(lm_fit), col="red")


#Exercise 4
#Exercise 1 continues.
#Develop the model further. Add new explanatory variables 
#from the puudata and examine how
#model's coefficient of determination and residual error 
#change. Can you make the model better and
#if so, which explanatory variables belong into the model?
lm_fit2= lm(formula= PITUUS~LPM+ELAVALARAJA, data=spruceCanopy1)
lm_fit3= lm(formula= PITUUS~LPM+ELAVALARAJA+LATVUSLEV, data=spruceCanopy1)
summary(lm_fit2)
summary(lm_fit3)
summary(lm_fit)

#ELAVALARAJA(lower limit of living canopy )  was added
#the coefficient of determination increased from 0.761 to 0.8447
#and the residual standard error reduced from 27.07 to 21.82

#However adding LATVUSLEV(width of the canopy) didn't improve the 
#prediction considerably. The coefficient of determination
#only increased from 0.8447 to 0.845 while the residual standard error
#only reduced neglibigly from 21.82 to 21.8. The P value of LATVUSLEV
#was also higher and close to 0.05 compared to others there are 
#much more lower. Also the standard error is much higher.




