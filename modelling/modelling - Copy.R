#FOR 215
#Modelling task
#Your task is to create regression models for diameter and
#height of linden (lehmus) and maple
#(vaahtera). Laser scanning-based features are used as
#explanatory variables.


rm(list = ls())

setwd("C:/Users/oyeda/Desktop/R_COURSE/modelling")

#load the data
data1 <- read.table("koepuut.txt", header = T, sep = "\t")
data2 <- read.table("mallinnettavat.txt", header = T, sep = "\t")
#Data:
#Modeling data (koepuut.xlsx):
#The file contains 1138 trees from the test area. DBH and
#species (puulaji) have been determined
#from every tree. In addition, height is known for about
#half of the trees. 10 ALS metrics have been
#calculated for each tree. The data contains several
#tree species.
#Trees with no measured DBH and height (mallinnettavat.xlsx):
#10 ALS metrics have been calculated for each tree. DBH
#and height are predicted with models. The
#data consists of linden and maples.

#Fork flow:
#  Creating the models with modelling data
#- Separate linden and maples from the data (there are
#also other species in the modelling  data).

#è Choose (by testing) best predictors from ALS features
#for each model (best features may vary between species).
#Predicting variables
#- Form your own functions that utilize the created models
#è models' explanatory variables are used as parameters
#for the functions
#- Form a loop structure that runs through all trees in
#mallinnettavat.txt and calls for the
#correct functions according to tree species
#è save the predicted diameter (mm) and height (dm) values
#for example in dbh and h
#vectors
#- Create a result matrix that contains the following columns:
#tree number (Puunro), tree
#species (puulaji), dbh in cm and height in m
#- Export the matrix into csv file


# linden (lehmus) and maple (vaahtera)

#firstly, I have to use the data with some known heights to create the model
linden1 <- data1[data1$puulaji == "lehmus",]
#linear model for dbh of linden
linfit_dbh <- lm(dbh_mm ~ Hmax + Hmean + h30 + h50 + h70 + h90 + p30
                 + p50 + p70 + p90, linden1)
summary(linfit_dbh)

#Estimate Std. Error t value Pr(>|t|)
#(Intercept)  -70.199     21.831  -3.216 0.001378 **
#  Hmax           6.818      3.268   2.086 0.037429 *
#  Hmean         64.526     27.326   2.361 0.018555 *
#  h30          -31.825     12.282  -2.591 0.009817 **
#  h50           -3.033     11.354  -0.267 0.789448
#h70          -23.176     10.823  -2.141 0.032687 *
#  h90           12.948      7.800   1.660 0.097488 .
#p30           46.208     39.959   1.156 0.248025
#p50         -195.585     46.578  -4.199 3.12e-05 ***
#  p70           18.229     55.552   0.328 0.742926
#p90          158.242     46.969   3.369 0.000807 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#from the above, we can take away  h50, p30, and p70, because,
#they all have p values below 0.05
#thus, i'm left with dbh_mm ~ Hmax + Hmean + h30 + h70 + p50 + h90 + p90

#To further corroborate this, I use a stepwise regression next

# Stepwise Regression
library(MASS)
linfit_dbh <- lm(dbh_mm ~ Hmax + Hmean + h30 + h50 + h70 + h90 + p30
                 + p50 + p70 + p90, linden1)
step <- stepAIC(linfit_dbh, direction = "both")
step$anova # display results

#?stepAIC
#Below is the result of the analaysis, which confirms earlier the choice
#made earlier
#Stepwise Model Path
#Analysis of Deviance Table

#Initial Model:
#  dbh_mm ~ Hmax + Hmean + h30 + h50 + h70 + h90 + p30 + p50 + p70 +
#  p90

#Final Model:
#  dbh_mm ~ Hmax + Hmean + h30 + h70 + h90 + p50 + p90

#Step Df  Deviance Resid. Df Resid. Dev      AIC
#1                          552    1892368 4593.594
#2 - h50  1  244.6803       553    1892613 4591.667
#3 - p70  1  436.4771       554    1893050 4589.796
#4 - p30  1 3994.8186       555    1897044 4588.983

#Thus, my final model for dbh for linden would be:
#dbh_mm ~ Hmax + Hmean + h30 + h70 + p50 + h90 + p90


#Next is for the diameter
linden1 <- data1[data1$puulaji == "lehmus",]
#linear model for dbh of linden
linfit_h <- lm(h_dm ~ Hmax + Hmean + h30 + h50 + h70 + h90 + p30
               + p50 + p70 + p90, linden1)
summary(linfit_h)

#Call:
# lm(formula = h_dm ~ Hmax + Hmean + h30 + h50 + h70 + h90 + p30 +
#     p50 + p70 + p90, data = linden1)

#Residuals:
#  Min      1Q  Median      3Q     Max
#-88.831  -5.673   1.032   7.379  27.274

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#(Intercept)    5.192      8.298   0.626  0.53223
#Hmax           3.693      1.233   2.994  0.00307 **
#  Hmean        -12.727     12.139  -1.048  0.29559
#h30            9.596      5.316   1.805  0.07242 .
#h50           -9.270      4.706  -1.970  0.05012 .
#h70           13.175      4.720   2.791  0.00572 **
#  h90            4.222      3.102   1.361  0.17489
#p30            4.994     14.336   0.348  0.72790
#p50          -29.220     17.123  -1.707  0.08935 .
#p70           22.127     21.603   1.024  0.30686
#p90           14.675     18.186   0.807  0.42060
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 14.51 on 216 degrees of freedom
#(336 observations deleted due to missingness)
#Multiple R-squared:  0.8759,	Adjusted R-squared:  0.8702
#F-statistic: 152.5 on 10 and 216 DF,  p-value: < 2.2e-16

#from the above, I can eliminate Hmean, h50, h90, p30, p50, p70 and p90
#model can then be:  h_dm~ Hmax + h30 + h70

#next, use stepwise regression to eliminate the redundant variables:
step_h <- stepAIC(linfit_h, direction = ("both"))
step_h$anova #display results

#final model: h_dm ~ Hmax + h30 + h50 + h70 + p50 + p70



#cor(linden1[,"dbh_mm"],linden[,4:13])

##################################################################
#subset the dataframe to vaahtera species
maple1 <- data1[data1$puulaji == "vaahtera", ]

#multiple linear regression, using all the variables
mapfit_dbh <- lm(dbh_mm ~ Hmax + Hmean + h30 + h50 + h70 + h90 + p30
                 + p50 + p70 + p90, maple1)
#get the summary details
summary(mapfit_dbh)

#Perform a stepwise regression to remove the redundant variables
m_step_dbh <- stepAIC(mapfit_dbh, direction = ("both"))
m_step_dbh$anova #display results

#Final Model based on the chosen variables by stepwise regression:
#  dbh_mm ~ h30 + p90 + h90

#######################################################
#Linear model using all the variables
mapfit_h <- lm(h_dm ~ Hmax + Hmean + h30 + h50 + h70 + h90 + p30
               + p50 + p70 + p90, maple1)
summary(mapfit_h)

#Stepwise regression
m_step_h <- stepAIC(mapfit_h, direction = ("both"))
m_step_h$anova #display results

#Final Model chosen after using the pvalues and stepwise regression:
#  h_dm ~ h30 + h50 + h70




##- Create linear models for diameter and height of linden
#and maple (four different models).

##FOR LINDEN
#dbh_mm ~ Hmax + Hmean + h30 + h70 + p50 + h90 + p90
#h_dm ~ Hmax + h30 + h50 + h70 + p50 + p70

##FOR MAPLE
#dbh_mm ~ h30 + p90 + h90
#h_dm ~ h30 + h50 + h70

###Final creation of models based on the chosen predictors
##LINDEN
#for diameter at breast height(DBH)
lindbh_model <- lm(dbh_mm ~ Hmax + Hmean + h30 + h70
                   + p50 + h90 + p90, linden1)
lindbh_coef <- coef(lindbh_model) #extract the coefficients

#model for height for linden species
linh_model <-
  lm(h_dm ~ Hmax + h30 + h50 + h70 + p50 + p70, linden1)
linh_coef <- coef(linh_model) #extract the coefficients


#MAPLES
#model for diameter at breast height(DBH) of maple
mapdbh_model <- lm(dbh_mm ~ Hmax + Hmean + h30 + h70 + p50
                   + h90 + p90, maple1)
mapdbh_coef <- coef(mapdbh_model) #extract the coefficients

#model for height of maple
maph_model <- lm(h_dm ~ h30 + h50 + h70, maple1)
maph_coef <- coef(maph_model) #extract the coefficients


#create function for calculating the DBH of linden, by using the model
lin_DBH_fun <-
  function(Hmax , Hmean , h30 , h70 , p50 , h90 , p90) {
    lindbh_mod <-
      round((
        lindbh_coef[1] + (lindbh_coef[2] * Hmax) + (lindbh_coef[3] * Hmean) +
          (lindbh_coef[4] * h30) + lindbh_coef[5] * h70 + lindbh_coef[6] * p50
        + (lindbh_coef[7] * h90) + (lindbh_coef[8] * p90)
      ),
      2)
    return(lindbh_mod)
  }

#function for calculating the Height of linden, by using the model
lin_H_fun <- function(Hmax,  h30 , h50, h70, p50, p70) {
  linh_mod <- round((
    linh_coef[1] + (linh_coef[2] * Hmax) +
      (linh_coef[3] * h30) + linh_coef[4] * h50 + linh_coef[5] *
      h70 + linh_coef[6] * p50
    + (linh_coef[7] * p70)
  ),
  2)
  return(linh_mod)
}

#create function for calculating the DBH of maple, by using the model
map_DBH_fun <-
  function(Hmax , Hmean , h30 , h70 , p50 , h90 , p90) {
    mapdbh_mod <-
      round((
        mapdbh_coef[1] + (mapdbh_coef[2] * Hmax) + (mapdbh_coef[3] * Hmean) +
          (mapdbh_coef[4] * h30) + mapdbh_coef[5] * h70 + mapdbh_coef[6] * p50
        + (mapdbh_coef[7] * h90) + (mapdbh_coef[8] * p90)
      ),
      2)
    return(mapdbh_mod)
  }

#create function for calculating the Height of Maple, by using the model
map_H_fun <- function(h30 , h50, h70) {
  maph_mod <- round((
    maph_coef[1] + (maph_coef[2] * h30)
    + maph_coef[3] * h50 + maph_coef[4] * h70
  ), 2)
  return(maph_mod)
}


#data2[1,]

#Create a loop to predict the heigt and dbh of maple and linden
#by using the created models
{
  dbh_ln <- h_ln <- dbh_map <- h_map <- puunro <- puulaji <- c()
  for (i in 1:nrow(data2)) {
    if (data2$puulaji[i] == "lehmus") {
      dbh_ln <- append(
        dbh_ln,
        lin_DBH_fun(
          data2$Hmax[i] ,
          data2$Hmean[i]
          ,
          data2$h30[i] ,
          data2$h70[i] ,
          data2$p50[i]
          ,
          data2$h90[i] ,
          data2$p90[i]
        )
      )
      
      h_ln <-
        append(
          h_ln,
          lin_H_fun(
            data2$Hmax[i] ,
            data2$h30[i] ,
            data2$h50[i] ,
            data2$h70[i]
            ,
            data2$p50[i] ,
            data2$p70[i]
          )
        )
      puunro <- append(puunro, data2$Puunro[i])
      puulaji <- append(puulaji, as.character(data2$puulaji[i]))
      
    }
    
#here, I can also use  else alone instead of if (data2$puulaji[i] == "vaahtera") 
    if (data2$puulaji[i] == "vaahtera") {
      dbh_map <-
        append(dbh_map, (
          map_DBH_fun(
            data2$Hmax[i] ,
            data2$Hmean[i],
            data2$h30[i]
            ,
            data2$h70[i] ,
            data2$p50[i],
            data2$h90[i] ,
            data2$p90[i]
          )
        ))
      
      h_map <-
        append(h_map, (map_H_fun(data2$h30[i] , data2$h50[i] , data2$h70[i])))
      puunro <- append(puunro, data2$Puunro[i])
      puulaji <- append(puulaji, as.character(data2$puulaji[i]))
    }
  }
  #combine the  dbh and height vectors created for linden column-wise
  a <- cbind(dbh_ln, h_ln)
  #combine the  dbh and height vectors created for maple column-wise
  b <- cbind(dbh_map, h_map)
  
  #now, combine both data but row_wise since we want them to be merged
  c <- rbind(a, b)
#finally, add the plot number and names of the species which are
  #vectors created earlier for the entire data
  maple_linden <- cbind.data.frame(puunro, puulaji, c)
  
  #reset the index/rownames to default index
  rownames(maple_linden) <- NULL
  
  #rownames(maple_linden)<-rownames(maple_linden, do.NULL=T, prefix = "Obs.")
  
  #rename columns one and two
 colnames(maple_linden)[colnames(maple_linden)=="dbh_ln"] <- "DBH"
 colnames(maple_linden)[colnames(maple_linden)=="h_ln"] <- "height"
  
 #names(maple_linden)[1]<-"DBH"  #wont use this cos column number might change
  #names(maple_linden) = c("DBH", "height")
  
}

#write the data into csv format
write.csv(maple_linden, file = "C:/Users/oyeda/Desktop/R_COURSE/modelling/Trees_DBH_H"
          , row.names = TRUE)



