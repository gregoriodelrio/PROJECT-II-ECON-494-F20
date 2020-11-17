#INSTALL THE GGPLOT2 PACKAGE (MUST BE DONE ONCE)
install.packages('ggplot2')
install.packages("corrplot")

#LOAD THE GGPLOT2 LIBRARY (MUST BE DONE EVERY TIME)
library(ggplot2)
library(corrplot)
library(plyr)
library(tseries)

###################################################################
###   Read CSV files containing all spotify tracks and data     ###
### cleaning column names by making vector with new column names###
###################################################################

### import dataset
car_data <- read.csv("https://raw.githubusercontent.com/gregoriodelrio/PROJECT-II-ECON-494-F20/main/CarPrice_Assignment.csv")
dim(car_data)


carlength <- car_data$carlength
curbweight <- car_data$curbweight
enginesize <- car_data$enginesize
horsepower <- car_data$horsepower
highwaympg <- car_data$highwaympg
price <- car_data$price


###################################################################
####  1. Count of each make for dataset and histogram               ###
###################################################################
make_count = matrix(0,21,2)
make_matrix <- matrix(unique(car_data$make))

for (i in 1:length(make_matrix)) {
  make_count[i,2] <- sum(car_data$make == make_matrix[i])
}

make_count <- data.frame(make_count)
make_count[,1] <- make_matrix
names(make_count) <- c("make", "count")

make_count$make <- as.character(make_count$make)
make_count$count <- as.numeric(make_count$count)

ggplot(make_count, aes(x=count, reorder(make, count), width=0.6)) +
  geom_bar(stat = "identity", aes(fill= count)) + 
  geom_text(aes(x= count, y= make, label= count, hjust= 0), size= 4) + 
  theme(legend.position = "none") +
  xlab("Count") + ylab("Make")


###################################################################
####  2.	Scatter Plot of Price to Each Variable                   ###
###################################################################

# price:make
ggplot(car_data, aes(x= price, y= make)) +
  geom_point(aes(size=1, color=make)) +
  xlim(NA,50000) +
  theme(legend.position = "none")


# price:carlength
ggplot(car_data, aes(x= carlength, y= price)) +
  geom_point(aes(color=make, size=2)) +
  geom_smooth() + 
  ylim(NA, 50000) +
  theme(legend.position = "none")


# price:curbweight
ggplot(car_data, aes(x= curbweight, y= price)) +
  geom_point(aes(color=make, size=2)) +
  geom_smooth() + 
  ylim(NA, 50000) +
  theme(legend.position = "none")

# price:enginesize
ggplot(car_data, aes(x= enginesize, y= price)) +
  geom_point(aes(color=make, size=2)) +
  geom_smooth() + 
  ylim(NA, 50000) +
  theme(legend.position = "none")

# price:horsepower
ggplot(car_data, aes(x= horsepower, y= price)) +
  geom_point(aes(color=make, size=2)) +
  geom_smooth() + 
  ylim(NA, 50000) +
  theme(legend.position = "none")


# price:highwaympg
ggplot(car_data, aes(x= highwaympg, y= price)) +
  geom_point(aes(color=make, size=2)) +
  geom_smooth() + 
  ylim(NA, 50000) +
  theme(legend.position = "none")



###################################################################
####  3.	Column Variables Correlation                            ###
###################################################################
variables_corr <- data.frame(car_data$carlength, car_data$curbweight, 
                             car_data$enginesize, car_data$horsepower, 
                             car_data$highwaympg, car_data$price)

variables_corr <- data.frame(carlength, curbweight, enginesize, horsepower, 
                             highwaympg, price)
corrplot(cor(variables_corr))
corrplot(cor(variables_corr), method = "number")

###################################################################
####  4.	Creating Higher Order Polynomials          ###
###################################################################
### INCORPORATING NONLINEAR (POLYNOMIAL) TRANSFORMATIONS OF DISPL

### Variables
# carlength, curbweight, enginesize, horsepower, highwaympg

# QUADRATIC TRANSFORMATION (2nd ORDER)
car_data$carlength_2 <-  car_data$carlength^2
car_data$curbweight_2 <- car_data$curbweight^2
car_data$enginesize_2 <- car_data$enginesize^2
car_data$horsepower_2 <- car_data$horsepower^2
car_data$highwaympg_2 <- car_data$highwaympg^2

#CUBIC TRANSFORMATION (3rd ORDER)
car_data$carlength_3 <-  car_data$carlength^3
car_data$curbweight_3 <- car_data$curbweight^3
car_data$enginesize_3 <- car_data$enginesize^3
car_data$horsepower_3 <- car_data$horsepower^3
car_data$highwaympg_3 <- car_data$highwaympg^3

#A LOGARITHMIC TRANSFORMATION
car_data$carlength_ln <-  log(car_data$carlength)
car_data$curbweight_ln <- log(car_data$curbweight)
car_data$enginesize_ln <- log(car_data$enginesize)
car_data$horsepower_ln <- log(car_data$horsepower)
car_data$highwaympg_ln <- log(car_data$highwaympg)

dim(car_data) # (205, 42)


###################################################################
####  5.	Partition Training and Testing Data          ###
###################################################################
set.seed(619)

train_70 <- .7
test_30 <- .3

### number of observations
observations <- length(car_data$make) #205 observations
train_size <- floor(train_70 * observations) #143
train_ind <- sample(observations, train_size) #143

### making dataframes for training data and testing data. 
###     used for training and testing all models
train_data <- car_data[train_ind,] #dim = 143, 27
test_data <- car_data[-train_ind,] #dim = 62, 27

### PLOTTING THE TRAINING AND TESTING PARTITIONS###
### train_data scatter plot
train_scat <- ggplot(train_data, aes(x= enginesize, y=price, size=2)) +
  geom_point(colour="blue", pch=1) +
  ylim(NA, 50000) + theme(legend.position =  "none") + xlim(50,350)
train_scat

### test_data scatter plot
test_scat <- ggplot(test_data, aes(x= enginesize, y=price, size=2)) +
  geom_point(colour="red", pch=3) + xlim(50,350) +
  ylim(NA, 50000) + theme(legend.position = "none")
test_scat

### train and test_data scatter plot (ALL DATA)
train_scat + 
  geom_point(data= test_data, aes(x=enginesize, y=price, size=2), colour="red", pch=3)

###################################################################
####  6.  Model 1: All Variables to Predict Price        ###
###################################################################

# scatterplot variables + carbody 
M1 <- lm(price ~ make + carbody + carlength + curbweight +
           enginesize + horsepower + highwaympg +  wheelbase, train_data)
summary(M1) 

# Generating Predictions on the TRAINING data
Predict_1_IN <- predict(M1, train_data)

# Generating Predictions on the TEST data
Predict_1_OUT <- predict(M1, test_data)

# COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_1_IN<-sqrt(sum((Predict_1_IN-train_data$price)^2)/length(Predict_1_IN))
RMSE_1_OUT<-sqrt(sum((Predict_1_OUT-test_data$price)^2)/length(Predict_1_OUT))

RMSE_1_IN #IN-SAMPLE ERROR
RMSE_1_OUT #OUT-OF-SAMPLE ERROR


##############################################################################
#### 7.	Model 2: Significant Variables and Second Power Polynomials     ###
#############################################################################

# scatterplot variables + carbody + Second Power Polynomials
M2 <- lm(price ~ make + carbody + carlength + curbweight + enginesize + 
           horsepower + highwaympg + carlength_2 + curbweight_2 + enginesize_2 + 
           horsepower_2 + highwaympg_2, train_data)
summary(M2)

# Generating Predictions on the TRAINING data
Predict_2_IN <- predict(M2, train_data)

# Generating Predictions on the TEST data
Predict_2_OUT <- predict(M2, test_data)

# COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_2_IN<-sqrt(sum((Predict_2_IN-train_data$price)^2)/length(Predict_2_IN))  #computes in-sample error
RMSE_2_OUT<-sqrt(sum((Predict_2_OUT-test_data$price)^2)/length(Predict_2_OUT)) #computes out-of-sample 

RMSE_2_IN #IN-SAMPLE ERROR
RMSE_2_OUT #OUT-OF-SAMPLE ERROR


###############################################################################
#### 8. Model 3: Significant Variables, Second, and Third Power Polynomials ###
###############################################################################
# scatterplot variables + carbody + Second Power Polynomials
M3 <- lm(price ~ make + carbody + carlength + curbweight + enginesize + 
           horsepower + highwaympg + carlength_2 + curbweight_2 + enginesize_2 + 
           horsepower_2 + highwaympg_2 + carlength_3 + curbweight_3 + enginesize_3 + 
           horsepower_3 + highwaympg_3, train_data)

summary(M3) # R-Square = .9437, .9253

# Generating Predictions on the TRAINING data
Predict_3_IN <- predict(M3, train_data)

# Generating Predictions on the TEST data
Predict_3_OUT <- predict(M3, test_data)

# COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_3_IN<-sqrt(sum((Predict_3_IN-train_data$price)^2)/length(Predict_3_IN))  #computes in-sample error
RMSE_3_OUT<-sqrt(sum((Predict_3_OUT-test_data$price)^2)/length(Predict_3_OUT)) #computes out-of-sample 

RMSE_3_IN #IN-SAMPLE ERROR
RMSE_3_OUT #OUT-OF-SAMPLE ERROR


###############################################################################
####  9.	Model 4: Significant Variables and Log Variables     ###
###############################################################################
#### MODEL 4: LOG
# scatterplot variables + carbody
M4 <- lm(price ~ make + carbody + carlength_ln + curbweight_ln + 
           enginesize_ln + horsepower_ln + highwaympg_ln, train_data)

summary(M4) # R-Square = .9437, .9253

# Generating Predictions on the TRAINING data
Predict_4_IN <- predict(M4, train_data)

# Generating Predictions on the TEST data
Predict_4_OUT <- predict(M4, test_data)

# COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_4_IN<-sqrt(sum((Predict_4_IN-train_data$price)^2)/length(Predict_4_IN))  #computes in-sample error
RMSE_4_OUT<-sqrt(sum((Predict_4_OUT-test_data$price)^2)/length(Predict_4_OUT)) #computes out-of-sample 

RMSE_4_IN #IN-SAMPLE ERROR
RMSE_4_OUT #OUT-OF-SAMPLE ERROR


#################################################################
####  10.	Dataframe with Each Model and its RMSE    ###
#################################################################
RMSE_df <- data.frame("Model" = c(1, 2, 3, 4), 
                      "RMSE_IN" = c(RMSE_1_IN, RMSE_2_IN, RMSE_3_IN, RMSE_4_IN), 
                      "RMSE_OUT" = c(RMSE_1_OUT, RMSE_2_OUT, RMSE_3_OUT, RMSE_4_OUT),
                      "R-Squared" = c(0.954, 0.9674, 0.9704, 0.9392))
RMSE_df
View(RMSE_df)

summary(M1)
# M1 R^2 = 0.954
# M2 R^2 = 0.9674
# M3 R^2 = 0.9704
# M4 R^2 = 0.9392