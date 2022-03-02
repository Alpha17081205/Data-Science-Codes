library(readr)
library(ggplot2)
library(tidyverse)
theme_set(theme_bw(16))

#########################################################
################## Visulaising the Dataset###############
#########################################################

x<-seq(from = 2*pi/100,to = 2*pi,2*pi/100)
x ## Printing x values
error <- rnorm(100,mean = 0, sd = sqrt(0.1))
y <- -sin(x)+error
y## printing y vakues


#plotting the dataset
Data_set <- data.frame(x,y)
Org_Dataplot <- ggplot(data = Data_set, aes(x= x,y=y))+
  geom_point(color = "red")+
  labs(x = "X values", y = "-sin(X)",
       title ="Original Data set")+
  theme(plot.title=element_text(hjust=0.5))
Org_Dataplot #plotting the dataset

#########################################################
########################################################

#defining the models
H_0 = lm(y ~ 1, data = Data_set) ## constant y model
H_1 = lm(y ~ poly(x, degree = 1), data = Data_set)  ## straight ine model

## plotting the models
set.seed(300)
ggplot(data = Data_set, aes(x= x, y = y))+
  geom_line(aes(x, predict(H_0, newdata = data.frame(x=x))),
            col = "green", lwd = 1, lty = 2)+
  geom_line(aes(x, predict(H_1, newdata = data.frame(x=x))),
            col = "blue", lwd = 1, lty = 2)+
  geom_point(data = Data_set, aes(x= x,y=y),col = "red")+
  labs(x = "X values", y = "-sin(X)",
       title ="Original Data set and the predicted Models",color = "Legend")+
  theme(plot.title=element_text(hjust=0.2))+
  scale_colour_manual(values = colors)
  
  #################################################################################
## Green is H0, bue is H1 and red is original data
#################################################################################

## Defining Bias
get_bias = function(estimate, truth) {
  mean(estimate) - truth
}

## Defining MSE
get_mse = function(bias, variance) {
  variance+bias^2
}

## Defining Variance
get_var = function(estimate) {
  mean((estimate - mean(estimate)) ^ 2)
}


set.seed(300)

n_rep = 10000 # number of iterations
n_models = 2 # number of models
prediction_x0<-matrix(0,nrow=100,ncol=2)#here 100 rows represent 100 iterations,and 2 columns are for 2 models h1 and h0
x0=1

for (i in 1:100){
  Samples <- Data_set[sample(nrow(Data_set), 3,replace=FALSE),]


H_0 = lm(y ~ 1, data = Samples) ## constant y model
H_1 = lm(y ~x, data = Samples)  ## straight line model

prediction_x0[i,]=c(                                                  ##Here we predicted values for x=1, for every(100) models, predicted from 
  predict(H_0,newdata=data.frame(x=x0)),                              ##and then appended that to matrix which represents in column 1 values of h0 for x=1
  predict(H_1,newdata=data.frame(x=x0))                               ##and in column 2 values of h1 for x=1
)

}


bias = apply(prediction_x0, 2, get_bias,-sin(x0)) ##This calculates bias at point x=1, from predictions_x0 matrix in which we already have predicted values(estimates) at x=1 and calculate true value at x=1 using our true function then calculated bias at that point
variance = apply(prediction_x0, 2, get_var)  ##
mse = get_mse(bias,variance)
bias_of_h0<-bias[1]
bias_of_h1<-bias[2]
variance_of_h0<-variance[1]
variance_of_h1<-variance[2]
bias_of_h0
bias_of_h1
mse
mse_for_h0<-mse[1]
mse_for_h1<-mse[2]
variance_of_h0
variance_of_h1

