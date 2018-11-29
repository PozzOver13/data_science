########################################################
# PROGRAM: Template Ridge + Lasso Regression
# DATE:    2017-12-18
# NOTE: 
########################################################

rm(list = ls())
for (i in 1:10) gc()

# Ridge regression 
library(glmnet) 
library(dplyr) 
library(ggplot2)

setwd("Documents/POZZ/Data Science/Kaggle/progetti/Statistics for Machine Learning/")

wine_quality = read.csv("winequality-red.csv",header=TRUE,sep = ";",check.names = FALSE) 
names(wine_quality) <- gsub(" ", "_", names(wine_quality)) 

set.seed(1234) 
numrow = nrow(wine_quality) 
trnind = sample(1:numrow,size = as.integer(0.7*numrow)) 
train_data = wine_quality[trnind,]; test_data = wine_quality[-trnind,] 

xvars = c("fixed_acidity","volatile_acidity","citric_acid",
          "residual_sugar","chlorides","free_sulfur_dioxide",            
          "total_sulfur_dioxide","density","pH","sulphates",
          "alcohol") 
yvar = "quality" 

x_train = as.matrix(train_data[,xvars]);y_train = as.double (as.matrix (train_data[,yvar])) 
x_test = as.matrix(test_data[,xvars]) 

print(paste("Ridge Regression")) 
lambdas = c(1e-4,1e-3,1e-2,0.1,0.5,1.0,5.0,10.0, 15) 
initrsq = 0 
for (lmbd in lambdas){ 
  ridge_fit = glmnet(x_train,y_train,alpha = 0,lambda = lmbd) 
  coefficienti_r <- coef(ridge_fit) %>% 
    as.matrix() %>% 
    as.data.frame() %>%
    tibble::rownames_to_column("variabili") %>%
    mutate(lambda = lmbd)
  if (lmbd == lambdas[1]) {
    coef_r_f <- coefficienti_r
  } else {
    coef_r_f <- coef_r_f %>% bind_rows(coefficienti_r)
  }
  pred_y = predict(ridge_fit,x_test) 
  R2 <- 1 - (sum((test_data[,yvar]-pred_y )^2)/sum((test_data[,yvar]-mean(test_data[,yvar]))^2)) 
  
  # if (R2 > initrsq){ 
  #   print(paste("Lambda:",lmbd,"Test Adjusted R-squared :",round(R2,4))) 
  #   initrsq = R2 
  # } 
  print(paste("Lambda:",lmbd,"Test Adjusted R-squared :",round(R2,4))) 
}

# grafici: evoluzione beta
ggplot(coef_r_f, aes(lambda,s0, col = variabili)) +
  geom_line() +
  theme_bw() +
  labs(title = "Ridge - Penalizzazione coefficienti", y = "beta")

# Lasso Regression 
print(paste("Lasso Regression")) 
lambdas = c(1e-4,1e-3,1e-2,0.1,0.5,1.0,5.0,10.0, 15) 
lambdas = seq(0.005, 0.5, 0.005)
initrsq = 0 
for (lmbd in lambdas){ 
  lasso_fit = glmnet(x_train,y_train,alpha = 1,lambda = lmbd) 
  coefficienti_l <- coef(lasso_fit) %>% 
    as.matrix() %>% 
    as.data.frame()%>%
    tibble::rownames_to_column("variabili") %>%
    mutate(lambda = lmbd)
  if (lmbd == lambdas[1]) {
    coef_l_f <- coefficienti_l
  } else {
    coef_l_f <- coef_l_f %>% bind_rows(coefficienti_l)
  }
  pred_y = predict(lasso_fit,x_test) 
  R2 <- 1 - (sum((test_data[,yvar]-pred_y )^2)/sum((test_data[,yvar]-mean(test_data[,yvar]))^2)) 
  
  if (R2 > initrsq){ 
    print(paste("Lambda:",lmbd,"Test Adjusted R-squared :",round(R2,4))) 
    initrsq = R2 
  } 
}

# grafici: evoluzione beta
ggplot(coef_l_f, aes(lambda,s0, col = variabili)) +
  geom_line() +
  theme_bw() +
  labs(title = "Lasso - Penalizzazione coefficienti", y = "beta")


