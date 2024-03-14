# Lab 2 : Shrinkage Methods

rm(list = ls()) # clear all environment variable
graphics.off()  # close all plot

library ( ISLR2 )
library( boot )
set.seed(1)


## Resampling method
## Bootstrap: The bootstrap is a widely applicable and extremely powerful statistical tool
# bootstrap that can be used to quantify the uncertainty associated with a given estimator
# or statistical learning method diﬃcult to obtain

## Example 1: Estimate the variance of estimator
# alpha = (var_y - cov_xy)/(var_x + var_y - 2cov_xy)

# Create sample of X and Y 
X = Portfolio$X;
Y = Portfolio$Y;

# Define bootstrap function
get_alpha <- function(data,index){
  X <- data$X[index];
  Y <- data$Y[index];
  return ((var(Y) - cov(X,Y))/(var(X) + var(Y) - 2*cov(X,Y)));
}

get_alpha(Portfolio,1:100); # estimation of alpha :: variance??? 

# Use boot() function to perform bootstrap simulations
boot(Portfolio,get_alpha,R=10000)


## Example 2: Linear regression :: estimate variance of coeff. [Auto dataset]

model <- lm(mpg ~ horsepower,Auto)
summary(model)

# perform bootstrap simulation
fun_boot <- function(data,index){
  linear <- lm(mpg ~ horsepower,data = data,subset = index);
  return (linear$coefficients)
}

# test function rm
fun_boot(Auto,1:dim(Auto)[1])
fun_boot(Auto, sample(dim(Auto)[1], floor(dim(Auto)[1]*0.7), replace = TRUE));


boot(Auto,fun_boot,R = 1000);

# compare results with summary(model)
# Why are different? -> the standard formulas rely on certain assumptions (they 
# depend on the unknown parameter sigma, there is a non-linear relationship in
# the data and so the residuals from a linear fit will be inflated). 

plot(Auto$horsepower,Auto$mpg, pch='x',col="blue",
     ylab = "MPG", xlab = "Horsepower", main = "Non-Linear relationship", )
grid(10, 10, lwd = 2)


model <- lm(mpg ~ horsepower + I(horsepower^2),Auto)
summary(model)

fun_boot <- function(data,index){
  linear <- lm(mpg ~ horsepower + I(horsepower^2),data = data, subset = index);
  return (linear$coefficients)
}

boot(Auto,fun_boot,R = 1000);

## Example 3: Variance of estimator of the sample mean
X = rnorm(1000,0,1);
mean(X)
hist(X,20)

boot_fun <- function(data,index){
  mean(data[index])
}

# ~ N(u,sigma^2/n)
boot(X,boot_fun,R=1000)

## RIDGE REGRESSION: Hitters dataset 
install.packages("glmnet")
library( glmnet )
# it is best to apply ridge regression after standardizing the predictors so that 
# they are all on the same scale.
# ridge regression can still perform well by trading on a small increase in bias for a
# large decrease in variance.

# Remove nan values
sum(is.na(Hitters));
Hitters <- na.omit(Hitters); # remove row with nan;

# Construct Design Matrices and also automatically transforms any qualitative 
# variables into dummy variables
x <- model.matrix ( Salary ~ . , Hitters ) [ , -1]
y <- Hitters$Salary

# lambda grid
lambda <- 10^seq(-2,3,length = 50);

# tuning elastics net (alpha 0 - RIDGE, alpha 1 - LASSO)
ridge_fit <- glmnet(x,y,alpha = 0,lambda = lambda,standardize=TRUE)
dim(coef(ridge_fit))

# compare l2-norm at different value of lambda
ridge_fit$lambda[5]
sqrt(sum(coef(ridge_fit)[-1,5]^2))

ridge_fit$lambda[25]
sqrt(sum(coef(ridge_fit)[-1,25]^2))

# In general, instead of arbitrarily choosing λ, it would be better to
# use cross-validation to choose the tuning parameter λ.

set.seed(1)
#train <- sample(dim(x)[1],floor(dim(x)[1]*0.75),replace = FALSE);
train <- sample(nrow(x),floor(nrow(x)*0.5),replace = FALSE);

# lambda = seq() parameters is optional 
cv_model <- cv.glmnet(x[train, ],y[train], alpha = 0, nfolds = 10);
plot(cv_model)
opt_lambda <- cv_model$lambda.min; # cv_model$lambda.1se

# predict on test dataset
model <- glmnet(x[train,],y[train],alpha = 0,lambda = opt_lambda,standardize=TRUE)
fitt_value <- predict(model,newx = x[-train,])

test_MSE = mean((y[-train] - fitt_value)^2)


## LASSO
lasso_mod <- glmnet( x[train , ] , y[ train ], alpha = 1,lambda = lambda)
plot(lasso_mod)

set.seed(1)
cv_lasso <- cv.glmnet(x[train,],y[train],alpha=1,nfolds = 10);
plot(cv_lasso)
opt_lambda <- cv_lasso$lambda.min;

# use full datasets
model <- glmnet(x[train,],y[train],alpha = 1,lambda = opt_lambda)
fitt_value <- predict(model,s=opt_lambda, newx=x[-train,])
test_MSE = mean((y[-train] - fitt_value)^2)

model$beta
