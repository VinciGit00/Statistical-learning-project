# LAB - 4 :: Tree based method

library(tree)
library ( ISLR2 )

# Classification tree

View(Carseats)
Carseats$High <- factor( ifelse(Carseats$Sales <= 8 , " No " , " Yes " ))

# train model
tree_model <- tree( High ~ . - Sales , Carseats, split = "gini")

# show result 
summary(tree_model)
plot(tree_model)
text(tree_model,pretty = 0)


# Valuate performance with train and test data sets
set.seed(2)
train <- sample(1:nrow(Carseats),floor(nrow(Carseats)*0.5))

tree_model <- tree( High ~ . - Sales , Carseats, subset = train)
summary(tree_model)

# pred_value <- predict(tree_model, newdata = Carseats[-train,])
pred_value <- predict(tree_model, newdata = Carseats[-train,],type = "class")
table(pred_value,Carseats$High[-train])

# Cross validation 
set.seed(2)
tree_cv <- cv.tree(tree_model, FUN = prune.misclass)
# for regression FUN = prune.tree
tree_cv

plot(tree_cv$size, tree_cv$dev)

best = min(tree_cv$size[tree_cv$dev == min(tree_cv$dev)])
k = min(tree_cv$k[tree_cv$dev == min(tree_cv$dev)]) #alpha in the book

prune <- prune.misclass(tree_model, best = best)
# prune <- prune.misclass(tree_model, k = k)
summary(prune)


plot(prune)
text(prune, pretty = 0)
#plot(prune, type = "uniform")

pred_value <- predict(prune, newdata = Carseats[-train,],type = "class")
table(pred_value,Carseats$High[-train])


# Bagging
library ( randomForest )
set.seed(1)

train <- sample(1:nrow(Boston),floor(nrow(Boston)*0.5))

#mtry = Number of variables randomly sampled as candidates at each split.
#ntree = Number of trees to grow.
bagg_model <- randomForest(medv ~ . ,data = Boston, subset = train,
                           mtry = ncol(Boston)-1, importance = TRUE,replace = TRUE)

bagg_model
plot(bagg_model)

yhat <- predict(bagg_model, newdata = Boston[-train,])
plot(yhat,Boston$medv[-train])
abline(0,1)

mse <- mean((yhat - Boston$medv[-train])^2)

# how to change number of tree? 
# add ntree oprion 
bagg_model <- randomForest(medv ~ . ,data = Boston, subset = train,
                           mtry = ncol(Boston)-1, importance = TRUE, ntree = 100)
bagg_model

importance(bagg_model)

# Random Forest
# default p = sqrt(m) -> see doc 
forest_model <- randomForest(medv ~ . ,data = Boston, subset = train,
                           mtry = floor(sqrt(ncol(Boston)-1)), importance = TRUE, ntree = 100)
forest_model

# if the object has a non-null test component, then the returned object is a 
# matrix where the first column is the out-of-bag estimate of error, 
# and the second column is for the test set.
plot(forest_model,type = 'b',col="green",pch = "+")
par(new=TRUE)
plot(bagg_model,type = 'b',col="red",pch='o')

yhat <- predict(forest_model, newdata = Boston[-train,])
mse <- mean((yhat - Boston$medv[-train])^2)
importance(forest_model)


# Boosting
library ( gbm )
set.seed(1)

ntree = 5000; 
boost_model <- gbm(medv ~ . , data = Boston[train,], 
                        distribution = "gaussian" , n.trees = ntree,
                        interaction.depth = 4, shrinkage = 0.01 , verbose = F)
boost_model

yhat <- predict(boost_model, newdata = Boston[-train,], n.trees = ntree)
mse <- mean((yhat - Boston$medv[-train])^2)



