library(devtools)
library(BayesRecipe)
library(bayeslm)
prost<-prostate
prost.std <- data.frame(cbind(scale(prost[,1:8]),prost$lpsa))
names(prost.std)[9] <- 'lpsa'
library(caret)
train <-createDataPartition(y=prost.std$lcavol,p=0.70,list=FALSE)
data.train <- prost.std[train,]
data.test <- prost.std[-train,]
y.train   = data.train$lpsa - mean(data.train$lpsa)
y.test <- data.test$lpsa - mean(data.test$lpsa)
x.train = scale(as.matrix(data.train[,1:8], ncol=8))
x.test = scale(as.matrix(data.test[,1:8], ncol=8))

fit_BayesRLasso<-BayesRLasso(x.train, y.train)
y.pred.BayesRLasso<-x.test%*%fit_BayesRLasso$beta

set.seed(1234)
library(glmnet)
lasso.cv=cv.glmnet(x.train,y.train,alpha = 1)  
lambda.cv.lasso=lasso.cv$lambda.min          
lasso.sol=glmnet(x.train,y.train,alpha = 1)    
lasso.coeff=predict(lasso.sol,type="coef",s=lambda.cv.lasso)
y.pred.lasso=x.test%*%lasso.coeff[-1]

mean((y.pred.lasso-y.test)^2)
mean((y.pred.BayesRLasso - y.test)^2)

library(coda)
plot(mcmc(fit_BayesRLasso$beta.post),density=FALSE,smooth=TRUE)
library(psych)
multi.hist(fit_BayesRLasso$beta.post,density=TRUE,main="")